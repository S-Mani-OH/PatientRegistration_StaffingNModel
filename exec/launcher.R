# this file invokves the launch

# load libraries
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)

conf <- config::get(
  file=system.file(
    "config/config.yaml",
    package="staffingModels")
)

today_date <- Sys.Date()

## inbound EPIC data
# set up parameters
# the incremental path
inbound_path <- conf$path$inbound
# the outbound path for daily output
outbound_path <- conf$path$outbound
# the outbound path for historical daily outputs
old_daily_outbound <- conf$path$old_daily_outbound
# saving the historical data
historical_rds <- conf$path$historical_rds
# configure delimiter
delim <- conf$file$delimiter
# configure hospital names
hospitals <- conf$hospitals$names %>%
  str_split(",") %>%
  unlist()
# configure admission types
admission_types <- conf$hospitals$admission_types %>%
  str_split(",") %>%
  unlist()


### pull in directory info
ordered_files <- enframe(
  x=dir(
    inbound_path,
    pattern=conf$file$extension,
    full.names=TRUE)
  ) %>%
  mutate(date=str_extract(value, "[0-9]+_[0-9]+_[0-9]+"),
         date=as.Date(str_replace_all(date, "_", "-"))) %>%
  arrange(date) %>%
  arrange(value)

ordered_cols <- ordered_files %>%
  pull(value)

ordered_dates <- ordered_files %>%
  pull(date)

ordered_files <- ordered_files %>%
  mutate(group=
    {
      ordered_files$value %>%
        str_split("/") %>%
        map_chr(c(5,1)) %>%
        str_replace("_[0-9]+_[0-9]+_[0-9]+.dat", "")
    }
  )

# import data
read_add_cols <- function(x, cols) {x %>%
    read_delim(delim="|") %>%
    mutate(admit_type=cols)
}

incoming_length <- length(ordered_cols)

admit_cols <- rep(admission_types, each=incoming_length/length(admission_types))

# add admission type
hist_df <- ordered_cols %>%
  sort() %>%
  map2_dfr(.f = read_add_cols,
           .y = admit_cols)

## save compressed version of the historical data
historical_data <- dir(
  path=conf$path$historical_rds,
  pattern=".rds",
  full.names = TRUE
)

# remove old historical data
unlink(historical_data)

# save new historical data
saveRDS(
  hist_df,
  file=sprintf(
    "%s/Epic_ED_LD_DA_%s.rds",
    conf$path$historical_rds,
    today_date-1
    )
  )

convert_date <- function(date) {
  as.POSIXct(
    as.Date(
      as.numeric(date),
      origin="1899-12-30",
      tz="America/New_York"
      )
    )
}

hist_df <- hist_df %>%
  mutate(date=as.Date(substr(Patient_Registration_Time, 1, 10), format="%m/%d/%Y"),
         time=substr(Patient_Registration_Time, 12, 19),
         year=lubridate::year(date),
         month=lubridate::month(date),
         shift=case_when(
           time >= chron::times("07:00:00") & time <= chron::times("14:59:59") ~ 1,
           time >= chron::times("15:00:00") & time <= chron::times("22:59:59") ~ 2,
           time >= chron::times("23:00:00") ~ 3,
           time >= chron::times("00:00:00") & time <= chron::times("06:59:59") ~ 3),
         yearmonth=paste(year, str_pad(month, side="left", pad="0", width=2), sep="-"),
         days=as.numeric(strftime(date, format="%j")),
         hour=as.integer(str_sub(time, 1, 2)) + 1)


cond <- sprintf("'%s'", hospitals)
cnds <- sprintf("str_detect(LOC_NAME, '%s')", hospitals)
vals <- cond
cnds <- map(cnds, rlang::parse_expr)
vals <- map(vals, rlang::parse_expr)
fs <- map2(cnds, vals, function(c, v) rlang::expr(!!c ~ !!v))

# rlang::qq_show(mutate(hospital = case_when(!!!fs)))

hist_df <- hist_df %>%
  mutate(hospital = case_when(!!!fs)) %>%
  drop_na(hospital) %>%
  filter(!is.na(Patient_Registration_Time))

#### now I need to make sure that the diffdate makes sense ....
## filter out any data that is 'today'
daily_hist_df <- hist_df %>%
  filter(date < today_date,
         date >= as.Date("2017-01-01"))

### check dates
daily_hist_df %>%
  group_by(hospital) %>%
  summarize(min_date=min(date),
            max_date=max(date))

res <- hospitals %>%
  map(runLocationModel, daily_hist_df)

df1y_pred <- res %>%
  map_df(1)

obs <- res %>%
  map_df(2)

max_diffdates <- df1y_pred %>%
  group_by(hospital) %>%
  summarize(max_diffdate=max(diffdate))

projection_dates <- daily_hist_df %>%
  select(hospital, date) %>%
  group_by(hospital) %>%
  summarize(start_date=min(date),
            last_date=max(date)) %>%
  mutate(forecast_date=start_date+max_diffdates$max_diffdate)

min_dates <- projection_dates %>%
  pull(start_date)

cnds <- sprintf("hospital == '%s'", hospitals)
vals <- sprintf("date >= '%s'", min_dates)
cnds <- map(cnds, rlang::parse_expr)
vals <- map(vals, rlang::parse_expr)
fs <- map2(cnds, vals, function(c, v) rlang::expr(!!c ~ !!v))

obs <- obs %>%
  complete(hospital, hour, date, fill = list(n = 0))  %>%
  filter(case_when(!!!fs))


## save 'new' historical
patient_prediction <- function(proj_df, level, rate){

  if(level=="hour"){
    df1y_pred %>%
      left_join(proj_df) %>%
      mutate(date=start_date+diffdate,
             pred_mean=ifelse(pred_mean < 0, 0, pred_mean),
             q_lower=ifelse(q_lower < 0, 0, q_lower),
             q_upper=ifelse(q_upper < 0, 0, q_upper),
             staff_needed_mean=round(pred_mean/(rate/8), 4),
             staff_needed_lower=round(q_lower/(rate/8),4),
             staff_needed_upper=round(q_upper/(rate/8),4)) %>%
      select(date,
             hospital,
             hour,
             pred_mean,
             q_lower,
             q_upper,
             staff_needed_mean,
             staff_needed_lower,
             staff_needed_upper) %>%
      rename(encounter_estimate=pred_mean,
             encounter_lower_boundary=q_lower,
             encounter_upper_boundary=q_upper)
  } else if(level=="shift"){
      df1y_pred %>%
        left_join(proj_df) %>%
        mutate(date=start_date+diffdate) %>%
        group_by(date, hospital, shift) %>%
        summarize(sum_mean=sum(pred_mean),
                  sum_sd=sqrt(sum(pred_sd^2)),
                  upper_shift=qnorm(.90, sum_mean, sum_sd),
                  lower_shift=qnorm(.10, sum_mean, sum_sd)) %>%
        mutate(staff_needed_mean=round(sum_mean/rate, 4),
               staff_needed_lower=round(lower_shift/rate,4),
               staff_needed_upper=round(upper_shift/rate,4)) %>%
        rename(shift_estimate=sum_mean,
               shift_upper_boundary=upper_shift,
               shift_lower_boundary=lower_shift) %>%
        select(date,
               hospital,
               shift,
               shift_estimate,
               shift_lower_boundary,
               shift_upper_boundary,
               staff_needed_mean,
               staff_needed_lower,
               staff_needed_upper) %>%
        ungroup()
  }
}

# options(tibble.width=Inf)
res_hour <- patient_prediction(projection_dates, "hour", 32)
res_shift <- patient_prediction(projection_dates, "shift", 32)


# res_hour %>%
#   ungroup() %>%
#   mutate(week=week(date),
#          year=year(date)) %>%
#   group_by(hospital, week, year) %>%
#   summarize(
#     total_lower_bound=sum(encounter_lower_boundary),
#     total=sum(encounter_estimate),
#     total_upper_bound=sum(encounter_upper_boundary)
#     ) %>%
#   ungroup() %>%
#   filter(hospital=="Riverside") %>%
#   print(n=Inf)
#
#
# obs %>%
#   mutate(week=week(date),
#          year=year(date)) %>%
#   group_by(hospital, week, year) %>%
#   summarize(
#     total_n=sum(n)
#   ) %>%
#   filter(hospital=="Riverside") %>%
#   print(n=Inf)

## move out old data
file.copy(dir(outbound_path, full.names = TRUE),
          old_daily_outbound,
          overwrite=TRUE,
          copy.date=TRUE)

unlink(dir(outbound_path, full.names = TRUE))

### save predictions by hour
res_hour %>%
  write_csv(path=sprintf(
    "%s/prediction_%s_to_%s_by_hour.csv",
    outbound_path,
    today_date,
    unique(projection_dates$forecast_date)))

### save predictions by shift
res_shift %>%
  write_csv(path=sprintf(
    "%s/prediction_%s_to_%s_by_shift.csv",
    outbound_path,
    today_date,
    unique(projection_dates$forecast_date)))

forecast_date <- projection_dates %>%
  pull(forecast_date) %>%
  max()

### save observed data
obs %>%
  write_csv(path=sprintf("%s/observed_%s_to_%s_by_hour.csv",
                         outbound_path,
                         "2017-01-01",
                         today_date-1))

### save model output
df1y_pred %>%
  left_join(projection_dates) %>%
  mutate(date=start_date+diffdate,
         pred_mean=pred_mean,
         q_lower=q_lower,
         q_upper=q_upper) %>%
  arrange(hour, shift) %>%
  filter(date > last_date) %>%
  select(hour,
         hospital,
         date,
         pred_mean,
         pred_sd,
         probs,
         q_lower,
         q_upper,
         shift) %>%
  arrange(hospital, date)  %>%
  write_csv(path=sprintf("%s/model_output_%s_to_%s.csv",
                         outbound_path,
                         today_date,
                         unique(projection_dates$forecast_date)))



