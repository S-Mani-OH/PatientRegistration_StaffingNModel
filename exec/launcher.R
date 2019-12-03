# this file invokves the launch

# load libraries
library(readr)
library(tibble)
library(config)
library(lubridate)
library(chron)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)

# set date
today_date <- Sys.Date()

# inbound EPIC data
inbound_path <- "/storage/transfers/Inbound/pas_staffing_p2_np"


# inbound_path <- "/storage/transfers/Inbound/"

# historical data RDS file that will be loaded and saved
historical_data <- sprintf(
  "~/historical_data/historical_thru_%s.rds",
  today_date
)


historical_data <- dir(
  path=inbound_path,
  pattern=".dat",
  full.names = TRUE
)

# grab the historical data file names and put into tibble
ordered_files <- enframe(x=dir(inbound_path,'*.dat', full.names=TRUE)) %>%
  mutate(date=str_extract(value, "[0-9]+_[0-9]+_[0-9]+"),
         date=as.Date(str_replace_all(date, "_", "-"))) %>%
  arrange(date) %>%
  #slice(-1:-3) %>%
  arrange(value)


ordered_cols <- ordered_files %>%
  filter(!date %in% seq(today_date, Sys.Date(), "1 day")) %>%
  pull(value)

ordered_dates <- ordered_files %>%
  filter(!date %in% seq(today_date, Sys.Date(), "1 day")) %>%
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

admit_cols <- c(rep("DA", incoming_length/3),
                rep("LD", incoming_length/3),
                rep("ED", incoming_length/3))

# hist_df <- mapply(read_add_cols, ordered_files$value, admit_cols, ordered_files$date, SIMPLIFY = FALSE)
# hist_df <- do.call("rbind", hist_df)


# add admission type
hist_df <- ordered_cols %>%
  sort() %>%
  map2_dfr(.f=read_add_cols,
           .y = c(rep("DA", incoming_length/3),
                  rep("LD", incoming_length/3),
                  rep("ED", incoming_length/3)))



convert_date <- function(date) {
  as.POSIXct(
    as.Date(
      as.numeric(date),
      origin="1899-12-30",
      tz="America/New_York"
      )
    )
}
# hospitals to include

hospitals <- hist_df %>%
  pull(hospital) %>%
  unique() %>%
  sort()


## remove patient classes
### ask about psychiatric inpatient?
# maybe will be diff when i parse to the hospitals we need...
hist_df <- hist_df %>%
  mutate(date=as.Date(substr(Patient_Registration_Time, 1, 10), format="%m/%d/%Y"),
         time=substr(Patient_Registration_Time, 12, 19),
         year=year(date),
         month=month(date),
         shift=case_when(
           time >= times("07:00:00") & time <= times("14:59:59") ~ 1,
           time >= times("15:00:00") & time <= times("22:59:59") ~ 2,
           time >= times("23:00:00") ~ 3,
           time >= times("00:00:00") & time <= times("06:59:59") ~ 3),
         yearmonth=paste(year, str_pad(month, side="left", pad="0", width=2), sep="-"),
         days=as.numeric(strftime(date, format="%j")),
         hour=as.integer(str_sub(time, 1, 2)) + 1)


cond <- sprintf("'%s'", hospitals)
cnds <- sprintf("str_detect(LOC_NAME, '%s')", hospitals)
vals <- cond
cnds <- map(cnds, rlang::parse_expr)
vals <- map(vals, rlang::parse_expr)
fs <- map2(cnds, vals, function(c, v) rlang::expr(!!c ~ !!v))
rlang::qq_show(mutate(hospital = case_when(!!!fs)))
hist_df <- hist_df %>%
  mutate(hospital = case_when(!!!fs)) %>%
  drop_na(hospital) %>%
  filter(!is.na(Patient_Registration_Time))

### check dates
hist_df %>%
  group_by(hospital) %>%
  summarize(min_date=min(date),
            max_date=max(date))
# hist_df2 %>%
#   mutate(hospital = case_when(!!!fs)) %>%
#   drop_na(hospital) %>%
#   group_by(LOC_NAME) %>%
#   filter(str_detect(LOC_NAME, "Lewis|Pickerington|Westerville")) %>%
#   summarize(min_date=min(date, na.rm=TRUE),
#             max_date=max(date, na.rm=TRUE),
#             row_count=n())

####
## modeling takes about 1h40m with 9 hospitals -- need to delay pick up
## dates look off -- possibly missing some of the old data.
##





#### missing registrations times
hist_df %>%
  filter(is.na(Patient_Registration_Time)) %>%
  count(hospital)
#### NA registration times







### now I need to make sure that the diffdate makes sense ....


# filter out any data that is 'today'
daily_hist_df <- hist_df %>%
  filter(date < today_date,
         date >= as.Date("2017-01-01"))


#daily_hist_df <-
daily_hist_df <- daily_hist_df %>%
  group_by(hospital) %>%
  mutate(start_date=min(date),
         diffdate=as.numeric(date-start_date)) %>%
  ungroup()

pilot_ct <- daily_hist_df %>%
  count(diffdate, hospital, hour) %>%
  mutate(getx=diffdate/365,
         hospital=as.factor(hospital)) %>%
  na.omit()

system.time({
  mod_hr <- gam(
    list(
      n ~
        diffdate +
        hospital +
        diffdate:hospital +
        s(getx, by=hospital) +
        s(hour, by=hospital),
      ~s(hour, by=hospital)
      ),
    family=gaulss,
    data=pilot_ct)
})




expected_hr <- predict(mod_hr)

exp_hr_df <- pilot_ct %>%
  mutate(pred_mean=expected_hr[,1],
         pred_sd=.01 + exp(expected_hr[,2])) %>%
  mutate(q_lower=qnorm(0.10, pred_mean, pred_sd),
         q_lower = if_else(q_lower < 0, 0, q_lower),
         q_upper=qnorm(0.90, pred_mean, pred_sd))

### PREPARE FOR LOGISTIC MODEL
date_hr <- pilot_ct %>%
  select(diffdate, hospital) %>%
  unite(diffhos, c("diffdate", "hospital")) %>%
  distinct()


log_df <- expand.grid(hour=1:24,
                      #days=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                      diffhos=date_hr$diffhos) %>%
  separate(diffhos, c("diffdate", "hospital"), extra = "merge") %>%
  mutate(diffdate=as.integer(diffdate))


log_df_poss <- log_df %>%
  left_join(pilot_ct %>% select(diffdate, hospital, hour, n)) %>%
  mutate(n=ifelse(is.na(n), 0, 1),
         n=ifelse(n >= 1, 1, 0)) %>%
  as_tibble()


system.time({
mod_log_hos <-
  gam(n ~ hospital +
        s(hour, by = factor(hospital)),
      family=binomial,
      data=log_df_poss)
  })




probs_log <- predict(mod_log_hos, type="response")

probs_hr <- cbind(log_df, probs_log)

### joining the results back together
model_shift_ct <- exp_hr_df %>%
  left_join(probs_hr) %>%
  mutate(mixed_n=probs_log*pred_mean,
         upper_mixed=probs_log*q_upper,
         lower_mixed=probs_log*q_lower) %>%
  filter(!is.na(mixed_n)) %>%
  mutate(shift=case_when(hour>=7 & hour <15 ~ 1,
                         hour>=15 & hour <23 ~ 2,
                         hour>=23 ~ 3,
                         hour>=0 & hour <7 ~ 3)) %>%
  group_by(diffdate, hospital, shift) %>%
  summarize(sum_mean=sum(pred_mean),
            sum_sd=sqrt(sum(pred_sd^2)),
            upper_shift=qnorm(.90, sum_mean, sum_sd),
            lower_shift=qnorm(.10, sum_mean, sum_sd)) %>%
  gather(key, value, sum_mean, sum_sd, upper_shift, lower_shift)

pilot_shift_ct <- daily_hist_df %>%
  #filter(year %in% c(2017, 2018)) %>%
  mutate(shift=case_when(hour>=7 & hour <15 ~ 1,
                         hour>=15 & hour <23 ~ 2,
                         hour>=23 ~ 3,
                         hour>=0 & hour <7 ~ 3)) %>%
  count(diffdate, hospital, shift)

models_joined <- model_shift_ct %>%
  rename(n=value) %>%
  bind_rows(list(., pilot_shift_ct %>% mutate(key="observed"))) %>%
  ungroup()


df_1y <-
expand.grid(hour=1:24,
            hospital=hospitals)# ,
           # diffdate=max_diff_dates$diffdate:(max_diff_dates$diffdate+1)+365)

projected_dates <- daily_hist_df %>%
  select(hospital, diffdate) %>%
  group_by(hospital) %>%
  summarize(diffdate=max(diffdate)) %>%
  left_join(df_1y) %>%
  group_split(hospital) %>%
  map(~expand(., diffdate=seq(unique(.$diffdate)+1, unique(.$diffdate)+365))) %>%
  map2(.x=., .y=sort(unique(df_1y$hospital)), .f=function(x, y) x %>% mutate(hospital=y)) %>%
  bind_rows() #%>%

df_1y <- df_1y %>%
  left_join(projected_dates) %>%
  mutate(getx=diffdate/365) %>%
  as_tibble()

pred_hos <- predict(object=mod_hr,
                    newdata=df_1y)

probs_hos <- predict(mod_log_hos,
                     df_1y,
                     type="response")


df1y_pred <- df_1y %>%
  mutate(pred_mean=pred_hos[,1],
         pred_sd=0.01+exp(pred_hos[,2]),
         probs=probs_hos) %>%
  mutate(q_lower=qnorm(0.10, pred_mean, pred_sd),
         q_lower = if_else(q_lower < 0, 0, q_lower),
         q_upper=qnorm(0.90, pred_mean, pred_sd)) %>%
  mutate(shift=case_when(hour>=7 & hour <15 ~ 1,
                         hour>=15 & hour <23 ~ 2,
                         hour>=23 ~ 3,
                         hour>=0 & hour <7 ~ 3))

df1y_pred <- df1y_pred %>%
  as_tibble()

## save 'new' historical
patient_prediction <- function(proj_df, level, rate){

  if(level=="hour"){

    df1y_pred %>%
      left_join(proj_df) %>%
      mutate(date=start_date+diffdate,
             pred_mean=pred_mean,
             q_lower=q_lower,
             q_upper=q_upper,
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
      select(date, hospital, shift, shift_estimate, shift_lower_boundary, shift_upper_boundary, staff_needed_mean, staff_needed_lower, staff_needed_upper) %>%
      ungroup()
  }
}
#real_today_date <- max(start_date+daily_hist_df$diffdate, na.rm=TRUE)



max_diffdates <- df_1y %>%
  group_by(hospital) %>%
  summarize(max_diffdate=max(diffdate))

projection_dates <- daily_hist_df %>%
  select(hospital, date) %>%
  group_by(hospital) %>%
  summarize(start_date=min(date),
            last_date=max(date)) %>%
  mutate(forecast_date=start_date+max_diffdates$max_diffdate)


# options(tibble.width=Inf)
res_hour <- patient_prediction(projection_dates, "hour", 32)
res_shift <- patient_prediction(projection_dates, "shift", 32)

res_hour %>%
  write_csv(path=sprintf("/storage/transfers/Outbound/nonprod_daily_output/prediction_%s_to_%s_by_hour.csv", today_date, forecast_date))

res_shift %>%
  write_csv(path=sprintf("/storage/transfers/Outbound/nonprod_daily_output/prediction_%s_to_%s_by_shift.csv", today_date, forecast_date))

forecast_date <- projection_dates %>%
  pull(forecast_date) %>%
  max()


obs <- daily_hist_df %>%
  select(hospital, date) %>%
  group_by(hospital) %>%
  summarize(start_date=min(date)) %>%
  left_join(pilot_ct) %>%
  mutate(date=start_date+diffdate) %>%
  select(-getx, -diffdate,
         -start_date)






#### need to fill in 0s for the observations


hospitals <- hospitals %>%
min_dates <- projection_dates %>%
  pull(last_date)


obs <-
  obs %>%
  complete(hospital, hour, obs_date, fill = list(n = 0))  %>%
  filter(
    case_when(
      hospital=="Lewis Center" ~ obs_date >= "2017-02-15",
      hospital=="Dublin" ~ obs_date >= "2017-01-01",
      hospital=="Grove City" ~ obs_date >= "2018-10-03",
      hospital=="Grady" ~ obs_date >= "2017-01-01",
      hospital=="Pickerington" ~ obs_date >= "2017-01-01",
      hospital=="Riverside" ~ obs_date >= "2017-01-01"
    )
  )


cnds <- sprintf("str_detect(LOC_NAME, '%s')", hospitals)
vals <- cond
cnds <- map(cnds, rlang::parse_expr)
vals <- map(vals, rlang::parse_expr)
fs <- map2(cnds, vals, function(c, v) rlang::expr(!!c ~ !!v))
rlang::qq_show(mutate(hospital = case_when(!!!fs)))


#start_date <- max(df_1y$diffdate na.rm=TRUE)+1

#forecast_date <- start_date+max(df_1y$diffdate, na.rm=TRUE)

forecast_seq <- seq(today_date, max(unique(projection_dates$forecast_date)), "1 day")



forecast_seq %>%
  map_dfr(.f=patient_prediction, level="hour", rate=25) %>%
  as_tibble() %>%
  arrange(date) #%>%
  #write_csv(path=sprintf("/storage/transfers/Outbound/daily_output/prediction_%s_to_%s_by_hour.csv", today_date, forecast_date))

forecast_seq %>%
  map_dfr(.f=patient_prediction, level="shift", rate=25) %>%
  as_tibble() %>%
  arrange(date) %>%
  write_csv(path=sprintf("/storage/transfers/Outbound/daily_output/prediction_%s_to_%s_by_shift.csv", today_date, forecast_date))

possible_hosp_date <-
  expand.grid(
    hospital= c("Dublin",
                "Grady",
                "Grove City",
                "Lewis Center",
                "Pickerington",
                "Riverside"),
    hour=1:24,
    date=seq(start_date, yesterday_date, "1 day")
)

### get proper observation data





obs %>%
  count(hospital, obs_date) %>%
  group_by(hospital) %>%
  summarize(min_date=min(obs_date),
         max_date=max(obs_date))


obs %>%
  write_csv(path=sprintf("/storage/transfers/Outbound/nonprod_daily_output/observed_%s_to_%s_by_hour.csv", today_date, yesterday_date))

projection_dates




obs <- pilot_ct %>%
  mutate(date=start_date+diffdate) %>%
  select(-diffdate, -getx) %>%
  right_join(possible_hosp_date) %>%
  mutate(n=ifelse(is.na(n), 0, n)) %>%
  # group_by(date) %>%
  # summarize(count=sum(n)) %>% print(n=Inf)
  arrange(date)  #%>%
  write_csv(path=sprintf("/storage/transfers/Outbound/daily_output/observed_%s_to_%s_by_hour.csv", start_date, yesterday_date))


df1y_pred %>%
  mutate(date=start_date+diffdate) %>%
  arrange(hour, shift) %>%
  as_tibble() %>%
  select(-diffdate, -getx) %>%
  filter(date %in% seq(today_date, forecast_date, "1 day")) %>%
  arrange(date) %>%
  write_csv(path=sprintf("/storage/transfers/Outbound/daily_output/model_output_%s_to_%s.csv", today_date, forecast_date))


df1y_pred %>%
  left_join(proj_df) %>%
  mutate(date=start_date+diffdate,
         pred_mean=pred_mean,
         q_lower=q_lower,
         q_upper=q_upper) %>%
  arrange(hour, shift) %>%
  select(hour, hospital, date, pred_mean, pred_sd, probs, q_lower, q_upper, shift) %>%
  arrange(date) %>%
  write_csv(path=sprintf("/storage/transfers/Outbound/nonprod_daily_output/model_output_%s_to_%s.csv", today_date, forecast_date))

## lets make some plots
library(ggplot2)
obs %>%
  mutate(week=week(obs_date),
         year=year(obs_date),
         day=wday(obs_date, label=TRUE)) %>%
  filter(week==46,
         year==2019) #%>%
  ggplot(aes(hour, n, color=hospital)) +
  geom_point() +
  geom_line() +
  facet_grid(hospital~day, scales="free_y")






