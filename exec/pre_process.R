hist_df <- readRDS("~/collab/ohiohealth/staffingmodels/inst/extdata/hist_df.rds")

hist_df %>%
  count(LOC_NAME, sort=TRUE)


today_date <- Sys.Date()

exp_hos <- hist_df %>% 
  mutate(
    hospital=case_when(
      str_detect(LOC_NAME, "Riverside") ~ "Riverside",
      str_detect(LOC_NAME, "Pickerington") ~ "Pickerington",
      str_detect(LOC_NAME, "Westerville") ~ "Westerville",
      str_detect(LOC_NAME, "Lewis Center") ~ "Lewis Center",
      str_detect(LOC_NAME, "Grady") ~ "Grady",
      str_detect(LOC_NAME, "Grove City") ~ "Grove City"
      ),
    weekday=lubridate::wday(date, label=TRUE)
  ) %>%
  filter(year > 2016,
         PATIENT_CLASS != "Surgery Admit",
         date < today_date-1) %>%
  drop_na(hospital) %>%
  drop_na(date) 


#### exploring the daily trends at a hopsital
exp_hos %>%
  count(hospital, weekday, year, month) %>% 
  group_by(hospital, weekday, year) %>%
  summarize(mean_n=mean(n)) %>% 
  ungroup() %>%
  filter(hospital=="Riverside") %>%
  ggplot(aes(weekday, mean_n, color=factor(year))) +
  geom_point() + 
  facet_wrap(~year, scale="free_x")


### 
library(mgcv)
exp_hos <- exp_hos %>%
  group_by(hospital) %>%
  mutate(start_date=min(date),
         diffdate=as.numeric(date-start_date)) %>%
  ungroup() 

# impute the 0s


exp_full <- exp_hos %>%
  count(date, year, month, days, hospital, hour, weekday, start_date, diffdate)


hos_dates <- exp_full %>%
  group_by(hospital) %>%
  summarize(min_range=range(diffdate)[1],
            max_range=range(diffdate)[2],
            min_date=as.character(range(date)[1]),
            max_date=as.character(range(date)[2])) %>%
  ungroup()



hospital_expansion <- function(hospital_name, start_date, end_date){
  expand_grid(
    hospital=hospital_name,
    date=seq(
      as.Date(start_date, origin="1970-01-01"),
      as.Date(end_date, origin="1970-01-01"),
      "1 day"
    ),
    hour=1:24
  )  
}

hospital_expansion(
  "Grady",
  "2017-01-01",
  "2019-10-30"
)


hospital_names <- unique(exp_full$hospital)

hos_list <- vector('list', length(hospital_names))

for(i in seq_along(hos_list)){
  hos_list[[i]] <- hospital_expansion(
    hos_dates$hospital[i],
    hos_dates$min_date[i],
    hos_dates$max_date[i]
  )
}

hos_zeros <- hos_list %>%
  bind_rows()


exp_full <- exp_full %>%
  full_join(hos_zeros) %>%
  mutate(n=ifelse(is.na(n), 0, n),
        year=year(date),
        month=month(date),
        days=lubridate::day(date),
        weekday=lubridate::wday(date, label=TRUE)) %>%
  group_by(hospital) %>%
  mutate(start_date=min(date),
         diffdate=as.numeric(date-start_date)) %>%
  ungroup() 

train <- exp_full %>%
  filter(year != 2019)


## partition the data



?xgboost::xgb.train

### 
oh_earning_code <- readr::read_csv("~/collab/ohiohealth/predictive_patient_access_staffing/data/OH_EarningCodes.csv")



