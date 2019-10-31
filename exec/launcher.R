# this file invokves the launch

# load libraries
library(readr)
library(tibble)
library(config)
library(lubridate)
library(chron)
library(stringr)

# set date
today_date <- Sys.Date()

# inbound EPIC data
inbound_path <- "/storage/transfers/Inbound"

# historical data RDS file that will be loaded and saved
historical_data <- sprintf(
  "~/historical_data/historical_thru_%s.rds",
  today_date
)

# grab the historical data file names and put into tibble
ordered_files <- enframe(x=dir(inbound_path,'*.dat', full.names=TRUE)) %>%
  mutate(date=str_extract(value, "[0-9]+_[0-9]+_[0-9]+"),
         date=as.Date(str_replace_all(date, "_", "-"))) %>%
  arrange(date) %>%
  slice(-1:-3)

ordered_files <- ordered_files %>%
  filter(!date %in% seq(today_date, Sys.Date(), "1 day")) %>%
  pull(value)

# import data
read_add_cols <- function(x, cols) {x %>%
    read_delim(delim="|") %>%
    mutate(admit_type=cols)
}

incoming_length <- length(ordered_files)

# add admission type
hist_df <- ordered_files %>%
  map2_dfr(read_add_cols, .y = c(rep("DA", incoming_length/3),
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

hospitals <- c("Pickerington",
               "Lewis Center",
               "Riverside",
               "Dublin",
               "Grady",
               "Grove City")

hist_df <- hist_df %>%
  filter(PATIENT_CLASS != "Surgery Admit") %>%
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
         hospital=case_when(
           str_detect(LOC_NAME, "Doctors") ~ "Doctors",
           str_detect(LOC_NAME, "Westerville") ~ "Westerville",
           str_detect(LOC_NAME, "Grant") ~ "Grant")
         ,
         hour=as.integer(str_sub(time, 1, 2)) + 1)

# parse down to just hospitals of interest





