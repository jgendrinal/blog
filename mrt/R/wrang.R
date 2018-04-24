#Load packages ----
suppressPackageStartupMessages({
  library(tidyverse)})
#Setup ----
#Namespace
files <- c("data/raw/2012_mrt_hourly_daily_ridership 2.csv", 
           "data/raw/2013_mrt_hourly_daily_ridership.csv", 
           "data/raw/2014_mrt_hourly_daily_ridership.csv")
years <- c(2012L, 2013L, 2014L)
#Import function
import_mrtfiles <- function(file){
  read_csv(file) %>% 
    select_if(~sum(!is.na(.)) > 0) %>% 
    fill(day, .direction = "down") %>% 
    gather(key = "station_mode", value = "traff", 
           -month, -day, -time) %>% 
    mutate( 
      #Change time range to integer value
      hour = recode(time, 
                    "00:00 - 00:59" = 1L,
                    "01:00 - 01:59" = 2L, 
                    "02:00 - 02:59" = 3L, 
                    "03:00 - 03:59" = 4L, 
                    "04:00 - 04:59" = 5L, 
                    "05:00 - 05:59" = 6L, 
                    "06:00 - 06:59" = 7L, 
                    "07:00 - 07:59" = 8L, 
                    "08:00 - 08:59" = 9L, 
                    "09:00 - 09:59" = 10L, 
                    "10:00 - 10:59" = 11L, 
                    "11:00 - 11:59" = 12L, 
                    "12:00 - 12:59" = 13L, 
                    "13:00 - 13:59" = 14L, 
                    "14:00 - 14:59" = 15L, 
                    "15:00 - 15:59" = 16L, 
                    "16:00 - 16:59" = 17L, 
                    "17:00 - 17:59" = 18L, 
                    "18:00 - 18:59" = 19L, 
                    "19:00 - 19:59" = 20L, 
                    "20:00 - 20:59" = 21L, 
                    "21:00 - 21:59" = 22L, 
                    "22:00 - 22:59" = 23L, 
                    "23:00 - 23:59" = 24L), 
      #Change month string to integer value
      month = recode(month, 
                     "January" = 1L, 
                     "Feb" = 2L, 
                     "Febuary" = 2L, 
                     "February" = 2L, 
                     "March" = 3L, 
                     "April" = 4L, 
                     "May" = 5L, 
                     "June" = 6L, 
                     "July" = 7L, 
                     "August" = 8L, 
                     "September" = 9L, 
                     "October" = 10L, 
                     "November" = 11L, 
                     "December" = 12L)) %>% 
    select(-time) %>% 
    #split station from flows
    separate(station_mode, 
             into = c("station", "mode"), 
             sep = "_", 
             extra = "merge") %>% 
    #entry, exit
    mutate(mode = if_else(str_detect(mode, "entry"), "entry", "exit"), 
         #Change to integer values
         traff = if_else(traff == "-", 0L, as.integer(traff)))}
#Load, Wrangle dataset ----
data_frame(file = files, year = years) %>% 
  mutate(data = map(file, import_mrtfiles)) %>% 
  unnest() %>% 
  select(-file) %>% 
  replace_na(list(traff = 0L)) %>% 
  spread(mode, traff) %>% 
  mutate(net = entry - exit) %>% 
  write_csv("data/clean/mrttraff.csv")
rm(files, years, import_mrtfiles)