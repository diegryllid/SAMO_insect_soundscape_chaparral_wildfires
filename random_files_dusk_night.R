#basic stats
library(readr)
library(scales)
library(magrittr)
library(dplyr)
library(suncalc)
library(lubridate)


tz <- "America/Los_Angeles"

# categorize night/day/morning_twilight/evening_twilight

#read and categorize by night/day
full_df <- read_csv("D:/full_data/full_data.csv") %>% 
  mutate(recorded_date_time = 
           as.POSIXct(recorded_date_time, tz = tz),
         years_last_fire = 2022 - last_fire,
         date = date(recorded_date_time)) %>% 
  mutate(locality =  
           case_when(
             recorder_id %in% c("36","16","21","9") ~ "eleanor", 
             recorder_id %in% c("26","4","42","8") ~ "eleanor",
             recorder_id %in% c("2","12","30","23") ~ "eleanor",
             recorder_id %in% c("22","5","19","14") ~ "eleanor",
             recorder_id %in% c("25","17","43","34") ~ "topanga", 
             recorder_id %in% c("18","15","6","39") ~ "topanga",
             recorder_id %in% c("24","27","20","41") ~ "topanga",
             recorder_id %in% c("40","1","31","33") ~ "topanga",
             recorder_id %in% c("38","29","7","3") ~ "rancho", 
             recorder_id %in% c("45","10","35","11") ~ "rancho",
             recorder_id %in% c("32","37","28","44") ~ "rancho",
             TRUE ~ NA))

full_df<-
  bind_cols(full_df,
            getSunlightTimes(data = full_df %>% 
                               rename(lat = Latitude, lon = Longitude) %>% 
                               select(date, lat, lon),
                             keep = c("sunrise","sunset","sunriseEnd","sunsetStart", "nauticalDawn","nauticalDusk","nightEnd","night"),
                             tz = tz)) %>% 
  mutate(day_night =
           case_when(
             recorded_date_time %within%
               interval(if_else(recorded_date_time > sunset,
                                night,
                                night - days(1)),
                        if_else(recorded_date_time > sunset,
                                nightEnd + days(1),
                                nightEnd),
                        tz = tz) ~ "night",
             recorded_date_time %within%
               interval(sunriseEnd, sunsetStart,
                        tz = tz) ~ "day",
             recorded_date_time %within%
               interval(nauticalDawn, sunrise,
                        tz = tz) ~ "dawn",
             recorded_date_time %within%
               interval(sunset, nauticalDusk,
                        tz = tz) ~ "dusk",
             TRUE ~ "astro_twilight")) 

#subset to night
files_df <-
full_df %>% 
  select(season, day_night, recorder_id, file_name, recorded_date_time) %>% 
  filter(season == "summer", day_night == "night") %>% 
  mutate(day = day(recorded_date_time), week = week(recorded_date_time))

#inputs
recording_directory <- "D:/deployment1/"
destination_directory <- "D:/summer_sample"

#code

recorder_numbers <- unique(files_df$recorder_id)

folder_paths <- paste(recording_directory,recorder_numbers,sep="")

#randomize sample
sample_df <-
files_df %>% 
  group_by(recorder_id, week) %>% 
sample_n(2) %>% 
  mutate(source = paste(recording_directory,
                        as.character(recorder_id),
                        file_name, sep="/"),
         destination = paste(destination_directory,
                             as.character(recorder_id),
                             file_name, sep="/"))

#create destination folders

dest_folders<-
paste(destination_directory,
      as.character(sample_df$recorder_id),
      sep="/")

dir.create(destination_directory)

for (folder in dest_folders) {
  dir.create(folder)
}

#copy files
file.copy(from = sample_df$source, to = sample_df$destination, recursive = T)
