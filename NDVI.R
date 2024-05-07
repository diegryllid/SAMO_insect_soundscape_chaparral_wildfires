#basic stats
library(readr)
library(scales)
library(magrittr)
library(dplyr)
library(suncalc)
library(lubridate)
library(sf)
library(tmap)
library(tmaptools)

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

#Dominant vegetation categories
full_df <-
  full_df %>% 
  mutate(vegetation =
           case_when(
             recorder_id %in% c("37","45","35") ~ "Ceanothus_spinosus", 
             recorder_id %in% c("32","28") ~ "Artemisia_californica",
             recorder_id %in% c("44","30","42","8","26") ~ "Malosma_laurina",
             recorder_id %in% c("38","22","5","19","14","4","9","21","16","1") ~ "Adenostoma_fasciculatum",
             recorder_id %in% c("29","7","3","11","33","24","27","41") ~ "Ceanothus_megacarpus", 
             recorder_id %in% c("43","25","17","34") ~
               "Rhus_ovata_&_Rhamnus_ilicifolia",
             recorder_id %in% c("10", "39") ~ "Firebreak_early_seral_undifferentiated",
             recorder_id %in% c("2","12","31","40","20","18","15","6") ~ "Erigonium_fasciculatum",
             recorder_id %in% c("23") ~ "Salvia_melifera",
             recorder_id %in% c("36") ~ "Ceanothus_spp_&_Cercocarpus_betuloides",
             TRUE ~ NA))

#Importante note: points 25, 27, 43 and 34 used to be dominated by Ceanothus before the recent Palisades wildfire. At the time of deployment it was dominated by Rhamnus ilicifolia and Rhus ovata (Ided by Mark Mendelsohn and Shane Jordan)

#convert locations into points
topanga_points <-
  full_df %>% 
  select(locality, recorder_id, Latitude, Longitude) %>% 
  unique() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326) %>% 
filter(locality == "topanga")

rancho_eleanor_points <-
  full_df %>% 
  select(locality, recorder_id, Latitude, Longitude) %>% 
  unique() %>% 
  st_as_sf( coords = c("Longitude", "Latitude"),
            crs = 4326) %>% 
  filter(locality != "topanga")

#Sentinel NDVI

tmap_mode('view')

B02<-
terra::rast("D:/ndvi/topanga/2022-06-02-00_00_2022-06-29-23_59_Sentinel-2_L1C_B02_(Raw).tiff")
B03<-
  terra::rast("D:/ndvi/topanga/2022-06-02-00_00_2022-06-29-23_59_Sentinel-2_L1C_B03_(Raw).tiff")
B04<-
  terra::rast("D:/ndvi/topanga/2022-06-02-00_00_2022-06-29-23_59_Sentinel-2_L1C_B04_(Raw).tiff")
B08<-
  terra::rast("D:/ndvi/topanga/2022-06-02-00_00_2022-06-29-23_59_Sentinel-2_L1C_B08_(Raw).tiff")

topanga_raster<-
terra::rast(list(B08, B04))

names(topanga_raster) <- c("B08","B04")

B02<-
  terra::rast("D:/ndvi/rancho_eleanor/2022-06-02-00_00_2022-06-29-23_59_Sentinel-2_L1C_B02_(Raw).tiff")
B03<-
  terra::rast("D:/ndvi/rancho_eleanor/2022-06-02-00_00_2022-06-29-23_59_Sentinel-2_L1C_B03_(Raw).tiff")
B04<-
  terra::rast("D:/ndvi/rancho_eleanor/2022-06-02-00_00_2022-06-29-23_59_Sentinel-2_L1C_B04_(Raw).tiff")
B08<-
  terra::rast("D:/ndvi/rancho_eleanor/2022-06-02-00_00_2022-06-29-23_59_Sentinel-2_L1C_B08_(Raw).tiff")

rancho_eleanor_raster<-
  terra::rast(list(B08, B04))

names(rancho_eleanor_raster) <- c("B08","B04")
rm(B02,B03,B04,B08)

# Created a VI function (vegetation index)
ndvi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

# For Sentinel 2, the relevant bands to use are:
# NIR = 8, red = 4

topanga_ndvi<-
ndvi(topanga_raster, 1, 2)

rancho_eleanor_ndvi<-
  ndvi(rancho_eleanor_raster, 1, 2)

full_variables_df<-
full_df %>% 
  left_join(
topanga_points %>%
  as.data.frame() %>% 
  mutate(ndvi =
topanga_ndvi %>% 
terra::extract(
  topanga_points %>% 
    st_transform(crs = st_crs(topanga_ndvi))) %>% 
  pull()) %>% 
  select(recorder_id,ndvi) %>% 
  bind_rows(
    rancho_eleanor_points %>%
      as.data.frame() %>% 
      mutate(ndvi =
               rancho_eleanor_ndvi %>% 
               terra::extract(
                 rancho_eleanor_points %>% 
                   st_transform(crs = st_crs(rancho_eleanor_ndvi))) %>% 
               pull()) %>% 
      select(recorder_id,ndvi)))

write_csv(full_variables_df, "D:/full_data/full_variables_df.csv")

#visualize
topanga_ndvi %>% 
tm_shape(name = 'NDVI') +
  tm_raster(
    title = 'NDVI',
    palette = hcl.colors(n = 7, "greens", rev = T),
    alpha = .7) +
  topanga_points %>% 
  tm_shape(name = 'recorders') +
  tm_dots(
    title = "Recorders")



rancho_eleanor_ndvi %>% 
  tm_shape(name = 'NDVI') +
  tm_raster(
    title = 'NDVI',
    palette = hcl.colors(n = 7, "greens", rev = T),
    alpha = .7) +
  rancho_eleanor_points %>% 
  tm_shape(name = 'recorders') +
  tm_dots(
    title = "Recorders")
