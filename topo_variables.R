library(gpx)
library(elevatr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(tmap)
library(sf)
library(terra)
library(smoothr)
library(chirps)
library(rgrass)

# Elevation, aspect and slope rasters -------------------------------------

tmap_mode("view") #sets viewing window for double-checking
tmap_options(check.and.fix = TRUE)


#loads GPX file waypoints as a dataframe
recorders_coord <-
  read_gpx("D:/recorders_summer.gpx")$waypoints %>% 
  select("Latitude", "Longitude", "Name") %>% 
  add_row(Latitude = 34.10683, Longitude = -118.9084, Name = "11") %>% 
  add_row(Latitude = 34.11359, Longitude = -118.9194, Name = "38") #add missing locations manually

prj <- "EPSG:4326" #CRS code for raster download

#converts dataframe into SF point object
recorders_points <-
  recorders_coord %>% 
  st_as_sf(coords = c("Longitude","Latitude")) %>% 
  st_set_crs(4326) %>%  #EPSG code for WGS84 datum
  st_transform(.,crs=4326)

#retrieves an elevation raster from AmazonWebServices
elevation <-
  recorders_points %>% 
  get_elev_raster(z = 14, #zoom level, 14 is max resolution
                  prj = prj,
                  src = "aws")

#calculates slope raster
slope <-
  elevation %>% 
  terra::terrain(
    opt = 'slope',
    unit = 'degrees')

#calculates aspect raster
aspect <- 
  elevation %>% 
  terra::terrain(
    opt = 'aspect',
    unit = 'degrees')


#retrieve elevation, slope and aspect
recorders_points <-
recorders_points %>% 
  mutate(elevation =
           recorders_points %>% 
           vect() %>% 
           extract(
             rast(elevation),
             .,
             mean,
             na.rm = TRUE) %>% 
  pull()) %>% 
  mutate(aspect =
           recorders_points %>% 
           vect() %>% 
           extract(
             rast(aspect),
             .,
             mean,
             na.rm = TRUE) %>% 
           pull()) %>% 
  mutate(slope =
           recorders_points %>% 
           vect() %>% 
           extract(
             rast(slope),
             .,
             mean,
             na.rm = TRUE) %>% 
           pull()) 


# temperature and rainfall ----------------------------------------
indices_df<-
read_csv("D:/joined_data/indices.csv", col_names = TRUE)

full_df <-
indices_df %>% as_tibble() %>% 
  mutate(recorder_id = as.character(recorder_id)) %>% 
  left_join(recorders_points %>% 
              rename(recorder_id = Name) %>% 
              as_tibble()) %>% 
  left_join(recorders_coord %>% 
              rename(recorder_id = Name) %>% 
              as_tibble())

#daily rainfall
#daily precipitation data from the Climate Hazards Group

#summer
chirps_summer <- get_chirps(
    data.frame(lon = recorders_coord$Longitude,
               lat = recorders_coord$Latitude),
             c("2022-06-01", "2022-07-10"),
    server = "CHC") %>% 
  rename(Longitude = lon, Latitude = lat) %>% 
  select(-id)

chirps_summer<-
full_df %>% mutate(date = lubridate::date(recorded_date_time)) %>% 
  filter(season == "summer") %>% 
  left_join(chirps_summer)


#fall
chirps_fall <- get_chirps(
  data.frame(lon = recorders_coord$Longitude,
             lat = recorders_coord$Latitude),
  c("2022-09-01", "2022-10-30"),
  server = "CHC") %>% 
  rename(Longitude = lon, Latitude = lat) %>% 
  select(-id)

full_df<-
  full_df %>% mutate(date = lubridate::date(recorded_date_time)) %>% 
  filter(season == "fall") %>% 
  left_join(chirps_fall) %>% 
  bind_rows(chirps_summer, .) %>% 
    rename(d_precipitation = chirps) %>% 
    select(-date)

lubridate::tz(full_df$recorded_date_time) <- "America/Los_Angeles"

#Categorize aspect
full_df <- 
full_df %>% mutate(
   cat_aspect = case_when(
     aspect > 270 ~ "N",
     aspect < 90 ~ "N",
     TRUE ~ "S"))


# #sun radiation
# temp<-
# full_df %>% mutate(aspect = units::set_units(aspect, "degree")) %>% 
#   mutate(rad = units::set_units(aspect, "radians")) %>% 
#   select(aspect, rad, recorder_id) %>% 
#   mutate(south = rad + units::set_units(pi, "radians")) %>% 
#   mutate(south_d = units::set_units(south, "degree"))
# 
# temp<-
# full_df %>% mutate(aspect = aspect + 180) %>% 
#   mutate(DOY = DayOfYear(recorded_date_time))
# 
# temp2 <-
# temp %>% mutate(sun_d_radiation =
# DirectRadiation(DOY = DOY, Lat = Latitude, Lon= Longitude, SLon=-118, DS=0, Elevation = elevation, Slope = slope, Aspect = aspect)) %>% 
#   select(recorded_date_time, sun_d_radiation)


# Sunrise(DOY = temp$DOY, Lat = temp$Latitude)

# #FINISH GRASS FUNCTION
# #rgrass sun radiation
# execGRASS("r.sun",
#           elevation = elevation,
#           aspect = aspect,
#           slope = slope,
# )

# Fire polygons -----------------------------------------------------------

topanga_polygon <- read_sf("D:/shape_files/topanga_merged.shp") %>%
  select(YEAR1, geometry) %>% 
  mutate_at("YEAR1", str_replace, " ", "") %>% 
  filter(YEAR1 == "1961" | YEAR1 =="1984" | YEAR1 =="2021")

topanga_1961 <-
  topanga_polygon %>% filter(YEAR1 == "1961") %>% 
  select(geometry) %>% 
  st_cast("MULTILINESTRING")

topanga_2021 <-
  topanga_polygon %>% filter(YEAR1 == "2021") %>% 
  select(geometry) %>% 
  st_cast("MULTILINESTRING")

topanga_84_21 <-
  topanga_polygon %>% filter(YEAR1 == "2021") %>% 
  st_union(topanga_polygon %>% filter(YEAR1 == "1984")) %>%
  fill_holes(threshold = units::set_units(0.1, km^2)) %>% 
  st_buffer(0.001) %>% select(geometry) %>% 
  st_cast("MULTILINESTRING")

eleanor_polygon <- read_sf("D:/shape_files/eleanor_merged.shp") %>%
  select(YEAR1, geometry) %>% 
  mutate_at("YEAR1", str_replace, " ", "") %>% 
  filter(YEAR1 == "2006" | YEAR1 =="1984" | YEAR1 =="1985"| 
           YEAR1 =="1985" | YEAR1 =="1988" | YEAR1 =="2017")

eleanor_2006 <-
  eleanor_polygon %>% filter(YEAR1 == "2006") %>% 
  select(geometry) %>% 
  st_cast("MULTILINESTRING")
  
eleanor_80s <-
eleanor_polygon %>% filter(YEAR1 == "2017") %>% 
  st_union(eleanor_polygon %>% filter(YEAR1 == "1988")) %>%
  st_union(eleanor_polygon %>% filter(YEAR1 == "1984")) %>% 
  st_union(eleanor_polygon %>% filter(YEAR1 == "1985")) %>% 
  fill_holes(threshold = units::set_units(0.1, km^2)) %>% 
  st_buffer(0.001) %>% select(geometry) %>% 
  st_cast("MULTILINESTRING")
  
ranch_polygon <- read_sf("D:/shape_files/ranch_merged.shp") %>% 
  select(layer, geometry) %>% 
  mutate_at("layer", str_replace, "less than", "unburnt") %>% 
  mutate_at("layer", str_replace, " ", "") %>% 
  mutate_at("layer", str_replace, " ", "")

ranch_woolsey <-  read_sf("D:/shape_files/FIRE_NAME_Woolsey.shp") %>% 
  select(YEAR1, geometry) %>% select(geometry) %>% 
  st_cast("MULTILINESTRING") %>% st_transform(.,crs=4326)
  
ranch_green_meadows <- read_sf("D:/shape_files/green_meadows_1993.shp") %>% 
  select(YEAR1, geometry) %>% select(geometry) %>% 
  st_cast("MULTILINESTRING")

ranch_springs  <- read_sf("D:/shape_files/springs_2013.shp") %>% 
  select(YEAR1, geometry) %>% select(geometry) %>% 
  st_cast("MULTILINESTRING")

#measure distance from recorders to >15 years old fire area
recorders_points<-
recorders_points %>% 
  mutate(distance_10y =  
           case_when(
             Name %in% c(36,16,21,9,26,4,42,8) ~ #eleanor 80s
           st_distance(., eleanor_80s),
           Name %in% c(2,12,30,23) ~ #eleanor 2006
             st_distance(., eleanor_2006),
           Name %in% c(22,5,19,14) ~ #eleanor unburnt
             units::set_units(0, m) ,
           Name %in% c(25,17,43,34) ~ #topanga 2021
             st_distance(., topanga_2021),
           Name %in% c(18,15,6,39) ~ #topanga 1984
             st_distance(., topanga_84_21),
           Name %in% c(24,27,20,41) ~ #topanga 1961
             st_distance(., topanga_1961),
           Name %in% c(40,1,31,33) ~ #topanga unburnt
             units::set_units(0, m) ,
           Name %in% c(38,29,7,3) ~ #rancho woolsey
             st_distance(., ranch_woolsey),
           Name %in% c(45,10,35,11) ~ #rancho green meadows
             st_distance(., ranch_green_meadows),
           Name %in% c(32,37,28,44) ~ #rancho springs
             st_distance(., ranch_springs),
           TRUE ~ NA))

full_df <-
recorders_points %>% 
  mutate(distance_10y = as.numeric(distance_10y)) %>% 
  rename(recorder_id = Name) %>% 
  select(recorder_id, distance_10y) %>% 
  left_join(full_df, .)

full_df <-
full_df %>% 
  mutate(last_fire =  
           case_when(
             recorder_id %in% c("36","16","21","9") ~ 1984, #eleanor
             recorder_id %in% c("26","4","42","8") ~ 1988,
             recorder_id %in% c("2","12","30","23") ~ 2006,
             recorder_id %in% c("22","5","19","14") ~ 1900,
             recorder_id %in% c("25","17","43","34") ~ 2021, #topanga
             recorder_id %in% c("18","15","6","39") ~ 1984,
             recorder_id %in% c("24","27","20","41") ~ 1961,
             recorder_id %in% c("40","1","31","33") ~ 1900,
             recorder_id %in% c("38","29","7","3") ~ 2018, #rancho
             recorder_id %in% c("45","10","35","11") ~ 1993,
             recorder_id %in% c("32","37","28","44") ~ 2013,
             TRUE ~ NA))

full_df %>% select(-geometry) %>% 
  write_csv("D:/full_data/full_data.csv")

#visualize raster plus points to make sure they match
tm_basemap(c("OpenTopoMap",
             "Esri.WorldTopoMap",
             "USGS.USTopo",
             "USGS.USImageryTopo")) +
  elevation %>% 
  tm_shape(name = "Elevation") +
  tm_raster(palette = "plasma",
            n = 80,
            legend.show = F,
            alpha = 0.75) +
  recorders_points %>%
  tm_shape(name = "Recordings") +
  tm_dots()

tm_basemap(c("OpenTopoMap",
             "Esri.WorldTopoMap",
             "USGS.USTopo",
             "USGS.USImageryTopo")) +
  ranch_springs  %>%
  tm_shape(name = "StatePark") +
  tm_lines() +
  recorders_points %>%
  tm_shape(name = "Recordings") +
  tm_dots()
