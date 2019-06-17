library(tidyverse)
library(rgdal)
library(data.table)

setwd("C:/Users/KelseyWalker/vision_zero")
source("code/functions/unfactor_df.R")

read.crashes <- function(path){
  path %>% 
    read.csv(skip = 8, header = T) %>% # adjust header information
    unfactor.df()
}

## read in all raw data
crashes <- c(201401:201412, 
             201501:201512, 
             201601:201612, 
             201701:201712, 
             201801:201812,
             201901:201906) %>% 
  as.list() %>% 
  lapply(function(yyyymm){
    crashes_path <- paste0("data/raw/txdot_cris_crashes_harris_fortbend_montgomery_",yyyymm,".csv") # fix
    crashes <- crashes_path %>% read.crashes()
    crashes$year <- floor(yyyymm/100)
    crashes$month <- yyyymm %% 100
    return(crashes)
  })

# test for single set of column names
crashes %>% lapply(colnames) %>% unique() 
## merge into one data frame
crashes <- crashes %>%
  rbindlist() %>% 
  as.data.frame(stringsAsFactors=F)
## remove crashes without latitude/longitude
crashes %>% 
  filter(is.na(as.numeric(Latitude))) %>% 
  group_by(Latitude) %>% 
  tally()
crashes %>% 
  filter(is.na(as.numeric(Longitude))) %>% 
  group_by(Longitude) %>% 
  tally()
crashes <- crashes %>% 
  filter(Latitude != "No Data" & Longitude != "No Data")
## transform Latitude & Longtiude into numeric variables
crashes[c("Latitude", "Longitude")] <- crashes[c("Latitude", "Longitude")] %>% 
  lapply(as.numeric)
## project (longitude, latitude) into (x, y) -- state plane texas south central projection, units are feet
crashes_xy <- crashes[c("Longitude", "Latitude")] %>% 
  as.matrix() %>% 
  SpatialPoints(proj4string = CRS("+init=epsg:4326")) %>% # wgs84
  spTransform(CRSobj = CRS("+init=epsg:2278")) %>% # texas south central (feet)
  as.data.frame()
crashes$x <- crashes_xy[,1]
crashes$y <- crashes_xy[,2]
crashes <- crashes %>% select(-Longitude, -Latitude)
rm(crashes_xy)

# read in units data
units <- c(201401:201412, 
           201501:201512, 
           201601:201612, 
           201701:201712, 
           201801:201812,
           201901:201906) %>% 
  as.list() %>% 
  lapply(function(yyyymm){
    units_path <- paste0("data/raw/txdot_cris_units_harris_fortbend_montgomery_",yyyymm,".csv") # fix later
    units <- units_path %>% read.crashes()
    units <- units %>% select(Crash.ID, Unit.Description)
    return(units)
  })
# test for one set of column names
units %>% lapply(colnames) %>% unique() 
# consolidate into single data frame
units <- units %>% 
  rbindlist() %>% 
  as.data.frame(stringsAsFactors = F)
# calculate number of cars, pedestrians, and bikes for each crash id and join to crashes dataset
units %>% group_by(Unit.Description) %>% tally() 
units_crashes_xw <- units %>% 
  group_by(Crash.ID) %>% 
  summarise(n_cars = sum(Unit.Description == "1 - MOTOR VEHICLE", na.rm = T), 
            n_peds = sum(Unit.Description == "4 - PEDESTRIAN", na.rm = T), 
            n_bikes = sum(Unit.Description == "3 - PEDALCYCLIST", na.rm = T)) %>% 
  ungroup()
units_crashes_xw %>% summarise(sum(n_cars), sum(n_peds), sum(n_bikes))
units_crashes_xw %>% filter(Crash.ID %in% crashes$Crash.ID) %>% summarise(sum(n_cars), sum(n_peds), sum(n_bikes))
# join to crashes dataset
crashes %>% nrow()
crashes <- crashes %>% left_join(units_crashes_xw, by = "Crash.ID")
crashes %>% nrow()
rm(units, units_crashes_xw)

crashes %>% summarise(sum(is.na(n_cars)), sum(is.na(n_peds)), sum(is.na(n_bikes)))

# filter out crashes that are either intersection (related) or ped/bike
crashes %>% group_by(Intersection.Related) %>% tally()
crashes <- crashes %>% 
  filter(Intersection.Related %in% c("INTERSECTION", "INTERSECTION RELATED") | 
           n_peds > 0 |
           n_bikes > 0)
crashes %>% group_by(Intersection.Related) %>% tally()
crashes %>% summarise(sum(n_peds), sum(n_bikes))

## assign time period -- time period 11 (1/2019 - 6/2019) is test set
crashes <- crashes %>% 
  mutate(period = (year-2014)*2 + ceiling(month/6))

## general field clean up
## replace "No Data" with null values in all columns
crashes[] <- crashes[] %>% 
  lapply(function(v){
    v[v %in% c("No Data","NO DATA")] <- NA
    return(v)
  })

write.csv(crashes, "data/txdot_cris_crashes_harris_fortbend_montgomery_2014_2019.csv", row.names = F)
