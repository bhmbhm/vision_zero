# just do it the slow way
# do it right

library(tidyverse)
library(rgdal)
library(data.table)

setwd("E:/Dropbox (Traffic Engineers)/kelsey/projects/hackathon19")
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
             201801:201812) %>% 
  as.list() %>% 
  lapply(function(yyyymm){
    crashes_path <- paste0("data/raw/txdot_cris_crashes_harris_fortbend_montgomery_",yyyymm,".csv") # fix
    crashes <- crashes_path %>% read.crashes()
    crashes$year <- floor(yyyymm/100)
    crashes$month <- yyyymm %% 100
    return(crashes)
  })

## figure out column stuff
crashes %>% 
  lapply(colnames) %>%
  unique()

## merge into one data frame
crashes <- crashes %>%
  rbindlist() %>% 
  as.data.frame(stringsAsFactors=F)
## remove crashes without latitude/longitude data from the dataset
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
crashes_xy <- crashes[c("Longitude", "Latitude")] %>% 
  as.matrix() %>% 
  SpatialPoints(proj4string = CRS("+init=epsg:4326")) %>% # wgs84
  spTransform(CRSobj = CRS("+init=epsg:2278")) %>% # texas south central
  as.data.frame()
crashes$x <- crashes_xy[,1]
crashes$y <- crashes_xy[,2]
crashes <- crashes %>% select(-Longitude, -Latitude)
rm(crashes_xy)

# read in units data
units <- crashes <- c(201401:201412, 
                      201501:201512, 
                      201601:201612, 
                      201701:201712, 
                      201801:201812) %>% 
  as.list() %>% 
  lapply(function(yyyymm){
    units_path <- paste0("data/raw/txdot_cris_units_hgac_",yyyymm,".csv") # fix later
    units <- units_path %>% read.crashes()
    units <- units %>% select(Crash.ID, Unit.Description)
    return(units)
  })
# consolidate list into single data frame
units %>% lapply(colnames) %>% unique() # test for one set of column names
units <- units %>% 
  rbindlist() %>% 
  as.data.frame(stringsAsFactors = F)
# calculate number of cars, pedestrians, and bikes for each crash id and join to crashes dataset
units %>% group_by(Unit.Description) %>% tally() 
units_crashes_xw <- units %>% 
  group_by(Crash.ID) %>% 
  summarise(n_cars = sum(Unit.Description == "Motor Vehicle", na.rm = T), 
            n_peds = sum(Unit.Description == "Pedestrian", na.rm = T), 
            n_bikes = sum(Unit.Description == "Pedalcyclist", na.rm = T)) %>% 
  ungroup()
units_crashes_xw %>% summarise(sum(n_cars), sum(n_peds), sum(n_bikes))
# join to crashes dataset
crashes %>% nrow()
crashes <- crashes %>% left_join(units_crashes_xw, by = "Crash.ID")
crashes %>% nrow()
rm(units, units_crashes_xw)

crashes %>% summarise(sum(is.na(n_cars)), sum(is.na(n_peds)), sum(is.na(n_bikes)))
# fix if needed -- not needed for 2014 data

# filter out crashes that are either intersection (related) or ped/bike
##### explore at intersection flag
crashes %>% group_by(Intersection.Related) %>% tally()
crashes <- crashes %>% 
  filter(Intersection.Related %in% c("Intersection", "Intersection Related") | 
           n_peds > 0 |
           n_bikes > 0)
crashes %>% group_by(Intersection.Related) %>% tally()
crashes %>% summarise(n_peds = sum(n_peds), n_bikes = sum(n_bikes))

crashes %>%
  group_by(Street.Name) %>% 
  tally() %>%
  arrange(desc(n)) %>%
  head(50) %>% 
  as.data.frame()
crashes %>% 
  group_by(Intersecting.Street.Name) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  head(50) %>% 
  as.data.frame()
crashes %>%
  group_by(Street.Name) %>% 
  tally() %>%
  arrange(desc(n)) %>%
  filter(str_detect(Street.Name, "UNKNOWN|N/A"))
crashes %>% 
  group_by(Intersecting.Street.Name) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  filter(str_detect(Intersecting.Street.Name, "UNKNOWN|N/A"))
crashes[c("Street.Name", "Intersecting.Street.Name")] <- crashes[c("Street.Name", "Intersecting.Street.Name")] %>% 
  lapply(function(v){
    v[v %in% c("UNKNOWN","N/A")] <- NA
    return(v)
  })

unique_street_names <- sort(unique(c(crashes$Street.Name, crashes$Intersecting.Street.Name)))
crashes[c("Street.Name", "Intersecting.Street.Name")] <- crashes[c("Street.Name", "Intersecting.Street.Name")] %>% 
  lapply(factor, levels = unique_street_names)
crashes <- crashes %>% 
  mutate(Intersection.Name = ifelse(as.integer(Street.Name) < as.integer(Intersecting.Street.Name), 
                                    paste(Street.Name, Intersecting.Street.Name, sep = " @ "), 
                                    paste(Intersecting.Street.Name, Street.Name, sep = " @ ")))

## assign time period
crashes <- crashes %>% 
  mutate(period = (year-2014)*2 + ceiling(month/6))

## general field clean up
## replace "No Data" with null values in all columns
crashes[] <- crashes[] %>% 
  lapply(function(v){
    v[v == c("No Data","NO DATA")] <- NA
    return(v)
  })



