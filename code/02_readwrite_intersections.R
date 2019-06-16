library(tidyverse)
library(rgdal)
library(data.table)

source("E:/Dropbox (Traffic Engineers)/GIS/1. Source Files/R/functions/unfactor.df.R")

starmap <- "E:/Dropbox (Traffic Engineers)/GIS/1. Source Files/STARMap Road - December 2018/HarrisFortBendMontgomery_StarMap_Centerlines.csv" %>% 
  read.csv() %>%
  unfactor.df()

endpoints <- starmap %>% 
  select(StreetName, RoadClass, x = x0, y = y0) %>% 
  rbind(starmap %>% 
          select(StreetName, RoadClass, x = x1, y = y1))
endpoints <- endpoints %>% 
  filter(RoadClass != "PROPOSED")
endpoints$RoadClass <- endpoints$RoadClass %>% 
  factor(levels = c("FREEWAY", "TOLLWAY", "HOV", "FRONTAGE", "RAMP", "MAJOR", "LOCAL", "ACCESS", "PRIVATE", "PARKING"))

intersections <- endpoints %>% 
  group_by(x, y) %>% 
  summarise(street_names = paste(sort(unique(StreetName)), collapse = " @ "),
            road_classes = paste(sort(unique(RoadClass)), collapse = " @ "),
            n_roads = n(), 
            n_street_names = n_distinct(StreetName)) %>%
  ungroup()
intersections <- intersections %>% 
  mutate(street_names = ifelse(street_names == " ", NA, street_names))
intersections$street_names <- intersections$street_names %>% 
  str_replace("^ +@ ", "")
intersections <- intersections %>% 
  filter(n_roads > 2 | n_street_names > 1) %>% 
  filter(!road_classes %in% c("ACCESS", "PRIVATE", "PARKING", "ACCESS @ PRIVATE", "ACCESS @ PARKING", "PRIVATE @ PARKING"))

intersections$Intersection.ID <- 1:nrow(intersections)
write.csv(intersections, "E:/Dropbox (Traffic Engineers)/kelsey/projects/hackathon19/data/intersections_coords.csv" , row.names = F)


