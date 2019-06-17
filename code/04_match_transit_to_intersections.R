library(tidyverse)
library(rgdal)
library(data.table)

setwd("C:/Users/KelseyWalker/vision_zero")

source("code/functions/unfactor_df.R")
source("code/functions/read_gtfs.R")

# read in intersections dataset and select intersection ids/coordinates
intersections <- "data/intersections_coords.csv" %>% 
  read.csv() %>%
  unfactor.df()
intersections <- intersections %>% 
  select(Intersection.ID, int_x = x, int_y = y) 

# read in gtfs dataset and select stop ids/coordinates
gtfs <- "data/raw/gtfs_2019_0210_0831" %>% 
  read.gtfs()
stops <- gtfs$stops %>% 
  select(Stop.ID = stop_id, stop_x, stop_y)

# summarise gtfs data
weekday_service_id <- 6
stops_routes_trips <- gtfs$trips %>% 
  filter(service_id == weekday_service_id) %>% 
  left_join(gtfs$patterns %>% 
              select(pattern_id, stop_id), 
            by = "pattern_id") %>%
  select(Stop.ID = stop_id, trip_id, route_short_name)

# prep stops dataset for spatial join to intersections  
stops <- stops %>% 
  arrange(ceiling(stop_x/1000), ceiling(stop_x/1000))
stops$group <- ceiling((1:nrow(stops))/500)

# match stops to the nearest intersections
stops_ints_closest <- stops$group %>% 
  unique() %>% 
  as.list() %>%
  lapply(function(g){
    stops_g <- stops %>% 
      filter(group == g) %>% 
      mutate(join_id = TRUE)
    intersections_g <- intersections %>% 
      filter(min(stops_g$stop_x)-1000 <= int_x & int_x <= max(stops_g$stop_x) + 1000 & 
               min(stops_g$stop_y)-1000 <= int_y & int_y <= max(stops_g$stop_y) + 1000) %>%
      mutate(join_id = TRUE)
    stops_g <- stops_g %>% 
      left_join(intersections_g, by="join_id") %>% 
      filter(abs(int_x - stop_x) <= 1000 & 
               abs(int_y - stop_y) <= 1000)
  })
stops_ints_closest <- stops_ints_closest %>% 
  rbindlist() %>%
  as.data.frame(stringsAsFactors = F)
stops_ints_closest <- stops_ints_closest %>% 
  mutate(Int.Distance.Ft = sqrt((int_x - stop_x)^2 + (int_y - stop_y)^2))
stops_ints_closest <- stops_ints_closest %>% 
  group_by(Stop.ID) %>% 
  filter(Int.Distance.Ft == min(Int.Distance.Ft)) %>% 
  ungroup()
stops_ints_closest_over1000 <- stops %>% 
  filter(!Stop.ID %in% stops_ints_closest$Stop.ID) %>% 
  mutate(join_id = TRUE) %>% 
  left_join(intersections %>% 
              mutate(join_id = TRUE),
            by = "join_id") %>% 
  mutate(Int.Distance.Ft = sqrt((int_x - stop_x)^2 + (int_y - stop_y)^2)) %>% 
  group_by(Stop.ID) %>% 
  filter(Int.Distance.Ft == min(Int.Distance.Ft)) %>% 
  ungroup()
stops_ints_closest <- stops_ints_closest %>% 
  rbind(stops_ints_closest_over1000)
rm(stops_ints_closest_over1000)

# match stops to intersections within 200 feet
stops_ints_200 <- stops$group %>% 
  unique() %>% 
  as.list() %>% 
  lapply(function(g){
    stops_g <- stops %>% 
      filter(group == g) %>% 
      mutate(join_id = TRUE)
    intersections_g <- intersections %>% 
      filter(min(stops_g$stop_x)-200 <= int_x & int_x <= max(stops_g$stop_x) + 200 & 
               min(stops_g$stop_y)-200 <= int_y & int_y <= max(stops_g$stop_y) + 200) %>%
      mutate(join_id = TRUE)
    stops_g <- stops_g %>% 
      left_join(intersections_g, by="join_id") %>% 
      filter(abs(int_x - stop_x) <= 200 & 
               abs(int_y - stop_y) <= 200)
    
  })
stops_ints_200 %>% lapply(nrow) %>% unlist() %>% sum()
stops_ints_200 <- stops_ints_200 %>% 
  rbindlist() %>% 
  as.data.frame(stringsAsFactors = F)
stops_ints_200 <- stops_ints_200 %>%
  mutate(Int.Distance.Ft = sqrt((int_x - stop_x)^2 + (int_y - stop_y)^2)) %>% 
  filter(Int.Distance.Ft <= 200)

# match stops to intersections within 400 feet
stops_ints_400 <- stops$group %>% 
  unique() %>% 
  as.list() %>% 
  lapply(function(g){
    stops_g <- stops %>% 
      filter(group == g) %>% 
      mutate(join_id = TRUE)
    intersections_g <- intersections %>% 
      filter(min(stops_g$stop_x)-400 <= int_x & int_x <= max(stops_g$stop_x) + 400 & 
               min(stops_g$stop_y)-400 <= int_y & int_y <= max(stops_g$stop_y) + 400) %>%
      mutate(join_id = TRUE)
    stops_g <- stops_g %>% 
      left_join(intersections_g, by="join_id") %>% 
      filter(abs(int_x - stop_x) <= 400 & 
               abs(int_y - stop_y) <= 400)
    
  })
stops_ints_400 %>% lapply(nrow) %>% unlist() %>% sum()
stops_ints_400 <- stops_ints_400 %>% 
  rbindlist() %>% 
  as.data.frame(stringsAsFactors = F)
stops_ints_400 <- stops_ints_400 %>% 
  mutate(Int.Distance.Ft = sqrt((int_x - stop_x)^2 + (int_y - stop_y)^2)) %>% 
  filter(Int.Distance.Ft <= 400)

# for each method (closest, within 200 feet, within 400 feet) aggregate the number of transit stops, weekday routes, and weekday trips by intersection
ints_stops_closest <-  stops_ints_closest %>% 
  left_join(stops_routes_trips, by = "Stop.ID") %>% 
  group_by(Intersection.ID) %>% 
  summarise(n_transit_stops_closest = n_distinct(Stop.ID), 
            n_transit_routes_closest = n_distinct(route_short_name), 
            n_transit_trips_closest = n_distinct(trip_id)) %>%
  ungroup()
ints_stops_200 <- stops_ints_200 %>% 
  left_join(stops_routes_trips, by = "Stop.ID") %>% 
  group_by(Intersection.ID) %>% 
  summarise(n_transit_stops_200ft = n_distinct(Stop.ID), 
            n_transit_routes_200ft = n_distinct(route_short_name), 
            n_transit_trips_200ft = n_distinct(trip_id)) %>% 
  ungroup()
ints_stops_400 <- stops_ints_400 %>% 
  left_join(stops_routes_trips, by = "Stop.ID") %>% 
  group_by(Intersection.ID) %>% 
  summarise(n_transit_stops_400ft = n_distinct(Stop.ID), 
            n_transit_routes_400ft = n_distinct(route_short_name), 
            n_transit_trips_400ft = n_distinct(trip_id)) %>% 
  ungroup()

# combine the fields from all methods
ints_stops <- intersections %>% 
  select(Intersection.ID) %>% 
  left_join(ints_stops_closest, by = "Intersection.ID") %>% 
  left_join(ints_stops_200, by = "Intersection.ID") %>%
  left_join(ints_stops_400, by = "Intersection.ID")
ints_stops[-1] <- ints_stops[-1] %>% 
  lapply(function(v){
    v[is.na(v)] <- 0
    return(v)
  })

write.csv(ints_stops, "data/intersections_transit.csv", row.names = F)
