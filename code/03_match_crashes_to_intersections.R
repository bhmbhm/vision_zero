library(tidyverse)
library(data.table)

#setwd("E:/Users/KelseyWalker/vision_zero")

source("code/functions/unfactor_df.R")

crashes <- "data/txdot_cris_crashes_harris_fortbend_montgomery_2014_2019.csv" %>% 
  read.csv() %>%
  unfactor.df()
crashes <- crashes %>% 
  select(Crash.ID, crash_x = x, crash_y = y)

intersections <- "data/intersections_coords.csv" %>% 
  read.csv() %>% 
  unfactor.df()
intersections <- intersections %>% 
  select(Intersection.ID, int_x = x, int_y = y)

crashes <- crashes %>%
  arrange(floor(crash_x/10000), floor(crash_y/10000))
crashes$group <- ceiling(1:nrow(crashes)/1000)
crashes_ints <- crashes$group %>% 
  unique() %>% 
  as.list() %>% 
  lapply(function(g){
    crashes_g <- crashes %>%
      filter(group == g) %>% 
      mutate(join_id = TRUE)
    intersections_g <- intersections %>% 
      filter(min(crashes_g$crash_x)-1000 <= int_x & int_x <= max(crashes_g$crash_x) + 1000 & 
             min(crashes_g$crash_y)-1000 <= int_y & int_y <= max(crashes_g$crash_y) + 1000) %>%
      mutate(join_id = TRUE)
    crashes_g <- crashes_g %>% 
      left_join(intersections_g, by="join_id") %>% 
      filter(abs(int_x - crash_x) < 1000 & 
             abs(int_y - crash_y) < 1000)
  })

crashes_ints <- crashes_ints %>% 
  rbindlist() %>% 
  as.data.frame(stringsAsFactors = F)
crashes_ints <- crashes_ints %>% 
  mutate(distance_ft = sqrt((crash_x - int_x)^2 + (crash_y - int_y)^2)) %>% 
  group_by(Crash.ID) %>% 
  filter(distance_ft == min(distance_ft)) %>%
  ungroup()
crashes_ints %>% summarise(n(), n_distinct(Crash.ID))
crashes %>% summarise(n(), n_distinct(Crash.ID))

crashes_ints_over1000 <- crashes %>% 
  filter(!Crash.ID %in% crashes_ints$Crash.ID) %>%
  mutate(join_id = TRUE) %>% 
  left_join(intersections %>% 
              mutate(join_id = TRUE), 
            by = "join_id") %>%
  mutate(distance_ft = sqrt((crash_x - int_x)^2 + (crash_y - int_y)^2)) %>% 
  group_by(Crash.ID) %>% 
  filter(distance_ft == min(distance_ft)) %>%
  ungroup()

crashes_ints %>% 
  nrow()
crashes_ints %>% 
  rbind(crashes_ints_over1000) %>% 
  summarise(n(), n_distinct(Crash.ID))
crashes %>% 
  summarise(n(), n_distinct(Crash.ID))
crashes_ints <- crashes_ints %>% 
  rbind(crashes_ints_over1000)
sum(crashes_ints$Crash.ID %in% crashes$Crash.ID) == nrow(crashes_ints) & nrow(crashes_ints) == nrow(crashes)
crashes_ints <- crashes_ints %>% 
  select(Crash.ID, Intersection.ID, Int.Distance.Ft = distance_ft)

write.csv(crashes_ints, "data/crashes_intersections_crosswalk.csv", row.names = F)