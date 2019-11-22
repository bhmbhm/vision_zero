library(tidyverse)

setwd("C:/Users/KelseyWalker/vision_zero")

source("code/functions/unfactor_df.R")

# read in crash dataset -- this only includes intersection/intersection-related crashes and pedestrian/bike crashes
crashes <- "data/txdot_cris_crashes_harris_fortbend_montgomery_2014_2019.csv" %>% 
  read.csv() %>% 
  unfactor.df()
# read in crashes intersections crosswalk -- includes crash id, intersection id (of nearest intersection), and distance to that intersection
crashes_intersections <- "data/crashes_intersections_crosswalk.csv" %>% 
  read.csv() %>% 
  unfactor.df()
# join intersection info to main crash data frame via crash id
crashes <- crashes %>% 
  left_join(crashes_intersections, by = "Crash.ID")
rm(crashes_intersections)
# exclude crashes that are over 200 feet from an intersection
crashes <- crashes %>%
  filter(Int.Distance.Ft <= 200)
# divide into training and test sets
crashes_test <- crashes %>% filter(year == 2019)
crashes <- crashes %>% filter(year < 2019)
