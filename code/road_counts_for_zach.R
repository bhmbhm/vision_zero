# Load library
library(dplyr)

# Read in data
join_data <- read.csv("data/SurfaceCrashes_SpatialJoin.csv")

# Create separate data frames for different scenarios -----
unlist_crashes <- function(crash_data, label) {
  crash_counts <- gsub(" ", "", unlist(strsplit(as.character(crash_data$UIDText), ","))) %>%
    table(.) %>%
    as.data.frame(.) %>%
    rename(., RoadID = `.`) %>%
    mutate(RoadID = as.character(RoadID))
  colnames(crash_counts)[2] <- label
  return(crash_counts)
}

# Pedestrian crashes
ped_crash <- join_data %>%
  filter(VZ_PedCount > 0)
  
ped_crash_counts <- unlist_crashes(ped_crash, "ped_crash_count")
  
# Bicycle crashes
bike_crash <- join_data %>%
  filter(VZ_BikeCount > 0)

bike_crash_counts <- unlist_crashes(bike_crash, "bike_crash_count")

# Vehicle crashes
car_crash <- join_data %>%
  filter(VZ_PedCount == 0 & VZ_BikeCount == 0)

car_crash_counts <- unlist_crashes(car_crash, "car_crash_count")

# Total crashes
total_crash_counts <- unlist_crashes(join_data, "total_crash_count") 

# Pedestrian fatalities
ped_death <- join_data %>%
  filter(VZ_PedCount > 0 & Crash_Sev_ID == 4)

ped_death_counts <- unlist_crashes(ped_death, "ped_death_crash_count")

# Bicycle fatalities
bike_death <- join_data %>%
  filter(VZ_BikeCount > 0 & Crash_Sev_ID == 4)

bike_death_counts <- unlist_crashes(bike_death, "bike_death_crash_count")

# Vehicle fatalities
car_death <- join_data %>%
  filter(VZ_PedCount == 0 & VZ_BikeCount == 0 & Crash_Sev_ID == 4)

car_death_counts <- unlist_crashes(car_death, "car_death_crash_count")

# Total fatalities
total_death <- join_data %>%
  filter(Crash_Sev_ID == 4)

total_death_counts <- unlist_crashes(total_death, "total_death_crash_count")

# Join all datasets together
final_counts <- total_crash_counts %>%
  left_join(ped_crash_counts, by = "RoadID") %>%
  left_join(bike_crash_counts, by = "RoadID") %>%
  left_join(car_crash_counts, by = "RoadID") %>%
  left_join(total_death_counts, by = "RoadID") %>%
  left_join(ped_death_counts, by = "RoadID") %>%
  left_join(bike_death_counts, by = "RoadID") %>%
  left_join(car_death_counts, by = "RoadID")

# Replace all missing values with zero
final_counts[is.na(final_counts)] <- 0

# Write to csv
write.csv(final_counts, "data/crash_and_fatal_crash_counts.csv", row.names = FALSE)