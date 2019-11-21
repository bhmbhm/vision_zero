# Load libraries
library(dplyr)
library(tidyr)
library(fastDummies)

# Load in data
int_coords <- read.csv(paste0(getwd(), "/data/intersections_coords.csv"), header = TRUE, stringsAsFactors = FALSE) %>%
  select(-c(x,y))
crash_int_cw <- read.csv(paste0(getwd(), "/data/crashes_intersections_crosswalk.csv"), header = TRUE, stringsAsFactors = FALSE)
int_trans <- read.csv(paste0(getwd(), "/data/intersections_transit.csv"), header = TRUE, stringsAsFactors = FALSE)
crash_df <- read.csv(paste0(getwd(), "/data/txdot_cris_crashes_harris_fortbend_montgomery_2014_2019.csv"), header = TRUE, stringsAsFactors = FALSE)
int_sw <- read.csv(paste0(getwd(), "/data/intersections_sidewalks.csv"), header = TRUE, stringsAsFactors = FALSE)

int_ids <- int_coords %>%
  select(Intersection.ID)

#List of columns to keep for downstream analysis, as determined by Lauren and Kelsey
crash_keep_cols <- c('Intersection.ID',
              'year',
              'month',
              'Adjusted.Roadway.Part',
              'Crash.ID', # this is needed
              'Crash.Severity',
              'Day.of.Week',
              'Int.Distance.Ft', # filter for crashes within 200 feet of intersections
              'Light.Condition',
              'Manner.of.Collision',
              'Median.Width',
              'Number.of.Lanes',
              'Number.of.Entering.Roads',
              'Road.Base.Type',
              'Roadbed.Width',
              'Roadway.Alignment',
              'Roadway.Function',
              'Roadway.Relation',
              'Roadway.Part',
              'Roadway.Type',
              'Speed.Limit',
              'Surface.Condition',
              'Traffic.Control.Type',
              'Weather.Condition',
              'n_bikes',
              'n_cars',
              'n_peds') 

int_keep_cols <- c('Intersection.ID',
                   'n_roads',
                    'n_street_names', 
                    'n_transit_routes_200ft',
                    'n_transit_routes_400ft', 
                    'n_transit_routes_closest',
                    'n_transit_stops_200ft', 
                    'n_transit_stops_400ft',
                    'n_transit_stops_closest', 
                    'n_transit_trips_200ft',
                    'n_transit_trips_400ft', 
                    'n_transit_trips_closest',
                    'sidewalk_dist_50ft', 
                    'sidewalk_50ft',
                    'sidewalk_dist_100ft', 
                    'sidewalk_100ft',
                    'sidewalk_dist_200ft', 
                    'sidewalk_200ft',
                    'road_classes')

crash_dummy_cols<- c('Adjusted.Roadway.Part',
                    'Day.of.Week',
                    'Light.Condition',
                    'Road.Base.Type',
                    'Roadway.Alignment', 
                    'Roadway.Function',
                    'Roadway.Relation', 
                    'Roadway.Part', 
                    'Roadway.Type',
                    'Surface.Condition',
                    'Traffic.Control.Type',
                    'Weather.Condition',
                    'manner_of_collision',
                    'num_entering_roads_cat')

int_dummy_cols <- c('road_classes')

# Join data frames
full_df <- crash_df %>%
  left_join(crash_int_cw, by = "Crash.ID") %>%
  select(crash_keep_cols)

int_df <- int_coords %>%
  left_join(int_trans, by = "Intersection.ID") %>% 
  left_join(int_sw, by = "Intersection.ID") %>%
  select(int_keep_cols)

# Rounding function
mround <- function(x,base){ 
  base*round(x/base) 
}

# Pre-process data
process_intersections <- function(in_df, cols){
  out_df <- in_df %>%
    mutate(sidewalk_50ft = ifelse(sidewalk_50ft, 1, 0),
           sidewalk_100ft = ifelse(sidewalk_100ft, 1, 0),
           sidewalk_200ft = ifelse(sidewalk_200ft, 1, 0)) %>%
    generate_dummies(., cols)
  return(out_df)
}


pre_process_crashes <- function(in_df){
  out_df <- in_df %>%
    mutate(Speed.Limit = ifelse(Speed.Limit < 0, NA, Speed.Limit),
           Speed.Limit = ifelse(Speed.Limit %% 5 != 0, mround(Speed.Limit, 5), Speed.Limit),
           Crash.Severity = toupper(Crash.Severity),
           Crash.Severity = ifelse(Crash.Severity == 'N - NOT INJURED', 'NOT INJURED', Crash.Severity),
           Crash.Severity = ifelse(Crash.Severity == 'C - POSSIBLE INJURY', 'POSSIBLE INJURY', Crash.Severity),
           Crash.Severity = ifelse(Crash.Severity == '99 - UNKNOWN', 'UNKNOWN', Crash.Severity),
           Crash.Severity = ifelse(Crash.Severity == 'B - NON-INCAPACITATING INJURY', 'NON-INCAPACITATING INJURY', Crash.Severity),
           Crash.Severity = ifelse(Crash.Severity == 'A - SUSPECTED SERIOUS INJURY', 'SUSPECTED SERIOUS INJURY', Crash.Severity),
           Crash.Severity = ifelse(Crash.Severity == 'K - KILLED', 'KILLED', Crash.Severity),
           injured = ifelse(Crash.Severity == 'NOT INJURED', 0, 1),
           killed = ifelse(Crash.Severity == 'KILLED', 1, 0)) %>%
    filter(Crash.Severity != 'UNKNOWN') %>%
    mutate(Roadbed.Width = ifelse(Roadbed.Width == 'No Data', NA, Roadbed.Width),
           Roadbed.Width = ifelse(Roadbed.Width == 'nan', NA, Roadbed.Width),
           Roadbed.Width = as.numeric(Roadbed.Width),
           Number.of.Lanes = ifelse(Number.of.Lanes == 'No Data', NA, Number.of.Lanes),
           Number.of.Lanes = ifelse(Number.of.Lanes == 'nan', NA, Number.of.Lanes),
           Number.of.Lanes = as.numeric(Number.of.Lanes),
           Number.of.Lanes = ifelse(is.na(Number.of.Lanes), 1, Number.of.Lanes),
           Roadway.Type = ifelse(Roadway.Type == 'No Data', NA, Roadway.Type),
           Roadway.Type = ifelse(Roadway.Type == 'nan', NA, Roadway.Type),
           Median.Width = ifelse(Median.Width == 'No Data', NA, Median.Width),
           Median.Width = ifelse(Median.Width == 'nan', NA, Median.Width),
           Median.Width = as.numeric(Median.Width),
           weekend = ifelse(Day.of.Week == "FRIDAY" | Day.of.Week == "SATURDAY" | Day.of.Week == "SUNDAY", 1, 0),
           manner_of_collision = gsub("^(.*)\\s-\\s.*$", "\\1", Manner.of.Collision),
           num_entering_roads_cat = gsub("^(\\d{1,2})( - ).*$", "\\1", Number.of.Entering.Roads),
           num_entering_roads = as.numeric(num_entering_roads_cat),
           num_entering_roads = ifelse(num_entering_roads == 97 | num_entering_roads == 98, 0, num_entering_roads),
           num_entering_roads_cat = ifelse(num_entering_roads_cat == "97", NA, num_entering_roads_cat),
           num_entering_roads_cat = ifelse(num_entering_roads_cat == "98", "Other", num_entering_roads_cat)) %>%
    mutate(month = gsub("^(\\d{1})$", "0\\1", month)) %>%
    mutate(date = as.Date(paste0(year, "-", month, "-01")))
  return(out_df)
}

generate_dummies <- function(in_df, cols){
  # Create dummy columns
  out_df <- fastDummies::dummy_cols(in_df, select_columns = cols) %>%
    select(-cols)
  return(out_df)
}

crash_process <- function(in_df, int_df, drop_cols, date = FALSE, intersection_id = FALSE){
  
  # Set rownames by intersection ID
  rownames(in_df) <- in_df$Crash.ID
  
  out_df <- in_df %>%
    select(-c(year,
              month,
              Crash.ID,
              Crash.Severity,
              Manner.of.Collision,
              Number.of.Entering.Roads
              )) %>%
    select(Intersection.ID, injured, killed, date, everything()) %>%
    group_by(Intersection.ID) %>%
    fill(Median.Width, Number.of.Lanes, Roadbed.Width, Speed.Limit,) %>%
    fill(Median.Width, Number.of.Lanes, Roadbed.Width, Speed.Limit, .direction = "up") %>%
    ungroup()
  
  # Join intersection data
  out_df <- out_df %>%
    left_join(int_df, by = "Intersection.ID")
  
  if(date == FALSE){
    out_df <- out_df %>%
      select(-date)
  }
  
  if(intersection_id == FALSE){
    out_df <- out_df %>%
      select(-Intersection.ID)
  }
  
  return(out_df)
}

aggregate_by_intersection <- function(in_df, int_df, date_start, date_end, drop_avg = FALSE){
  
  # Intersection attributes to drop before aggregation
  int_drop_list <- colnames(int_df)[colnames(int_df) != "Intersection.ID"]
  
  in_df <- in_df %>%
    select(-int_drop_list)
  
  # Convert to date type
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)
  
  # Define dummy columns
  start_var <- grep("Adjusted.Roadway.Part_1 - MAIN/PROPER LANE", colnames(in_df))
  end_var <- ncol(in_df)
  dummy_vars <- colnames(in_df)[start_var:end_var]
  
  # Filter by date
  tmp_df <- in_df %>%
    filter(date >= date_start & date <= date_end)
  
  # Filter by distance to intersection
  tmp_df <- tmp_df %>%
    filter(Int.Distance.Ft <= 200)

  # Aggregate by intersection
  smart_df <- tmp_df %>%
    group_by(Intersection.ID) %>%
    summarise(
      num_crashes = n(),
      num_injured = sum(injured, na.rm = TRUE),
      avg_injured = mean(injured, na.rm = TRUE),
      num_killed = sum(killed, na.rm = TRUE),
      avg_killed = mean(killed, na.rm = TRUE),
      avg_int_distance = mean(Int.Distance.Ft, na.rm = TRUE),
      median_width = Median.Width[1],
      number_of_lanes = Number.of.Lanes[1],
      roadbed_width = Roadbed.Width[1],
      speed_limit = Speed.Limit[1],
      num_bikes = sum(n_bikes, na.rm = TRUE),
      avg_bikes = mean(n_bikes, na.rm = TRUE),
      num_cars = sum(n_cars, na.rm = TRUE),
      avg_cars = mean(n_cars, na.rm = TRUE),
      num_peds = sum(n_peds, na.rm = TRUE),
      avg_peds = mean(n_peds, na.rm = TRUE),
      #num_roads = n_roads[1],
      #num_street_names = n_street_names[1],
      num_entering_roads = num_entering_roads[1],
      num_weekend = sum(weekend, na.rm = TRUE),
      avg_weekend = mean(weekend, na.rm = TRUE),
      # num_transit_routes_200 = n_transit_routes_200ft[1],
      # num_transit_routes_400 = n_transit_routes_400ft[1],
      # num_transit_routes_closest = n_transit_routes_closest[1],
      # num_transit_stops_200 = n_transit_stops_200ft[1],
      # num_transit_stops_400 = n_transit_stops_400ft[1],
      # num_transit_stops_closest = n_transit_stops_closest[1],
      # num_transit_trips_200 = n_transit_trips_200ft[1],
      # num_transit_trips_400 = n_transit_trips_400ft[1],
      # num_transit_trips_closest = n_transit_trips_closest[1],
      # sidewalk_total_dist_50 = sidewalk_dist_50ft[1],
      # sidewalk_num_50 = sidewalk_50ft[1],
      # sidewalk_total_dist_100 = sidewalk_dist_100ft[1],
      # sidewalk_num_100 = sidewalk_100ft[1],
      # sidewalk_total_dist_200 = sidewalk_dist_200ft[1],
      # sidewalk_num_200 = sidewalk_200ft[1]
    )
  
  # Generate sum and mean features for all dummy columns
  dummy_df_sum <- tmp_df %>%
    group_by(Intersection.ID) %>%
    summarise_at(dummy_vars, sum, na.rm = TRUE)
  
  colnames(dummy_df_sum)[-1] <- paste0("num_", colnames(dummy_df_sum)[-1])
  
  dummy_df_mean <- tmp_df %>%
    group_by(Intersection.ID) %>%
    summarise_at(dummy_vars, mean, na.rm = TRUE)
  
  colnames(dummy_df_mean)[-1] <- paste0("avg_", colnames(dummy_df_mean)[-1])
  
  # Bring everything back together
  out_df <- smart_df %>%
    left_join(dummy_df_sum, by = "Intersection.ID") %>%
    left_join(dummy_df_mean, by = "Intersection.ID")
  
  # Drop average columns (optional)
  if(drop_avg){
    drop_avgs <- grepl("^avg_.*$", colnames(out_df))
    out_df <- out_df[,!drop_avgs]
  }
  
  # Join on full intersection dataset
  out_df <- int_ids %>%
    left_join(out_df, by = "Intersection.ID") 
  
  # Replace missing values
  out_df[is.na(out_df)] <- 0
  
  # Join intersection attributes
  out_df <- out_df %>%
    left_join(int_df, by = "Intersection.ID")
  
  # Make intersection ID the row label and drop the column
  rownames(out_df) <- out_df$Intersection.ID
  
  out_df <- out_df %>%
    select(-Intersection.ID)

  return(out_df)
}


# Run pipeline
raw_process <- pre_process_crashes(full_df)
raw_dummy <- generate_dummies(raw_process, crash_dummy_cols)
int_process <- process_intersections(int_df, int_dummy_cols)
all_crashes <- crash_process(raw_dummy, int_process, intersection_id = TRUE, date = TRUE)
