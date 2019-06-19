library(tidyverse)
library(rgdal)
library(data.table)

source("code/functions/unfactor_df.R")

sidewalks <- readOGR(dsn = "E:/Dropbox (Traffic Engineers)/kelsey/projects/hackathon19/data/raw", layer = "hgac_sidewalks_preliminary")

sidewalks@lines %>% lapply(function(lines){length(lines@Lines)}) %>% unique() # multiple values

sidewalks_lines <- sidewalks@lines %>% 
  lapply(function(line){
    1:length(line@Lines) %>% 
      as.list() %>% 
      lapply(function(i){
        line@Lines[[i]]@coords %>% 
          as.data.frame() %>% 
          mutate(segment_id = i)
      }) %>% 
      rbindlist() %>% 
      mutate(line_id = line@ID)
  })

sidewalks <- sidewalks_lines %>%
  rbindlist() %>% 
  as.data.frame(stringsAsFactors = F) %>% 
  select(line_id, segment_id, x = V1, y = V2) %>% 
  group_by(line_id, segment_id) %>%
  mutate(sequence = 1:n()) %>%
  ungroup()
rm(sidewalks_lines)
sidewalks <- sidewalks %>% 
  mutate(Sidewalk.ID = paste(line_id, segment_id, sep = "_"))
sidewalks$Sidewalk.ID <- sidewalks$Sidewalk.ID %>% 
  factor() %>% 
  as.integer()
sidewalks <- sidewalks %>%
  select(Sidewalk.ID, sw_x = x, sw_y = y, sw_seq = sequence)

# read in intersections coordinates and prep for spatial join
intersections <- "data/intersections_coords.csv" %>% 
  read.csv() %>% 
  unfactor.df()
intersections <- intersections %>% 
  select(Intersection.ID, int_x = x, int_y = y) %>% 
  arrange(floor(int_x/1000), floor(int_y/1000)) # arranging by general (x, y) gives groups a spatial component
# create intersection "groups" to avoid massive join
intersections$group <- ceiling((1:nrow(intersections))/1000)
intersections_sidewalks <- intersections$group %>% 
  unique() %>% 
  as.list() %>% 
  lapply(function(g){
    ints_g <- intersections %>% 
      filter(group == g) %>% 
      mutate(join_id = TRUE)
    sws_g <- sidewalks %>% 
      filter(min(ints_g$int_x)-200 <= sw_x & sw_x <= max(ints_g$int_x)+200 & 
             min(ints_g$int_y)-200 <= sw_y & sw_y <= max(ints_g$int_y)+200) %>% 
      mutate(join_id = TRUE)
    ints_g %>% 
      left_join(sws_g, by = "join_id") %>% 
      filter(abs(int_x - sw_x) <= 200 & 
             abs(int_y - sw_y) <= 200)
  })
intersections_sidewalks <- intersections_sidewalks %>% 
  rbindlist() %>% 
  as.data.frame(stringsAsFactors = F)
intersections_sidewalks <- intersections_sidewalks %>% 
  mutate(Int.Dist.Ft = sqrt((int_x - sw_x)^2 + (int_y - sw_y)^2)) %>% 
  filter(Int.Dist.Ft <= 200)
intersections_sidewalks <- intersections_sidewalks %>% 
  mutate(sw_seq_1 = sw_seq + 1) %>% 
  left_join(sidewalks %>% 
              select(Sidewalk.ID, sw_seq_1 = sw_seq, sw_x_1 = sw_x, sw_y_1 = sw_y),
            by = c("Sidewalk.ID", "sw_seq_1"))
intersections_sidewalks_alt <- intersections_sidewalks %>% 
  filter(is.na(sw_x_1)) %>% 
  select(-group, -join_id)
intersections_sidewalks <- intersections_sidewalks %>% 
  filter(!is.na(sw_x_1)) %>% 
  select(-group, -join_id)
intersections_sidewalks_alt <- intersections_sidewalks_alt %>% 
  select(Intersection.ID, int_x, int_y, Sidewalk.ID, sw_seq_1 = sw_seq, sw_x_1 = sw_x, sw_y_1 = sw_y) %>% 
  mutate(sw_seq = sw_seq_1 - 1)
intersections_sidewalks_alt <- intersections_sidewalks_alt %>% 
  left_join(sidewalks, by = c("Sidewalk.ID", "sw_seq"))
intersections_sidewalks_alt <- intersections_sidewalks_alt %>%
  mutate(Int.Dist.Ft = sqrt((sw_x - int_x)^2 + (sw_y - int_y)^2))
intersections_sidewalks <- intersections_sidewalks %>%
  rbind(intersections_sidewalks_alt[colnames(intersections_sidewalks)])
rm(intersections_sidewalks_alt)
intersections_sidewalks <- intersections_sidewalks %>% 
  unique()
intersections_sidewalks <- intersections_sidewalks %>%
  mutate(Int.Dist.Ft.1 = sqrt((int_x - sw_x_1)^2 + (int_y - sw_y_1)^2), 
         sw_m = (sw_y_1 - sw_y)/(sw_x_1 - sw_x)) %>% 
  mutate(sw_b = sw_y - sw_m * sw_x)

intersections_sidewalks_check <- intersections_sidewalks %>%
  filter(Int.Dist.Ft > 200 | Int.Dist.Ft.1 > 200)
intersections_sidewalks_check <- intersections_sidewalks_check %>% 
  mutate(a = sw_m^2 + 1,
         b = 2 * (sw_m*sw_b - int_x - sw_m*int_y),
         c = sw_b^2 - 2*sw_b*int_y + int_y^2 + int_x^2 - 200^2)

# sw_y = sw_m * sw_x + sw_b
# sw_y - sw_m * sw_x = sw_b

# y = sw_m * x + sw_b
# (x - int_x)^2 + (y - int_y)^2 == d^2
# (x - int_x)^2 + (sw_m * x + sw_b - int_y)^2 == d^2
# x^2 - 2*int_x*x + int_x^2 + sw_m^2*x^2 + 2*sw_m*sw_b*x - 2*sw_m*int_y*x + sw_b^2 - 2*sw_b*int_y + int_y^2 == d^2
# x^2 + sw_m^2*x^2 - 2*int_x*x + int_x^2 + 2*sw_m*sw_b*x - 2*sw_m*int_y*x + sw_b^2 - 2*sw_b*int_y + int_y^2 == d^2
# x^2 - 2*int_x*x + int_x^2 + sw_m^2*x^2 + 2*sw_m*sw_b*x - 2*sw_m*int_y*x + sw_b^2 - 2*sw_b*int_y + int_y^2 == d^2
# (1 + sw_m^2)*x^2 + 2*(sw_m*sw_b - int_x - sw_m*int_y)*x + sw_b^2 - 2*sw_b*int_y + int_y^2 + int_x^2 == d^2

# a = 1 + sw_m^2
# b = 2 * (sw_m*sw_b - int_x - sw_m*int_y)
# c = sw_b^2 - 2*sw_b*int_y + int_y^2 + int_x^2 - d^2

intersections_sidewalks_check <- intersections_sidewalks_check %>% 
  mutate(x_200_p = (-b + sqrt(b^2 - 4*a*c))/(2*a),
         x_200_m = (-b - sqrt(b^2 - 4*a*c))/(2*a)) %>% 
  mutate(y_200_p = sw_m * x_200_p + sw_b, 
         y_200_m = sw_m * x_200_m + sw_b)
intersections_sidewalks_check <- intersections_sidewalks_check %>% 
  mutate(p_on_line = (sw_x <= x_200_p & x_200_p <= sw_x_1) | (sw_x_1 <= x_200_p & x_200_p <= sw_x),
         m_on_line = (sw_x <= x_200_m & x_200_m <= sw_x_1) | (sw_x_1 <= x_200_m & x_200_m <= sw_x))
intersections_sidewalks_check <- intersections_sidewalks_check %>% 
  filter(p_on_line | m_on_line)
intersections_sidewalks_check <- intersections_sidewalks_check %>% 
  mutate(x_200 = ifelse(p_on_line, x_200_p, x_200_m), 
         y_200 = ifelse(p_on_line, y_200_p, y_200_m))
intersections_sidewalks_check <- intersections_sidewalks_check %>%
  mutate(Sidewalk.Dist.Ft = ifelse(Int.Dist.Ft <= 200, 
                                   sqrt((sw_x - x_200)^2 + (sw_y - y_200)^2), 
                                   sqrt((sw_x_1 - x_200)^2 + (sw_y_1 - y_200)^2)))
intersections_sidewalks_200 <- intersections_sidewalks %>% 
  filter(Int.Dist.Ft <= 200 & Int.Dist.Ft.1 <= 200) %>% 
  mutate(Sidewalk.Dist.Ft = sqrt((sw_x_1-sw_x)^2 + (sw_y_1-sw_y)^2)) %>% 
  select(Intersection.ID, Sidewalk.ID, sw_seq, Sidewalk.Dist.Ft) %>% 
  rbind(intersections_sidewalks_check %>%
          select(Intersection.ID, Sidewalk.ID, sw_seq, Sidewalk.Dist.Ft))

intersections_sidewalks_200_summary <- intersections_sidewalks_200 %>%
  group_by(Intersection.ID) %>% 
  summarise(sidewalk_dist_200ft = sum(Sidewalk.Dist.Ft)) %>% 
  ungroup()

intersections_sidewalks_100_check <- intersections_sidewalks %>% 
  filter(Int.Dist.Ft <= 100 | Int.Dist.Ft.1 <= 100) %>% 
  filter(Int.Dist.Ft > 100 | Int.Dist.Ft.1 > 100)
intersections_sidewalks_100_check <- intersections_sidewalks_100_check %>% 
  mutate(a = sw_m^2 + 1,
         b = 2 * (sw_m*sw_b - int_x - sw_m*int_y),
         c = sw_b^2 - 2*sw_b*int_y + int_y^2 + int_x^2 - 100^2) %>% 
  mutate(x_dist_p = (-b + sqrt(b^2 - 4*a*c))/(2*a),
         x_dist_m = (-b - sqrt(b^2 - 4*a*c))/(2*a)) %>% 
  mutate(y_dist_p = sw_m * x_dist_p + sw_b, 
         y_dist_m = sw_m * x_dist_m + sw_b) %>% 
  mutate(p_on_line = (sw_x <= x_dist_p & x_dist_p <= sw_x_1) | (sw_x_1 <= x_dist_p & x_dist_p <= sw_x),
         m_on_line = (sw_x <= x_dist_m & x_dist_m <= sw_x_1) | (sw_x_1 <= x_dist_m & x_dist_m <= sw_x))
intersections_sidewalks_100_check <- intersections_sidewalks_100_check %>% 
  filter(p_on_line | m_on_line) %>% 
  mutate(x_dist = ifelse(p_on_line, x_dist_p, x_dist_m), 
         y_dist = ifelse(p_on_line, y_dist_p, y_dist_m)) %>% 
  mutate(Sidewalk.Dist.Ft = ifelse(Int.Dist.Ft <= 100, 
                                   sqrt((sw_x - x_dist)^2 + (sw_y - y_dist)^2), 
                                   sqrt((sw_x_1 - x_dist)^2 + (sw_y_1 - y_dist)^2)))
intersections_sidewalks_100 <- intersections_sidewalks %>% 
  filter(Int.Dist.Ft <= 100 & Int.Dist.Ft.1 <= 100) %>% 
  mutate(Sidewalk.Dist.Ft = sqrt((sw_x_1-sw_x)^2 + (sw_y_1-sw_y)^2)) %>% 
  select(Intersection.ID, Sidewalk.ID, sw_seq, Sidewalk.Dist.Ft) %>% 
  rbind(intersections_sidewalks_100_check %>%
          select(Intersection.ID, Sidewalk.ID, sw_seq, Sidewalk.Dist.Ft))
intersections_sidewalks_100_summary <- intersections_sidewalks_100 %>% 
  group_by(Intersection.ID) %>% 
  summarise(sidewalk_dist_100ft = sum(Sidewalk.Dist.Ft)) %>% 
  ungroup()


intersections_sidewalks_50_check <- intersections_sidewalks %>% 
  filter(Int.Dist.Ft <= 50 | Int.Dist.Ft.1 <= 50) %>% 
  filter(Int.Dist.Ft > 50 | Int.Dist.Ft.1 > 50)
intersections_sidewalks_50_check <- intersections_sidewalks_50_check %>% 
  mutate(a = sw_m^2 + 1,
         b = 2 * (sw_m*sw_b - int_x - sw_m*int_y),
         c = sw_b^2 - 2*sw_b*int_y + int_y^2 + int_x^2 - 50^2) %>% 
  mutate(x_dist_p = (-b + sqrt(b^2 - 4*a*c))/(2*a),
         x_dist_m = (-b - sqrt(b^2 - 4*a*c))/(2*a)) %>% 
  mutate(y_dist_p = sw_m * x_dist_p + sw_b, 
         y_dist_m = sw_m * x_dist_m + sw_b) %>% 
  mutate(p_on_line = (sw_x <= x_dist_p & x_dist_p <= sw_x_1) | (sw_x_1 <= x_dist_p & x_dist_p <= sw_x),
         m_on_line = (sw_x <= x_dist_m & x_dist_m <= sw_x_1) | (sw_x_1 <= x_dist_m & x_dist_m <= sw_x))
intersections_sidewalks_50_check <- intersections_sidewalks_50_check %>% 
  filter(p_on_line | m_on_line) %>% 
  mutate(x_dist = ifelse(p_on_line, x_dist_p, x_dist_m), 
         y_dist = ifelse(p_on_line, y_dist_p, y_dist_m)) %>% 
  mutate(Sidewalk.Dist.Ft = ifelse(Int.Dist.Ft <= 50, 
                                   sqrt((sw_x - x_dist)^2 + (sw_y - y_dist)^2), 
                                   sqrt((sw_x_1 - x_dist)^2 + (sw_y_1 - y_dist)^2)))
intersections_sidewalks_50 <- intersections_sidewalks %>% 
  filter(Int.Dist.Ft <= 50 & Int.Dist.Ft.1 <= 50) %>% 
  mutate(Sidewalk.Dist.Ft = sqrt((sw_x_1-sw_x)^2 + (sw_y_1-sw_y)^2)) %>% 
  select(Intersection.ID, Sidewalk.ID, sw_seq, Sidewalk.Dist.Ft) %>% 
  rbind(intersections_sidewalks_50_check %>%
          select(Intersection.ID, Sidewalk.ID, sw_seq, Sidewalk.Dist.Ft))
intersections_sidewalks_50_summary <- intersections_sidewalks_50 %>% 
  group_by(Intersection.ID) %>% 
  summarise(sidewalk_dist_50ft = sum(Sidewalk.Dist.Ft)) %>% 
  ungroup()

intersections_sidewalks_summary <- intersections %>% 
  left_join(intersections_sidewalks_50_summary) %>%
  left_join(intersections_sidewalks_100_summary) %>%
  left_join(intersections_sidewalks_200_summary)
intersections_sidewalks_summary <- intersections_sidewalks_summary %>% 
  select(Intersection.ID, sidewalk_dist_50ft, sidewalk_dist_100ft, sidewalk_dist_200ft)
intersections_sidewalks_summary[2:4] <- intersections_sidewalks_summary[2:4] %>% 
  lapply(function(v){
    v[is.na(v)] <- 0
    return(v)
  })
intersections_sidewalks_summary[paste0("sidewalk_",c(50,100,200),"ft")] <- intersections_sidewalks_summary[paste0("sidewalk_dist_",c(50,100,200),"ft")] %>% 
  lapply(function(v){v > 0})

write.csv(intersections_sidewalks_summary, "data/intersections_sidewalks.csv", row.names = F)