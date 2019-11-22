# Source pipeline
source('code/05_aggregate_by_intersection.R')
source('code/functions/loss_functions.R')

# Pull data year by year
inj_2014 <- aggregate_by_intersection(all_crashes,
                                      int_process,
                                      date_start = "2014-01-01",
                                      date_end = "2015-01-01",
                                      drop_avg = T)

inj_2015 <- aggregate_by_intersection(all_crashes, 
                                      int_process,
                                      date_start = "2015-01-01",
                                      date_end = "2016-01-01",
                                      drop_avg = T)

inj_2016 <- aggregate_by_intersection(all_crashes, 
                                      int_process,
                                      date_start = "2016-01-01",
                                      date_end = "2017-01-01",
                                      drop_avg = T)

inj_2017 <- aggregate_by_intersection(all_crashes, 
                                      int_process,
                                      date_start = "2017-01-01",
                                      date_end = "2018-01-01",
                                      drop_avg = T)

inj_2018 <- aggregate_by_intersection(all_crashes, 
                                      int_process,
                                      date_start = "2018-01-01",
                                      date_end = "2019-01-01",
                                      drop_avg = T)

# Calculate loss using the previous years injury as predicted value
m_loss_2014 <- mse_loss(inj_2014$num_injured, inj_2015$num_injured)
m_loss_2015 <- mse_loss(inj_2015$num_injured, inj_2016$num_injured)
m_loss_2016 <- mse_loss(inj_2016$num_injured, inj_2017$num_injured)
m_loss_2017 <- mse_loss(inj_2017$num_injured, inj_2018$num_injured)

