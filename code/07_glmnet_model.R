# Source pipeline
source('code/06_baseline_model.R')
source('code/functions/loss_functions.R')

# Load libraries
library(glmnet)
library(broom)

# Ridge Regression ----
# ridge_fit_2014 <- cv.glmnet(x = as.matrix(inj_2014), 
#                             y = inj_2015$num_injured, 
#                             family = "poisson",
#                             alpha = 0)
# 
# ridge_fit_2015 <- cv.glmnet(x = as.matrix(inj_2015), 
#                             y = inj_2016$num_injured, 
#                             family = "poisson",
#                             alpha = 0)
# 
# ridge_fit_2016 <- cv.glmnet(x = as.matrix(inj_2016), 
#                             y = inj_2017$num_injured, 
#                             family = "poisson",
#                             alpha = 0)
# 
# ridge_fit_2017 <- cv.glmnet(x = as.matrix(inj_2017), 
#                             y = inj_2018$num_injured, 
#                             family = "poisson",
#                             alpha = 0)
# 
# # Save models
# saveRDS(ridge_fit_2014, "code/models/ridge_cvfit_2014.rds")
# saveRDS(ridge_fit_2015, "code/models/ridge_cvfit_2015.rds")
# saveRDS(ridge_fit_2016, "code/models/ridge_cvfit_2016.rds")
# saveRDS(ridge_fit_2017, "code/models/ridge_cvfit_2017.rds")

# Load models
ridge_fit_2014 <- readRDS("code/models/ridge_cvfit_2014.rds")
ridge_fit_2015 <- readRDS("code/models/ridge_cvfit_2015.rds")
ridge_fit_2016 <- readRDS("code/models/ridge_cvfit_2016.rds")
ridge_fit_2017 <- readRDS("code/models/ridge_cvfit_2017.rds")

# Calculate training and test loss for each year ----

# 2014

# Refit un-penalized poisson regression model
# new_2014 <- data.frame(y = inj_2015$num_injured, inj_2014)
# 
# good_col_idx <- which(apply(new_2014, 2, function(x) length(unique(x))) != 1)
# 
# new_2014 <- new_2014[, good_col_idx]
# 
# pois_fit_2014 <- glm(formula = y ~ .,
#                      data = new_2014,
#                      family = "poisson")
# 
# pois_2014_sum <- tidy(pois_fit_2014)

# Predict
y_train_2014 <- predict(ridge_fit_2014, 
                        newx = as.matrix(inj_2014),
                        type = 'response',
                        s = 'lambda.min')

y_test_2014 <- predict(ridge_fit_2014, 
                        newx = as.matrix(inj_2015),
                        type = 'response',
                        s = 'lambda.min')

ridge_train_m_loss_2014 <- mse_loss(inj_2015$num_injured, y_train_2014)
ridge_test_m_loss_2014 <- mse_loss(inj_2016$num_injured, y_test_2014)

# Diagnostics
pct_improvement_2014 <- (m_loss_2014 - ridge_test_m_loss_2014)*100/m_loss_2014

effects_2014 <- tidy(coef(ridge_fit_2014, s = 'lambda.min'))

# 2015

# Refit un-penalized poisson regression model
# new_2015 <- data.frame(y = inj_2016$num_injured, inj_2015)
# 
# good_col_idx <- which(apply(new_2015, 2, function(x) length(unique(x))) != 1)
# 
# new_2015 <- new_2015[, good_col_idx]
# 
# pois_fit_2015 <- glm(formula = y ~ .,
#                      data = new_2015,
#                      family = "poisson")
# 
# pois_2015_sum <- tidy(pois_fit_2015)

# Predict
y_train_2015 <- predict(ridge_fit_2015, 
                        newx = as.matrix(inj_2015),
                        type = 'response',
                        s = 'lambda.min')

y_test_2015 <- predict(ridge_fit_2015, 
                        newx = as.matrix(inj_2016),
                        type = 'response',
                        s = 'lambda.min')

ridge_train_m_loss_2015 <- mse_loss(inj_2016$num_injured, y_train_2015)
ridge_test_m_loss_2015 <- mse_loss(inj_2017$num_injured, y_test_2015)

pct_improvement_2015 <- (m_loss_2015 - ridge_test_m_loss_2015)*100/m_loss_2015

# 2016

# Refit un-penalized poisson regression model
# new_2016 <- data.frame(y = inj_2017$num_injured, inj_2016)
# 
# good_col_idx <- which(apply(new_2016, 2, function(x) length(unique(x))) != 1)
# 
# new_2016 <- new_2016[, good_col_idx]
# 
# pois_fit_2016 <- glm(formula = y ~ .,
#                      data = new_2016,
#                      family = "poisson")
# 
# pois_2016_sum <- tidy(pois_fit_2016)

# Predict
y_train_2016 <- predict(ridge_fit_2016, 
                        newx = as.matrix(inj_2016),
                        type = 'response',
                        s = 'lambda.min')

y_test_2016 <- predict(ridge_fit_2016, 
                        newx = as.matrix(inj_2017),
                        type = 'response',
                        s = 'lambda.min')

ridge_train_m_loss_2016 <- mse_loss(inj_2017$num_injured, y_train_2016)
ridge_test_m_loss_2016 <- mse_loss(inj_2018$num_injured, y_test_2016)

pct_improvement_2016 <- (m_loss_2016 - ridge_test_m_loss_2016)*100/m_loss_2016

# 2017

# Refit un-penalized poisson regression model
# new_2017 <- data.frame(y = inj_2018$num_injured, inj_2017)
# 
# good_col_idx <- which(apply(new_2017, 2, function(x) length(unique(x))) != 1)
# 
# new_2017 <- new_2017[, good_col_idx]
# 
# pois_fit_2017 <- glm(formula = y ~ .,
#                      data = new_2017,
#                      family = "poisson")
# 
# pois_2017_sum <- tidy(pois_fit_2017)

# Predict
# y_train_2017 <- predict(ridge_fit_2017, 
#                         newx = as.matrix(inj_2017),
#                         type = 'response',
#                         s = 'lambda.min')
# 
# y_test_2017 <- predict(ridge_fit_2017, 
#                         newx = as.matrix(inj_2018),
#                         type = 'response',
#                         s = 'lambda.min')
# 
# ridge_train_m_loss_2017 <- mse_loss(inj_2018$num_injured, y_train_2017)
# ridge_test_m_loss_2017 <- mse_loss(inj_2019$num_injured, y_test_2017)
# 
# improvement_2017 <- (m_loss_2017 - ridge_test_m_loss_2017)*100/m_loss_2017