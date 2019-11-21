# Define MSE
mse_loss <- function(y, y_hat){
  
  # Throw error if predicted and true values are not the same length
  if (length(y) != length(y_hat)) stop('True and predicted values are not the same')
  
  return(mean((y-y_hat)^2))
}

# Define poisson loss function
# poisson_loss <- function(y, y_hat){
#   
#   # Throw error if predicted and true values are not the same length
#   if (length(y) != length(y_hat)) stop('True and predicted values are not the same')
#   
#   return(mean(y*y_hat - exp(y_hat)))
# }
