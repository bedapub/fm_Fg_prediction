
# Define functions for calculations
calculate_confidence_interval <- function(median_value, sigma) {
  if (sigma == 0) {
    lower_bound <- NaN
    upper_bound <- NaN
  } else {
    mu <- log(median_value)
    z <- 1.96
    lower_log <- mu - z * sigma
    upper_log <- mu + z * sigma
    lower_bound <- round(exp(lower_log), 2)
    upper_bound <- round(exp(upper_log), 2)
  }
  return(c(lower_bound, upper_bound))
}

calculate_sigma <- function(median_value, ci_string) {
  if (grepl(", ", ci_string)) {
    char_vector <- strsplit(ci_string, ", ")[[1]]
    numeric_vector <- as.numeric(char_vector)
    L <- numeric_vector[1]
    U <- numeric_vector[2]
    log_L <- log(L)
    log_U <- log(U)
    mu <- log(median_value)
    z <- 1.96
    sigma_U <- (log_U -  mu) / z
    sigma_L <- (mu - log_L) / z
    print("sigma L and U")
    print(sigma_L)
    print(sigma_U)
    sigma = (sigma_U + sigma_L) / 2
    # Calculate the absolute difference
    abs_diff <- abs(sigma_L - sigma_U)
    # Calculate the percentage difference
    percentage_diff <- (abs_diff / sigma) * 100
    
    # Check if the percentage difference is within 10%
    if (percentage_diff <= 10) {
      message_sigma = 1
      print("sigma_L and sigma_U are similar within 10%")
    } else {
      print("sigma_L and sigma_U are not similar within 10%")
      message_sigma = 0
    }
    
    # Print the percentage difference for reference
    print(paste("Percentage difference:", percentage_diff, "%"))
  } else {
    sigma <- 0.1
    sigma_L <- NA
    sigma_U <- NA
  }
  return(c(sigma, sigma_L, sigma_U, message_sigma))
}




round_to_three_digits <- function(input) {
  # Check if the input is numeric
  if (is.numeric(input)) {
    # Round to 3 decimal places
    rounded_number <- round(input, digits = 3)
    return(rounded_number)
  } else {
    # Return a message if the input is not numeric
    return(input)
  }
}