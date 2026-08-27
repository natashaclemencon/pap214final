# The input to this function should be a data frame containing stream chemistry data
moving_average <- function(streamdata) {
  # Initialize a tibble to contain the results
  result <- tibble(
    window_start = seq(
      ymd("1988-01-01"),
      ymd("1994-12-31"),
      by = "9 weeks"
    ),
    NH4N_ugL = NA,
    Ca_mgL = NA,
    Mg_mgL = NA,
    NO3N_ugL = NA,
    K_mgL = NA,
    site_ID = streamdata$Sample_ID[1]
  )

  # Fill in the iterator and sequence
  for (i in 1:nrow(result)) {
    # Create variables for the start and end of the current window
    w1 <- result$window_start[i]
    w2 <- w1 + weeks(9)

    # Create a logical vector, called "in_window", that says which samples are inside the window
    # Hint: you'll compare sample dates to the start and end of the window
    in_window <- w1 <= streamdata$Sample_Date & streamdata$Sample_Date < w2

    # Use indexing to pull out the ion concentrations that fall inside the window

    nh4n_window <- streamdata$`NH4-N`[in_window]
    ca_window <- streamdata$Ca[in_window]
    mg_window <- streamdata$Mg[in_window]
    no3n_data <- streamdata$`NO3-N`[in_window]
    k_window <- streamdata$K[in_window]

    # Calculate the mean of each ion concentration and fill in the result
    result$NH4N_ugL[i] <- mean(nh4n_window, na.rm = TRUE)
    result$Ca_mgL[i] <- mean(ca_window, na.rm = TRUE)
    result$Mg_mgL[i] <- mean(mg_window, na.rm = TRUE)
    result$NO3N_ugL[i] <- mean(no3n_data, na.rm = TRUE)
    result$K_mgL[i] <- mean(k_window, na.rm = TRUE)
  }

  return(result)
}
