# Added lines 2 through 13 to Data section of paper.qmd 8/27/2026
library(tidyverse)
source("R/moving-average.R")

LTER <- read_csv("data/LUQ_LTER_MDLs.csv")

BQ1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")

BQ2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")

BQ3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")

PRM <- read_csv("data/RioMameyesPuenteRoto.csv")

# Added lines 17 through 27 to Methods section of paper.qmd 8/27/2026

# Data cleaning ----------------------------------------------------------

BQ1_Cleaned <- BQ1 |>
  select(Sample_ID, Sample_Date, `NH4-N`, Ca, Mg, `NO3-N`, K)

BQ2_Cleaned <- BQ2 |>
  select(Sample_ID, Sample_Date, `NH4-N`, Ca, Mg, `NO3-N`, K)

BQ3_Cleaned <- BQ3 |>
  select(Sample_ID, Sample_Date, `NH4-N`, Ca, Mg, `NO3-N`, K)

PRM_Cleaned <- PRM |>
  select(Sample_ID, Sample_Date, `NH4-N`, Ca, Mg, `NO3-N`, K)


# Added lines 31 through 34 to Methods section of paper.qmd 8/27/2026

# BQ1 Site Moving Averages -----------------------------------------------
BQ1_smoothed <- moving_average(BQ1_Cleaned)
# adding a site column for joining later
BQ1_smoothed <- mutate(BQ1_smoothed, site_ID = "BQ1")

#BQ1_smoothed_plot <- BQ1_smoothed |>
#  pivot_longer(
#   cols = c(NH4N_ugL, Ca_mgL, Mg_mgL, NO3N_ugL, K_mgL),
#   names_to = "ion",
#  values_to = "concentration")

# ggplot(
#   data = BQ1_smoothed_plot,
#   mapping = aes(
#     x = window_start,
#     y = concentration,
#    color = ion
#    )
# ) +
# geom_point() +
#  geom_line() +
# scale_x_date(name = "Years") +
# facet_wrap(~ion, scales = "free")

# Added lines 57 through 93 to Methods section of paper.qmd on 8/27/2026
# (did not have moving avg function used yet)

# BQ2 Moving Averages ----------------------------------------------------
BQ2_smoothed <- tibble(
  window_start = seq(ymd("1984-05-20"), ymd("1994-12-31"), by = "9 weeks"),
  NH4N_ugL = NA,
  Ca_mgL = NA,
  Mg_mgL = NA,
  NO3N_ugL = NA,
  K_mgL = NA
)


for (i in 1:nrow(BQ2_smoothed)) {
  ws <- BQ2_smoothed$window_start[i]
  w2 <- ws + weeks(9)

  NH4N_data <- BQ2_Cleaned$`NH4-N`[
    (ws <= BQ2_Cleaned$Sample_Date & BQ2_Cleaned$Sample_Date < w2)
  ]
  Ca_data <- BQ2_Cleaned$Ca[
    (ws <= BQ2_Cleaned$Sample_Date & BQ2_Cleaned$Sample_Date < w2)
  ]
  Mg_data <- BQ2_Cleaned$Mg[
    (ws <= BQ2_Cleaned$Sample_Date & BQ2_Cleaned$Sample_Date < w2)
  ]
  NO3N_data <- BQ2_Cleaned$`NO3-N`[
    (ws <= BQ2_Cleaned$Sample_Date & BQ2_Cleaned$Sample_Date < w2)
  ]
  K_data <- BQ2_Cleaned$K[
    (ws <= BQ2_Cleaned$Sample_Date & BQ2_Cleaned$Sample_Date < w2)
  ]

  mean_NH4N <- mean(NH4N_data, na.rm = TRUE)
  mean_Ca <- mean(Ca_data, na.rm = TRUE)
  mean_Mg <- mean(Mg_data, na.rm = TRUE)
  mean_NO3N <- mean(NO3N_data, na.rm = TRUE)
  mean_K <- mean(K_data, na.rm = TRUE)

  BQ2_smoothed[i, 2] <- mean_NH4N
  BQ2_smoothed[i, 3] <- mean_Ca
  BQ2_smoothed[i, 4] <- mean_Mg
  BQ2_smoothed[i, 5] <- mean_NO3N
  BQ2_smoothed[i, 6] <- mean_K
}
# adding a site column for joining later
BQ2_smoothed <- mutate(BQ2_smoothed, site_ID = "BQ2")

# Added lines 99 through 135 to Methods section of paper.qmd on 8/27/2026
# (did not have moving avg function used yet)

# BQ3 Moving Averages ----------------------------------------------------
BQ3_smoothed <- tibble(
  window_start = seq(ymd("1984-05-20"), ymd("1994-12-31"), by = "9 weeks"),
  NH4N_ugL = NA,
  Ca_mgL = NA,
  Mg_mgL = NA,
  NO3N_ugL = NA,
  K_mgL = NA
)


for (i in 1:nrow(BQ3_smoothed)) {
  ws <- BQ3_smoothed$window_start[i]
  w2 <- ws + weeks(9)

  NH4N_data <- BQ3_Cleaned$`NH4-N`[
    (ws <= BQ3_Cleaned$Sample_Date & BQ3_Cleaned$Sample_Date < w2)
  ]
  Ca_data <- BQ3_Cleaned$Ca[
    (ws <= BQ3_Cleaned$Sample_Date & BQ3_Cleaned$Sample_Date < w2)
  ]
  Mg_data <- BQ3_Cleaned$Mg[
    (ws <= BQ3_Cleaned$Sample_Date & BQ3_Cleaned$Sample_Date < w2)
  ]
  NO3N_data <- BQ3_Cleaned$`NO3-N`[
    (ws <= BQ3_Cleaned$Sample_Date & BQ3_Cleaned$Sample_Date < w2)
  ]
  K_data <- BQ3_Cleaned$K[
    (ws <= BQ3_Cleaned$Sample_Date & BQ3_Cleaned$Sample_Date < w2)
  ]

  mean_NH4N <- mean(NH4N_data, na.rm = TRUE)
  mean_Ca <- mean(Ca_data, na.rm = TRUE)
  mean_Mg <- mean(Mg_data, na.rm = TRUE)
  mean_NO3N <- mean(NO3N_data, na.rm = TRUE)
  mean_K <- mean(K_data, na.rm = TRUE)

  BQ3_smoothed[i, 2] <- mean_NH4N
  BQ3_smoothed[i, 3] <- mean_Ca
  BQ3_smoothed[i, 4] <- mean_Mg
  BQ3_smoothed[i, 5] <- mean_NO3N
  BQ3_smoothed[i, 6] <- mean_K
}
# adding a site column for joining later
BQ3_smoothed <- mutate(BQ3_smoothed, site_ID = "BQ3")

# Added lines 140 through 176 to Methods section of paper.qmd on 8/27/2026
# (did not have moving avg function used yet)

# PRM Moving Averages ----------------------------------------------------
PRM_smoothed <- tibble(
  window_start = seq(ymd("1984-05-20"), ymd("1994-12-31"), by = "9 weeks"),
  NH4N_ugL = NA,
  Ca_mgL = NA,
  Mg_mgL = NA,
  NO3N_ugL = NA,
  K_mgL = NA
)


for (i in 1:nrow(PRM_smoothed)) {
  ws <- PRM_smoothed$window_start[i]
  w2 <- ws + weeks(9)

  NH4N_data <- PRM_Cleaned$`NH4-N`[
    (ws <= PRM_Cleaned$Sample_Date & PRM_Cleaned$Sample_Date < w2)
  ]
  Ca_data <- PRM_Cleaned$Ca[
    (ws <= PRM_Cleaned$Sample_Date & PRM_Cleaned$Sample_Date < w2)
  ]
  Mg_data <- PRM_Cleaned$Mg[
    (ws <= PRM_Cleaned$Sample_Date & PRM_Cleaned$Sample_Date < w2)
  ]
  NO3N_data <- PRM_Cleaned$`NO3-N`[
    (ws <= PRM_Cleaned$Sample_Date & PRM_Cleaned$Sample_Date < w2)
  ]
  K_data <- PRM_Cleaned$K[
    (ws <= PRM_Cleaned$Sample_Date & PRM_Cleaned$Sample_Date < w2)
  ]

  mean_NH4N <- mean(NH4N_data, na.rm = TRUE)
  mean_Ca <- mean(Ca_data, na.rm = TRUE)
  mean_Mg <- mean(Mg_data, na.rm = TRUE)
  mean_NO3N <- mean(NO3N_data, na.rm = TRUE)
  mean_K <- mean(K_data, na.rm = TRUE)

  PRM_smoothed[i, 2] <- mean_NH4N
  PRM_smoothed[i, 3] <- mean_Ca
  PRM_smoothed[i, 4] <- mean_Mg
  PRM_smoothed[i, 5] <- mean_NO3N
  PRM_smoothed[i, 6] <- mean_K
}
# adding a site column for joining later
PRM_smoothed <- mutate(PRM_smoothed, site_ID = "PRM")

# Added lines 180 through 191 to Methods section of paper.qmd on 8/27/2026

# Joining all smoothed sites ---------------------------------------------

sites_smoothed_joined <- BQ1_smoothed |>
  full_join(
    BQ2_smoothed,
    by = c(
      "window_start",
      "site_ID",
      "NH4N_ugL",
      "Ca_mgL",
      "K_mgL",
      "Mg_mgL",
      "NO3N_ugL"
    )
  ) |>
  full_join(
    BQ3_smoothed,
    by = c(
      "window_start",
      "site_ID",
      "NH4N_ugL",
      "Ca_mgL",
      "K_mgL",
      "Mg_mgL",
      "NO3N_ugL"
    )
  ) |>
  full_join(
    PRM_smoothed,
    by = c(
      "window_start",
      "site_ID",
      "NH4N_ugL",
      "Ca_mgL",
      "K_mgL",
      "Mg_mgL",
      "NO3N_ugL"
    )
  )


# Added lines 196 through 215 to Results section of paper.qmd on 8/27/2026

# Plotting smoothed and joined data for all sites together ---------------

sites_smoothed_joined_plot <- sites_smoothed_joined |>
  pivot_longer(
    cols = c(NH4N_ugL, Ca_mgL, Mg_mgL, NO3N_ugL, K_mgL),
    names_to = "ion",
    values_to = "concentration"
  )

ggplot(
  data = sites_smoothed_joined_plot,
  mapping = aes(
    x = window_start,
    y = concentration,
    color = site_ID
  )
) +
  geom_point() +
  geom_line() +
  scale_x_date(name = "Years") +
  facet_wrap(~ion, scales = "free")
