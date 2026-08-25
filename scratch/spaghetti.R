library(tidyverse)

LTER <- read_csv("data/LUQ LTER MDLs.csv")
glimpse(LTER)
BQ1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")
glimpse(BQ1)
BQ2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")
glimpse(BQ2)
BQ3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")
glimpse(BQ3)
PRM <- read_csv("data/RioMameyesPuenteRoto.csv")
glimpse(PRM)

BQ1_Cleaned <- BQ1 |> 
  select(Sample_ID, Sample_Date, `NH4-N`, Ca, Mg, `NO3-N`, K)

BQ2_Cleaned <- BQ2 |> 
  select(Sample_ID, Sample_Date, `NH4-N`, Ca, Mg, `NO3-N`, K)

BQ3_Cleaned <- BQ3 |> 
  select(Sample_ID, Sample_Date, `NH4-N`, Ca, Mg, `NO3-N`, K)

PRM_Cleaned <- PRM |> 
  select(Sample_ID, Sample_Date, `NH4-N`, Ca, Mg, `NO3-N`, K)



BQ1_smoothed <- tibble(
  window_start = seq(ymd("1984-05-20"),
  ymd("1994-12-31"), 
  by = "9 weeks"), 
  NH4N_ugL = NA,
  Ca_mgL = NA,
  Mg_mgL = NA,
  NO3N_ugL = NA,
  K_mgL = NA
)


for(i in 1:nrow(BQ1_smoothed)) {
  w1 <- BQ1_smoothed$window_start[i]
  w2 <- w1 + weeks(9)

  NH4N_data <- BQ1_Cleaned$`NH4-N`[(w1 <= BQ1_Cleaned$Sample_Date & BQ1_Cleaned$Sample_Date < w2)]
  Ca_data <- BQ1_Cleaned$Ca[(w1 <= BQ1_Cleaned$Sample_Date & BQ1_Cleaned$Sample_Date < w2)]
  Mg_data <- BQ1_Cleaned$Mg[(w1 <= BQ1_Cleaned$Sample_Date & BQ1_Cleaned$Sample_Date < w2)]
  NO3N_data <- BQ1_Cleaned$`NO3-N`[(w1 <= BQ1_Cleaned$Sample_Date & BQ1_Cleaned$Sample_Date < w2)]
  K_data <-BQ1_Cleaned$K[(w1 <= BQ1_Cleaned$Sample_Date & BQ1_Cleaned$Sample_Date < w2)]

  mean_NH4N <- mean(NH4N_data, na.rm = TRUE)
  mean_Ca <- mean(Ca_data, na.rm = TRUE)
  mean_Mg <- mean(Mg_data, na.rm = TRUE)
  mean_NO3N <- mean(NO3N_data, na.rm = TRUE)
  mean_K <- mean(K_data, na.rm = TRUE)

  BQ1_smoothed[i, 2] <- mean_NH4N 
  BQ1_smoothed[i, 3] <- mean_Ca
  BQ1_smoothed[i, 4] <- mean_Mg
  BQ1_smoothed[i, 5] <- mean_NO3N
  BQ1_smoothed[i, 6] <- mean_K
}

BQ1_smoothed_plot <- BQ1_smoothed |> 
   pivot_longer(
    cols = c(NH4N_ugL, Ca_mgL, Mg_mgL, NO3N_ugL, K_mgL),
    names_to = "ion",
   values_to = "concentration")

  ggplot(
    data = BQ1_smoothed_plot,
    mapping = aes(
      x = window_start,
      y = concentration,
     color = ion
     )
  ) + 
  geom_point() +
    geom_line() +
    facet_wrap(~ion)





