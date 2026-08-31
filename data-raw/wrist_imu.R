## code to prepare `DATASET` dataset goes here
library(dplyr)
raw <- actiread::acti_read_gt3x(actiread::acti_example_gt3x())
# MultiGait expects acceleration in m/s^2 and axis names ending in _x/_y/_z.
wrist_imu <- raw |>
  mutate(
    acc_x = X * 9.80665,
    acc_y = Y * 9.80665,
    acc_z = Z * 9.80665
  )
usethis::use_data(wrist_imu, overwrite = TRUE)
