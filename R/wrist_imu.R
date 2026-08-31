#' Example wrist-worn accelerometer recording
#'
#' A wrist-worn ActiGraph GT3X recording provided by `actiread`, prepared for
#' use with MultiGait. The source acceleration channels are retained in g
#' (`X`, `Y`, and `Z`), and the corresponding `acc_x`, `acc_y`, and `acc_z`
#' columns are supplied in m/s^2 for MultiGait. The recording is sampled at
#' 100 Hz and contains 240,500 observations.
#'
#' @format A data frame with 240,500 rows and 7 variables:
#' \describe{
#'   \item{time}{Sample timestamp (`POSIXct`).}
#'   \item{X, Y, Z}{Original tri-axial acceleration channels in g.}
#'   \item{acc_x, acc_y, acc_z}{Tri-axial acceleration channels in m/s^2,
#'   named to be converted to MultiGait's body-frame convention.}
#' }
#'
#' @source `actiread::acti_example_gt3x()`, read with
#'   `actiread::acti_read_gt3x()` and transformed in
#'   `data-raw/wrist_imu.R`.
#' @examples
#' head(wrist_imu)
#' attr(wrist_imu, "sample_rate")
"wrist_imu"
