#' Create a gait-sequence detector
#'
#' @param algorithm One of `"ionescu"`, `"kheirkhahan"`, `"maclean"`,
#'   `"hickey"`, or `"keren"`.
#' @param ... Arguments passed to the selected Python detector.
#' @return A Python MultiGait gait-sequence detector.
#' @export
gait_sequence_detector <- function(algorithm = c("ionescu", "kheirkhahan", "maclean", "hickey", "keren"), ...) {
  algorithm <- match.arg(algorithm)
  classes <- c(ionescu = "IonescuGSD", kheirkhahan = "KheirkhahanGSD",
               maclean = "MacLeanGSD", hickey = "HickeyGSD", keren = "KerenGSD")
  do.call(.multigait_import("GSD")[[classes[[algorithm]]]], list(...))
}

#' Detect gait sequences
#'
#' @param data Sensor samples as an R data frame or Python pandas data frame.
#' @param detector A detector returned by [gait_sequence_detector()].
#' @return A data frame of detected gait sequences.
#' @examples
#' component_imu <- wrist_imu[c("acc_x", "acc_y", "acc_z")]
#' names(component_imu) <- c("acc_is", "acc_ml", "acc_pa")
#' gait_sequences <- detect_gait_sequences(
#'   component_imu, gait_sequence_detector("ionescu", version = "wrist")
#' )
#' head(gait_sequences)
#' @export
detect_gait_sequences <- function(data, detector = gait_sequence_detector()) {
  result <- detector$detect(.as_python_data(data))
  .py_attribute(result, "gs_list_")
}

#' Create an initial-contact detector
#'
#' @param algorithm One of `"zijlstra"`, `"mccamley"`, `"pham"`,
#'   `"ducharme"`, `"gu"`, `"mico_amigo"`, or `"sassi"`.
#' @param ... Arguments passed to the selected Python detector.
#' @return A Python MultiGait initial-contact detector.
#' @export
initial_contact_detector <- function(algorithm = c("zijlstra", "mccamley", "pham", "ducharme", "gu", "mico_amigo", "sassi"), ...) {
  algorithm <- match.arg(algorithm)
  classes <- c(zijlstra = "ZijlstraIC", mccamley = "McCamleyIC", pham = "PhamIC",
               ducharme = "DucharmeIC", gu = "GuIC", mico_amigo = "MicoAmigoIC", sassi = "ICD6")
  do.call(.multigait_import("ICD")[[classes[[algorithm]]]], list(...))
}

#' Detect initial contacts
#'
#' @inheritParams detect_gait_sequences
#' @param detector An initial-contact detector from [initial_contact_detector()].
#' @return A data frame of initial contacts.
#' @examples
#' component_imu <- wrist_imu[c("acc_x", "acc_y", "acc_z")]
#' names(component_imu) <- c("acc_is", "acc_ml", "acc_pa")
#' initial_contacts <- detect_initial_contacts(
#'   component_imu, initial_contact_detector("zijlstra", version = "wrist")
#' )
#' head(initial_contacts)
#' @export
detect_initial_contacts <- function(data, detector = initial_contact_detector()) {
  result <- detector$detect(.as_python_data(data))
  .py_attribute(result, "ic_list_")
}

#' Calculate cadence from initial contacts
#'
#' @param data Sensor samples as an R data frame or Python pandas data frame.
#' @param initial_contacts An event data frame returned by
#'   [detect_initial_contacts()] for the same recording. It contains one row per
#'   detected initial contact (foot-strike); the required `ic` column gives the
#'   contact location as a sample index. Do not supply manually created times or
#'   contacts from a recording with a different `sample_rate`.
#' @param sample_rate Sampling frequency in Hertz. When `NULL`, it is obtained
#'   from `data` with [actibase::get_sample_rate()].
#' @param ... Arguments passed to the Python cadence constructor.
#' @return A data frame of per-second cadence estimates.
#' @examples
#' component_imu <- wrist_imu[c("acc_x", "acc_y", "acc_z")]
#' names(component_imu) <- c("acc_is", "acc_ml", "acc_pa")
#' initial_contacts <- detect_initial_contacts(
#'   component_imu, initial_contact_detector("zijlstra", version = "wrist")
#' )
#' cadence <- calculate_cadence(component_imu, initial_contacts, sample_rate = 100)
#' head(cadence)
#' @export
calculate_cadence <- function(data, initial_contacts, sample_rate = NULL, ...) {
  sample_rate <- .multigait_sample_rate(data, sample_rate)
  calculator <- do.call(.multigait_import("CAD")$Cadence, list(...))
  result <- calculator$calculate(.as_python_data(data),
    initial_contacts = .as_python_data(initial_contacts), sampling_rate_hz = sample_rate)
  .py_attribute(result, "cadence_per_sec_")
}

#' Create a stride-length calculator
#'
#' @param algorithm One of `"weinberg"`, `"bylemans"`, or `"kim"`.
#' @param ... Arguments passed to the selected Python calculator.
#' @return A Python MultiGait stride-length calculator.
#' @export
stride_length_calculator <- function(algorithm = c("weinberg", "bylemans", "kim"), ...) {
  algorithm <- match.arg(algorithm)
  classes <- c(weinberg = "WeinbergSL", bylemans = "BylemansSL", kim = "KimSL")
  do.call(.multigait_import("SL")[[classes[[algorithm]]]], list(...))
}

#' Calculate stride length from initial contacts
#'
#' @param data Sensor samples as an R data frame or Python pandas data frame.
#' @param initial_contacts Initial-contact data frame.
#' @param calculator A calculator returned by [stride_length_calculator()].
#' @param sample_rate Sampling frequency in Hertz. When `NULL`, it is obtained
#'   from `data` with [actibase::get_sample_rate()].
#' @param participant_metadata Named list of participant measurements passed to
#'   the selected Python calculator.
#' @return A data frame of per-second stride-length estimates.
#' @export
calculate_stride_length <- function(data, initial_contacts,
                                    calculator = stride_length_calculator(),
                                    sample_rate = NULL,
                                    participant_metadata = list()) {
  sample_rate <- .multigait_sample_rate(data, sample_rate)
  args <- c(list(.as_python_data(data),
    initial_contacts = .as_python_data(initial_contacts),
    sampling_rate_hz = sample_rate), participant_metadata)
  result <- do.call(calculator$calculate, args)
  .py_attribute(result, "stride_length_per_sec_")
}

#' Calculate walking speed from cadence and stride length
#'
#' @param data Optional sensor samples.
#' @param cadence Per-second cadence estimates.
#' @param stride_length Per-second stride-length estimates.
#' @param initial_contacts Optional initial-contact data frame.
#' @param sample_rate Sampling frequency in Hertz. When `NULL`, it is obtained
#'   from `data` with [actibase::get_sample_rate()]. Supplying it is required
#'   when `data` is `NULL`.
#' @return A data frame of per-second walking-speed estimates.
#' @export
calculate_walking_speed <- function(data = NULL, cadence, stride_length,
                                    initial_contacts = NULL, sample_rate = NULL) {
  sample_rate <- .multigait_sample_rate(data, sample_rate)
  result <- .multigait_import("WS")$Ws()$calculate(
    data = if (is.null(data)) NULL else .as_python_data(data),
    initial_contacts = if (is.null(initial_contacts)) NULL else .as_python_data(initial_contacts),
    cadence_per_sec = .as_python_data(cadence),
    stride_length_per_sec = .as_python_data(stride_length),
    sampling_rate_hz = sample_rate)
  .py_attribute(result, "walking_speed_per_sec_")
}

#' Interpolate sensor time series
#'
#' @param data A data frame, or list of data frames, to interpolate.
#' @param sample_rate Sampling frequency in Hertz. When `NULL`, it is obtained
#'   from `data` with [actibase::get_sample_rate()].
#' @param overlap_windows Whether source windows overlap.
#' @return A list of interpolated data frames.
#' @export
interpolate_multigait <- function(data, sample_rate = NULL, overlap_windows = FALSE) {
  sample_rate <- .multigait_sample_rate(data, sample_rate)
  out <- .multigait_import("interpolation_ts")$Interpolation()$interpolate(
    .as_python_data(data), sampling_rate_hz = sample_rate,
    overlap_windows = overlap_windows)
  reticulate::py_to_r(out)
}

#' Aggregate walking-bout digital mobility outcomes
#'
#' @param walking_bouts A walking-bout data frame.
#' @param type Aggregation scheme: `"generic"` for free-living summaries or
#'   `"laboratory"` for laboratory summaries.
#' @param mask Optional logical/data-frame validity mask.
#' @param ... Arguments passed to the selected Python aggregator.
#' @return An aggregated digital-mobility-outcome data frame.
#' @export
aggregate_walking_bouts <- function(walking_bouts, type = c("generic", "laboratory"),
                                    mask = NULL, ...) {
  type <- match.arg(type)
  aggregator <- if (type == "generic") {
    do.call(.multigait_import("aggregation")$GenericAggregator, list(...))
  } else {
    do.call(.multigait_import("aggregation")$LaboratoryAggregator, list(...))
  }
  result <- aggregator$aggregate(.as_python_data(walking_bouts),
    wb_dmos_mask = if (is.null(mask)) NULL else .as_python_data(mask))
  .py_attribute(result, "aggregated_data_")
}
