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
#' \dontrun{detect_gait_sequences(wrist_imu)}
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
#' \dontrun{detect_initial_contacts(wrist_imu)}
#' @export
detect_initial_contacts <- function(data, detector = initial_contact_detector()) {
  result <- detector$detect(.as_python_data(data))
  .py_attribute(result, "ic_list_")
}

#' Calculate cadence from initial contacts
#'
#' @param data Sensor samples as an R data frame or Python pandas data frame.
#' @param initial_contacts Initial-contact data frame.
#' @param sampling_rate_hz Sampling frequency in Hertz.
#' @param ... Arguments passed to the Python cadence constructor.
#' @return A data frame of per-second cadence estimates.
#' @examples
#' \dontrun{calculate_cadence(wrist_imu, initial_contacts)}
#' @export
calculate_cadence <- function(data, initial_contacts, sampling_rate_hz = 100, ...) {
  calculator <- do.call(.multigait_import("CAD")$Cadence, list(...))
  result <- calculator$calculate(.as_python_data(data),
    initial_contacts = .as_python_data(initial_contacts), sampling_rate_hz = sampling_rate_hz)
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
#' @param sampling_rate_hz Sampling frequency in Hertz.
#' @param participant_metadata Named list of participant measurements passed to
#'   the selected Python calculator.
#' @return A data frame of per-second stride-length estimates.
#' @export
calculate_stride_length <- function(data, initial_contacts,
                                    calculator = stride_length_calculator(),
                                    sampling_rate_hz = 100,
                                    participant_metadata = list()) {
  args <- c(list(.as_python_data(data),
    initial_contacts = .as_python_data(initial_contacts),
    sampling_rate_hz = sampling_rate_hz), participant_metadata)
  result <- do.call(calculator$calculate, args)
  .py_attribute(result, "stride_length_per_sec_")
}

#' Calculate walking speed from cadence and stride length
#'
#' @param data Optional sensor samples.
#' @param cadence Per-second cadence estimates.
#' @param stride_length Per-second stride-length estimates.
#' @param initial_contacts Optional initial-contact data frame.
#' @param sampling_rate_hz Sampling frequency in Hertz.
#' @return A data frame of per-second walking-speed estimates.
#' @export
calculate_walking_speed <- function(data = NULL, cadence, stride_length,
                                    initial_contacts = NULL, sampling_rate_hz = 100) {
  result <- .multigait_import("WS")$Ws()$calculate(
    data = if (is.null(data)) NULL else .as_python_data(data),
    initial_contacts = if (is.null(initial_contacts)) NULL else .as_python_data(initial_contacts),
    cadence_per_sec = .as_python_data(cadence),
    stride_length_per_sec = .as_python_data(stride_length),
    sampling_rate_hz = sampling_rate_hz)
  .py_attribute(result, "walking_speed_per_sec_")
}

#' Interpolate sensor time series
#'
#' @param data A data frame, or list of data frames, to interpolate.
#' @param sampling_rate_hz Sampling frequency in Hertz.
#' @param overlap_windows Whether source windows overlap.
#' @return A list of interpolated data frames.
#' @export
interpolate_multigait <- function(data, sampling_rate_hz = 100, overlap_windows = FALSE) {
  out <- .multigait_import("interpolation_ts")$Interpolation()$interpolate(
    .as_python_data(data), sampling_rate_hz = sampling_rate_hz,
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
