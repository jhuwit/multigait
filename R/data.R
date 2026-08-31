#' Create a MultiGait dataset from R data
#'
#' Creates the lightweight Python dataset object required by MultiGait's
#' pipelines. `data` should have one row per sample and body-frame accelerometer
#' columns (commonly `acc_is`, `acc_ml`, and `acc_pa`). The upstream pipeline
#' also recognises compatible axis names and renames them internally.
#'
#' @param data A data frame of sensor samples, or a compatible Python pandas
#'   data frame.
#' @param sampling_rate_hz Sampling frequency in Hertz.
#' @param participant_metadata Named list containing participant measurements.
#'   `height_m` is used by the default thresholding stage; add `foot_length_cm`,
#'   `leg_length_cm`, or `arm_length_cm` when required by a chosen algorithm.
#' @param recording_metadata Named list of recording-level metadata.
#' @param participant_id Optional identifier for the recording participant.
#' @return A Python `BaseGaitDataset` compatible object.
#' @export
as_multigait_data <- function(data, sampling_rate_hz = 100,
                              participant_metadata = list(),
                              recording_metadata = list(),
                              participant_id = NULL) {
  stopifnot(is.numeric(sampling_rate_hz), length(sampling_rate_hz) == 1L,
            sampling_rate_hz > 0)
  .multigait_import("data")
  reticulate::py_run_string(paste(
    "from multigait.data import BaseGaitDataset",
    "class _RMultiGaitDataset(BaseGaitDataset):",
    "    def __init__(self, data_ss, sampling_rate_hz, participant_metadata, recording_metadata, participant_id):",
    "        self.data_ss = data_ss",
    "        self.sampling_rate_hz = sampling_rate_hz",
    "        self.participant_metadata = participant_metadata",
    "        self.recording_metadata = recording_metadata",
    "        self.participant_id = participant_id",
    "    @property",
    "    def group_label(self):",
    "        return ()",
    sep = "\n"), local = FALSE)
  klass <- reticulate::py_eval("_RMultiGaitDataset", convert = FALSE)
  klass(.as_python_data(data), sampling_rate_hz,
        .as_python_data(participant_metadata), .as_python_data(recording_metadata),
        participant_id %||% "unknown")
}

`%||%` <- function(x, y) if (is.null(x)) y else x
