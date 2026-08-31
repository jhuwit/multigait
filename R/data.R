#' Create a MultiGait dataset from R data
#'
#' Creates the lightweight Python dataset object required by MultiGait's
#' pipelines. `data` should have one row per sample and body-frame accelerometer
#' columns ending in `_x`, `_y`, and `_z` (for example `acc_x`, `acc_y`, and
#' `acc_z`). The upstream pipeline renames these to its body-frame convention
#' (`acc_is`, `acc_ml`, and `acc_pa`). Acceleration must be expressed in m/s^2.
#'
#' @param data A data frame of sensor samples, or a compatible Python pandas
#'   data frame.
#' @param sample_rate Sampling frequency in Hertz. When `NULL`, the sampling
#'   rate is obtained with [actibase::get_sample_rate()].
#' @param standardize Logical; should compatible R data be standardized with
#'   [actibase::acti_standardize_data()]? Defaults to `TRUE`. Standardization is
#'   attempted for data containing `X`, `Y`, and `Z` columns and produces the
#'   `acc_x`, `acc_y`, and `acc_z` names expected by MultiGait.
#' @param scale How to handle likely acceleration units: `"guess"` (the
#'   default) converts values that appear to be in g to m/s^2, `"none"` leaves
#'   values unchanged, and `"error"` stops instead of applying an inferred
#'   conversion. Use `"none"` or `"error"` when units are known.
#' @param participant_metadata Named list containing participant measurements.
#'   `height_m` is used by the default thresholding stage; add `foot_length_cm`,
#'   `leg_length_cm`, or `arm_length_cm` when required by a chosen algorithm.
#' @param recording_metadata Named list of recording-level metadata.
#' @param participant_id Optional identifier for the recording participant.
#' @return A Python `BaseGaitDataset` compatible object.
#' @export
as_multigait_data <- function(data, sample_rate = NULL,
                              standardize = TRUE,
                              scale = c("guess", "none", "error"),
                              participant_metadata = list(),
                              recording_metadata = list(),
                              participant_id = NULL) {
  scale <- match.arg(scale)
  if (!reticulate::is_py_object(data)) {
    data <- .multigait_prepare_data(data, standardize = standardize, scale = scale)
  }
  sample_rate <- .multigait_sample_rate(data, sample_rate)
  .multigait_import("data")
  reticulate::py_run_string(paste(
    "from multigait.data import BaseGaitDataset",
    "import pandas as pd",
    "class _RMultiGaitDataset(BaseGaitDataset):",
    "    def __init__(self, data_ss, sampling_rate_hz, participant_metadata, recording_metadata, participant_id, groupby_cols=None, subset_index=None):",
    "        self.participant_id = participant_id",
    "        super().__init__(groupby_cols=groupby_cols, subset_index=subset_index)",
    "        self.data_ss = data_ss",
    "        self.sampling_rate_hz = sampling_rate_hz",
    "        self.participant_metadata = participant_metadata",
    "        self.recording_metadata = recording_metadata",
    "    def create_index(self):",
    "        return pd.DataFrame({'participant_id': [self.participant_id]})",
    "    @property",
    "    def group_label(self):",
    "        return ()",
    sep = "\n"), local = FALSE)
  klass <- reticulate::py_eval("_RMultiGaitDataset", convert = FALSE)
  klass(.as_python_data(data), sample_rate,
        .as_python_dict(participant_metadata, "participant_metadata"),
        .as_python_dict(recording_metadata, "recording_metadata"),
        participant_id %||% "unknown")
}

`%||%` <- function(x, y) if (is.null(x)) y else x
