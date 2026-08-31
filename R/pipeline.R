#' Create the healthy-comorbidity MultiGait pipeline
#'
#' Returns the upstream predefined pipeline for the healthier comorbidity
#' configuration: Kheirkhahan gait-sequence detection, McCamley initial-contact
#' detection, Weinberg stride length, cadence, and walking speed.
#'
#' @return A Python `MultiGaitPipelineHealthyCoMorbidity` object.
#' @examples
#' if (interactive() && have_multigait()) healthy_comorbidity_pipeline()
#' @export
healthy_comorbidity_pipeline <- function() {
  .multigait_import("pipeline")$MultiGaitPipelineHealthyCoMorbidity()
}

#' Create the multimorbidity-impaired MultiGait pipeline
#'
#' Returns the upstream predefined pipeline for multimorbidity-impaired
#' populations. It combines Ionescu gait-sequence detection, Zijlstra initial
#' contacts, Pham contacts for reported stride length, adaptive Weinberg stride
#' length, cadence, and walking speed.
#'
#' @return A Python `MultiGaitPipelineMultimorbidityImpaired` object.
#' @examples
#' if (interactive() && have_multigait()) multimorbidity_impaired_pipeline()
#' @export
multimorbidity_impaired_pipeline <- function() {
  .multigait_import("pipeline")$MultiGaitPipelineMultimorbidityImpaired()
}

#' Run a MultiGait pipeline
#'
#' @param data A dataset created by [as_multigait_data()] or a compatible
#'   Python dataset object.
#' @param pipeline A MultiGait pipeline, by default the
#'   [multimorbidity_impaired_pipeline()].
#' @param safe If `TRUE`, use the upstream `safe_run()` method.
#' @return A named list containing `pipeline`, `gait_sequences`,
#'   `initial_contacts`, `per_second`, `per_stride`, `per_walking_bout`, and
#'   `aggregated` results. Data frames are converted to R.
#' @examples
#' \dontrun{
#' recording <- as_multigait_data(wrist_imu, sampling_rate_hz = 100)
#' run_multimorbidity_impaired_pipeline(recording)
#' }
#' @export
run_multigait <- function(data, pipeline = multimorbidity_impaired_pipeline(),
                          safe = TRUE) {
  if (!reticulate::is_py_object(pipeline)) {
    stop("`pipeline` must be a MultiGait Python pipeline object.", call. = FALSE)
  }
  runner <- if (isTRUE(safe)) pipeline$safe_run else pipeline$run
  result <- runner(data)
  list(
    pipeline = result,
    gait_sequences = .py_attribute(result, "gs_list_"),
    initial_contacts = .py_attribute(result, "raw_ic_list_"),
    per_second = .py_attribute(result, "raw_per_sec_parameters_"),
    per_stride = .py_attribute(result, "per_stride_parameters_"),
    per_walking_bout = .py_attribute(result, "per_wb_parameters_"),
    aggregated = .py_attribute(result, "aggregated_parameters_")
  )
}

#' Run the healthy-comorbidity pipeline
#'
#' @inheritParams run_multigait
#' @return The result list returned by [run_multigait()].
#' @export
run_healthy_comorbidity_pipeline <- function(data, safe = TRUE) {
  run_multigait(data, healthy_comorbidity_pipeline(), safe = safe)
}

#' Run the multimorbidity-impaired pipeline
#'
#' @inheritParams run_multigait
#' @return The result list returned by [run_multigait()].
#' @export
run_multimorbidity_impaired_pipeline <- function(data, safe = TRUE) {
  run_multigait(data, multimorbidity_impaired_pipeline(), safe = safe)
}
