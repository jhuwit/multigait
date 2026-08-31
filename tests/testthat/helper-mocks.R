fake_py <- function(x = list()) structure(x, class = "fake_py")

fake_result <- function(name, value = data.frame(value = 1)) {
  fake_py(stats::setNames(list(value), name))
}

fake_detector <- function(name) fake_py(list(
  detect = function(data) fake_result(name)
))

fake_calculator <- function(name) fake_py(list(
  calculate = function(...) fake_result(name)
))

fake_aggregator <- function() fake_py(list(
  aggregate = function(...) fake_result("aggregated_data_")
))

fake_pipeline <- function() {
  output <- fake_py(list(
    gs_list_ = data.frame(gs = 1), raw_ic_list_ = data.frame(ic = 1),
    raw_per_sec_parameters_ = data.frame(cadence = 1),
    per_stride_parameters_ = data.frame(stride = 1),
    per_wb_parameters_ = data.frame(wb = 1),
    aggregated_parameters_ = data.frame(total = 1)
  ))
  output$safe_run <- function(data) output
  output$run <- function(data) output
  output
}

fake_multigait_modules <- function() {
  gsd <- stats::setNames(rep(list(function(...) fake_detector("gs_list_")), 5),
    c("IonescuGSD", "KheirkhahanGSD", "MacLeanGSD", "HickeyGSD", "KerenGSD"))
  icd <- stats::setNames(rep(list(function(...) fake_detector("ic_list_")), 7),
    c("ZijlstraIC", "McCamleyIC", "PhamIC", "DucharmeIC", "GuIC", "MicoAmigoIC", "ICD6"))
  list(
    GSD = gsd,
    ICD = icd,
    CAD = list(Cadence = function(...) fake_calculator("cadence_per_sec_")),
    SL = list(WeinbergSL = function(...) fake_calculator("stride_length_per_sec_"),
      BylemansSL = function(...) fake_calculator("stride_length_per_sec_"),
      KimSL = function(...) fake_calculator("stride_length_per_sec_")),
    WS = list(Ws = function(...) fake_calculator("walking_speed_per_sec_")),
    interpolation_ts = list(Interpolation = function(...) fake_py(list(
      interpolate = function(...) list(data.frame(value = 1))
    ))),
    aggregation = list(
      GenericAggregator = function(...) fake_aggregator(),
      LaboratoryAggregator = function(...) fake_aggregator()
    ),
    pipeline = list(
      MultiGaitPipelineHealthyCoMorbidity = function(...) fake_pipeline(),
      MultiGaitPipelineMultimorbidityImpaired = function(...) fake_pipeline()
    )
  )
}

mock_multigait <- function() {
  modules <- fake_multigait_modules()
  local_mocked_bindings(
    .multigait_import = function(module) modules[[module]],
    .as_python_data = function(x) x,
    .as_python_dict = function(x, ...) x,
    .py_attribute = function(x, name) x[[name]],
    .env = asNamespace("multigait")
  )
}
