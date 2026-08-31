test_that("as_multigait_data validates the sample rate before Python is used", {
  expect_error(as_multigait_data(data.frame(acc_is = 1), sample_rate = 0),
               "sample_rate")
})

test_that("a missing sample rate is obtained through actibase", {
  local_mocked_bindings(get_sample_rate = function(data) 80, .package = "actibase")
  expect_equal(multigait:::.multigait_sample_rate(data.frame(x = 1), NULL), 80)
})

test_that("actibase data preparation standardizes and controls unit scaling", {
  raw <- data.frame(X = c(0, 0), Y = c(0, 0), Z = c(1, 1))
  standardized <- multigait:::.multigait_prepare_data(raw)
  expect_named(standardized, c("acc_x", "acc_y", "acc_z"))
  expect_equal(standardized$acc_z, rep(9.80665, 2))
  expect_equal(multigait:::.multigait_prepare_data(raw, scale = "none")$acc_z,
    rep(1, 2))
  expect_error(multigait:::.multigait_prepare_data(raw, scale = "error"), "appears")
})

test_that("R sensor data are adapted when Python integration is enabled", {
  skip_if_not(identical(Sys.getenv("MULTIGAIT_RUN_PYTHON_TESTS"), "true"))
  data <- data.frame(acc_is = c(0, 1), acc_ml = c(0, 1), acc_pa = c(9.81, 9.81))
  dataset <- as_multigait_data(data, sample_rate = 100,
    participant_metadata = list(height_m = 1.7))
  expect_true(reticulate::is_py_object(dataset))
  expect_silent(reticulate::py_repr(dataset))
})

test_that("pipeline functions validate R-side inputs", {
  expect_error(run_multigait(data = NULL, pipeline = list()), "pipeline")
})
