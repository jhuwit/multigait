test_that("as_multigait_data validates the sampling rate before Python is used", {
  expect_error(as_multigait_data(data.frame(acc_is = 1), sampling_rate_hz = 0),
               "sampling_rate_hz")
})

test_that("R sensor data are adapted when Python integration is enabled", {
  skip_if_not(identical(Sys.getenv("MULTIGAIT_RUN_PYTHON_TESTS"), "true"))
  data <- data.frame(acc_is = c(0, 1), acc_ml = c(0, 1), acc_pa = c(9.81, 9.81))
  dataset <- as_multigait_data(data, participant_metadata = list(height_m = 1.7))
  expect_true(reticulate::is_py_object(dataset))
})

test_that("pipeline functions validate R-side inputs", {
  expect_error(run_multigait(data = NULL, pipeline = list()), "pipeline")
})
