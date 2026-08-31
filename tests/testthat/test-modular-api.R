test_that("all modular wrappers return their documented R results", {
  mock_multigait()
  data <- data.frame(acc_is = 1, acc_ml = 1, acc_pa = 1)
  local_mocked_bindings(.multigait_sample_rate = function(...) 100,
    .env = asNamespace("multigait"))

  expect_s3_class(gait_sequence_detector("keren"), "fake_py")
  expect_s3_class(initial_contact_detector("gu"), "fake_py")
  expect_s3_class(stride_length_calculator("kim"), "fake_py")
  expect_s3_class(detect_gait_sequences(data), "data.frame")
  contacts <- detect_initial_contacts(data)
  expect_s3_class(contacts, "data.frame")
  expect_s3_class(calculate_cadence(data, contacts), "data.frame")
  expect_s3_class(calculate_stride_length(data, contacts), "data.frame")
  expect_s3_class(calculate_walking_speed(data, contacts, contacts), "data.frame")
  expect_type(interpolate_multigait(data), "list")
  expect_s3_class(aggregate_walking_bouts(data.frame(wb_id = 1)), "data.frame")
  expect_s3_class(aggregate_walking_bouts(data.frame(wb_id = 1), "laboratory"), "data.frame")
})

test_that("pipeline wrappers expose every result table", {
  mock_multigait()
  local_mocked_bindings(is_py_object = function(x) inherits(x, "fake_py"), .package = "reticulate")
  pipeline <- multimorbidity_impaired_pipeline()
  expect_s3_class(healthy_comorbidity_pipeline(), "fake_py")
  result <- run_multigait(data.frame(x = 1), pipeline, safe = TRUE)
  expect_named(result, c("pipeline", "gait_sequences", "initial_contacts", "per_second",
    "per_stride", "per_walking_bout", "aggregated"))
  expect_s3_class(run_multigait(data.frame(x = 1), pipeline, safe = FALSE)$aggregated, "data.frame")
  expect_s3_class(run_healthy_comorbidity_pipeline(data.frame(x = 1))$aggregated, "data.frame")
  expect_s3_class(run_multimorbidity_impaired_pipeline(data.frame(x = 1))$aggregated, "data.frame")
})
