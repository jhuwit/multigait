test_that("Python helpers delegate to reticulate", {
  local_mocked_bindings(
    py_require = function(...) invisible(NULL),
    py_module_available = function(module) TRUE,
    import = function(module, convert = FALSE) fake_py(list(module = module, convert = convert)),
    dict = function(...) fake_py(list(...)),
    is_py_object = function(x) inherits(x, "fake_py"),
    r_to_py = function(x, convert) structure(x, converted = convert),
    py_has_attr = function(x, name) name %in% names(x),
    py_get_attr = function(x, name) x[[name]],
    py_to_r = identity,
    .package = "reticulate"
  )
  expect_invisible(py_require_multigait())
  expect_true(have_multigait())
  expect_equal(multigait_module(TRUE)$module, "multigait")
  expect_equal(multigait:::.as_python_data(1), 1)
  expect_s3_class(multigait:::.as_python_data(fake_py()), "fake_py")
  expect_equal(multigait:::.py_attribute(fake_py(list(a = 1)), "a"), 1)
  expect_null(multigait:::.py_attribute(fake_py(), "a"))
})

test_that("as_multigait_data creates the upstream-compatible object", {
  mock_multigait()
  local_mocked_bindings(.multigait_sample_rate = function(...) 100,
    .env = asNamespace("multigait"))
  local_mocked_bindings(
    py_run_string = function(...) NULL,
    py_eval = function(...) function(data, sample_rate, metadata, recording, id) {
      fake_py(list(data = data, sample_rate = sample_rate, id = id))
    },
    .package = "reticulate"
  )
  result <- as_multigait_data(data.frame(acc_is = 1), participant_id = "p01")
  expect_equal(result$sample_rate, 100)
  expect_equal(result$id, "p01")
  expect_equal(as_multigait_data(data.frame(acc_is = 1))$id, "unknown")
})
