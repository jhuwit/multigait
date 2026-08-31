test_that("Python requirements can be declared before Python starts", {
  expect_invisible(py_require_multigait())
})

test_that("the public module accessor imports MultiGait when available", {
  skip_if_not(identical(Sys.getenv("MULTIGAIT_RUN_PYTHON_TESTS"), "true"))
  skip_if_not(have_multigait())
  expect_true(reticulate::is_py_object(multigait_module()))
})
