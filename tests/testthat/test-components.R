test_that("component constructors reject unknown algorithms before Python is used", {
  expect_error(gait_sequence_detector("not-an-algorithm"))
  expect_error(initial_contact_detector("not-an-algorithm"))
  expect_error(stride_length_calculator("not-an-algorithm"))
})
