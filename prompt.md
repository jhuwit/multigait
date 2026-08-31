Wrap this python module https://github.com/DMegaritis/multigait for an R package that uses reticulate.
  Make sure to use the .onLoad with py_require_multigait() function.  make the py_require_multigait()
  function. Write this similarly to the packages we have done with actinet: https://github.com/jhuwit/actinet.  Make it user-friendly and well documented.  Add in vignettes, unit tests using testthat, and
  examples.  Use the usethis package when doing a number of operations.  Make sure roxygen2 is used for
  everything and that data is reused if possible from other packages such as actiread.  Ask any questions
  you  need answered prior to running a long running job to make code, but take all the time you will need
  to answer this question thoughtfully and fully.  Make sure a github action is created (using
  usethis::use_github_action()) that tests package coverage with Codecov.