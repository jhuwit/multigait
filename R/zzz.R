#' Declare the Python dependency used by multigait
#'
#' Registers the Python package requirement used by this R package. This is
#' called from [`.onLoad()`], before Python is initialized, so recent versions
#' of reticulate can create a managed environment automatically.
#'
#' @param python_version Python version requested by reticulate. MultiGait is
#'   tested with Python 3.10.
#' @return Invisibly, the registered requirement.
#' @examples
#' py_require_multigait()
#' @export
py_require_multigait <- function(python_version = "3.10") {
  reticulate::py_require("multigait", python_version = python_version)
  invisible("multigait")
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  py_require_multigait()
}
