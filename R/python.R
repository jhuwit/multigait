#' Test whether MultiGait is available
#'
#' @return A single logical value. `FALSE` is returned when Python or the
#'   `multigait` Python package cannot be initialized.
#' @examples
#' have_multigait()
#' @export
have_multigait <- function() {
  suppressWarnings(isTRUE(tryCatch({
    py_require_multigait()
    reticulate::py_module_available("multigait")
  }, error = function(e) FALSE)))
}

#' Import the MultiGait Python package
#'
#' @param convert Should reticulate convert Python objects to R objects?
#' @return The imported Python module.
#' @export
multigait_module <- function(convert = FALSE) {
  py_require_multigait()
  reticulate::import("multigait", convert = convert)
}

.multigait_import <- function(module) {
  py_require_multigait()
  reticulate::import(paste0("multigait.", module), convert = FALSE)
}

.as_python_data <- function(x) {
  if (reticulate::is_py_object(x)) return(x)
  reticulate::r_to_py(x, convert = TRUE)
}

.py_attribute <- function(x, name) {
  if (!reticulate::py_has_attr(x, name)) return(NULL)
  reticulate::py_to_r(reticulate::py_get_attr(x, name))
}
