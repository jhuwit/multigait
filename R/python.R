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

.as_python_dict <- function(x, argument = deparse(substitute(x))) {
  if (reticulate::is_py_object(x)) return(x)
  if (!is.list(x) || (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop("`", argument, "` must be a named list or Python dictionary.", call. = FALSE)
  }
  do.call(reticulate::dict, x)
}

.multigait_sample_rate <- function(data, sample_rate = NULL) {
  if (is.null(sample_rate)) {
    if (reticulate::is_py_object(data)) {
      stop("Supply `sample_rate` when `data` is a Python object.", call. = FALSE)
    }
    sample_rate <- actibase::get_sample_rate(data)
  }
  if (!is.numeric(sample_rate) || length(sample_rate) != 1L ||
      is.na(sample_rate) || sample_rate <= 0) {
    stop("`sample_rate` must be one positive numeric value.", call. = FALSE)
  }
  as.numeric(sample_rate)
}

.multigait_prepare_data <- function(data, standardize = TRUE,
                                    scale = c("guess", "none", "error")) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or a Python data frame.", call. = FALSE)
  }
  scale <- match.arg(scale)
  names_lower <- tolower(names(data))
  xyz <- c("x", "y", "z")

  if (isTRUE(standardize) && all(xyz %in% names_lower)) {
    data <- actibase::acti_standardize_data(data, subset_xyz = FALSE,
      lower_case = TRUE)
    names_lower <- tolower(names(data))
  }

  # MultiGait converts names ending in _x/_y/_z to body-frame names. Make the
  # conventional actibase X/Y/Z columns explicit before handing off to Python.
  axis_locations <- match(xyz, names_lower)
  if (all(!is.na(axis_locations))) {
    names(data)[axis_locations] <- paste0("acc_", xyz)
  }

  acceleration_columns <- grep("^acc_(x|y|z)$", names(data), value = TRUE)
  if (scale == "none" || length(acceleration_columns) != 3L) return(data)

  values <- unlist(data[acceleration_columns], use.names = FALSE)
  values <- values[is.finite(values)]
  if (!length(values)) return(data)
  magnitude <- sqrt(rowSums(as.matrix(data[acceleration_columns])^2, na.rm = TRUE))
  typical_magnitude <- stats::median(magnitude[is.finite(magnitude)], na.rm = TRUE)
  looks_like_g <- is.finite(typical_magnitude) && typical_magnitude >= 0.5 &&
    typical_magnitude <= 2.5

  if (looks_like_g && scale == "error") {
    stop("Acceleration appears to be in g; set `scale = 'guess'` to convert it ",
      "to m/s^2 or `scale = 'none'` to keep the original units.", call. = FALSE)
  }
  if (looks_like_g) data[acceleration_columns] <- data[acceleration_columns] * 9.80665
  data
}

.py_attribute <- function(x, name) {
  if (!reticulate::py_has_attr(x, name)) return(NULL)
  reticulate::py_to_r(reticulate::py_get_attr(x, name))
}
