null2NA <- function(x) if (is.null(x)) NA_character_ else x

make_label <- function(x, codelabel, meta) {
}

# (Optional) list names for multi-version calls
add_version_names <- function(x, is_release = FALSE) {
}

tidy_wide <- function(x, codelabel = FALSE, prettymeta = TRUE, ...) {
}


extract_metadata <- function(x, meta, allmeta = FALSE, origmeta = FALSE) {
}

tidy_long <- function(x, combine = FALSE, allmeta = FALSE, origmeta = FALSE, prettymeta = TRUE, ...) {
}

# Tidying the output of read_release()
tidy_data.eds_release <- function(x, ...) {
  axnull <- is.null(attributes(x))
  if (axnull && length(x) > 1L) {
    res <- lapply(x, tidy_data.eds_release)
    return(add_version_names(res, is_release = TRUE))
  }
  res <-
    rbindlist(lapply(x$releases, function(x) {
      list("release" = as.POSIXct(x[["release"]]),
           "start-period" = as.POSIXct(x[["start-period"]]),
           "end-period" = as.POSIXct(x[["end-period"]]),
           "description" = x[["description"]])
    }
    ))
  attr(res, "metadata") <- x[["data-set"]]
  return(qDT(res, keep.attr = TRUE))
}

# This is just needed to get rid of the wide argument for documenting this
# together with read_econdata()
tidy_data.eds_dataset <- function(x, wide = TRUE, ...) {
  if (wide) {
    tidy_wide(x, ...)
  } else {
    tidy_long(x, ...)
  }
}

tidy_data.eds_database <- function(x, ...) {
}

tidy_data.eds_registry <- function(x, ...) {
  return(x)
}

tidy_data <- function(x, ...) UseMethod("tidy_data", x)
