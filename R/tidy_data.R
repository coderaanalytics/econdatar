# Tidy functions that return the data in tidy format ----

tidy_data <- function(x, ...) UseMethod("tidy_data", x)

tidy_data.eds_database <- function(x, ...) {
  dots <- list(...)
  if (all(sapply(x, function(x) length(x) == 0))) {
    lapply(x, function(y) {
      m <- attr(y, "metadata")
      m$name <- m$name[[2]]
      return(m[c("agencyid", "id", "version", "name")])
    }) |>
    rbindlist()
  } else if (dots$include_series) {
    lapply(x, function(y) {
      m <- attr(y, "metadata")
      return(data.frame(agencyid = m$agencyid,
                        id = m$id,
                        version = m$version,
                        name = m$name[[2]],
                        series = names(y)))
    }) |>
    do.call(rbind.data.frame, args = _)
  } else {
    if (dots$wide) {
      tidy_wide(x, ...)
    } else {
      tidy_long(x, ...)
    }
  }
}

tidy_data.eds_dataset <- function(x, wide = TRUE, ...) {
  if (wide) {
    tidy_wide(x, ...)
  } else {
    tidy_long(x, ...)
  }
}

tidy_data.eds_release <- function(x, ...) {
  axnull <- is.null(attributes(x))
  if (axnull && length(x) > 1L) {
    res <- lapply(x, tidy_data.eds_release)
    return(collect_version_names(res, is_release = TRUE))
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

tidy_data.eds_registry <- function(x, ...) {
  return(x)
}


# Utility functions ----

null2NA <- function(x) if (is.null(x)) NA_character_ else x

tidy_wide <- function(x, prettify = TRUE, ...) {
  dots <- list(...)
  if (!is.null(dots$combine) && dots$combine == TRUE) {
    warning("Combine parameter has no effect when parameter: wide = TRUE")
  }
  time_period <- NULL
  # Apply recursively in the case of multiple datasets
  if (inherits(x, "eds_database")) {
    names(x) <- collect_version_names(x)
    result <- lapply(x, tidy_wide, FALSE)
    return(result)
  }
  # Construct wide data.table
  if (all(sapply(x, function (y) nrow(y) == 0))) {
    stop("No observations returned, unable to format data tidy = TRUE")
  } else {
    d <- unlist2d(x, "series_key", row.names = "time_period", DT = TRUE) |>
      dcast(time_period ~ series_key, value.var = "OBS_VALUE") |>
      fmutate(time_period = as.Date(time_period))
  }
  # Construct and append metadata
  metadata <- if (prettify) get_metadata(x) else NULL
  labs <- sapply(x, make_label, metadata$concepts)
  n <- names(d)[-1L]
  vlabels(d) <- c("Date", labs[1L, n])
  vlabels(d, "source_identifier")[-1L] <- labs[2L, n]
  attr(d, "metadata") <- attr(x, "metadata")
  # Return data.table
  return(qDT(d, keep.attr = TRUE))
}

tidy_long <- function(x, prettify = TRUE, combine = FALSE, ...) {
  time_period <- NULL
  # Apply recursively in the case of multiple datasets
  if (inherits(x, "eds_database")) {
    names(x) <- collect_version_names(x)
    result <- lapply(x, tidy_long, FALSE, combine)
    if (combine) {
      return(rbindlist(result, use.names = TRUE, fill = TRUE))
    } else {
      return(result)
    }
  }
  # Construct long data.table
  if (all(sapply(x, function (y) nrow(y) == 0))) {
    stop("No observations returned, unable to format data tidy = TRUE")
  } else {
    d <- unlist2d(x, "series_key", row.names = "time_period", DT = TRUE) |>
      fmutate(time_period = as.Date(time_period),
              series_key = qF(series_key)) |>
      frename(OBS_VALUE = "obs_value")
  }
  # Construct and append metadata
  data_set_name <- attr(x, "metadata")$name[[2]]
  data_set_description <- attr(x, "metadata")$description[[2]]
  data_set_ref <- sprintf("%s:%s(%s)",
                          attr(x, "metadata")$agencyid,
                          attr(x, "metadata")$id,
                          attr(x, "metadata")$version)
  provision_agreement_ref <-
    sprintf("%s:%s(%s)",
            attr(x, "metadata")[["provision-agreement"]][[2]]$agencyid,
            attr(x, "metadata")[["provision-agreement"]][[2]]$id,
            attr(x, "metadata")[["provision-agreement"]][[2]]$version)
  if (prettify) {
    metadata <- get_metadata(x)
    meta <- extract_metadata(x, metadata$concepts)
    meta$data_set_name <- data_set_name
    if (!is.null(data_set_description)) {
      meta$data_set_description <- data_set_description
    }
    meta$data_set_ref <- data_set_ref
    meta$provision_agreement_ref <- provision_agreement_ref
    meta$data_provider_ref <- metadata$data_provider_ref
    col_order <- c("data_set_name",
                  "data_set_ref",
                  "provision_agreement_ref",
                  "data_provider_ref",
                  "series_key")
    if(combine) {
      meta$series_key <- names(x)[names(x) %in% levels(d$series_key)]
      nseries <- length(meta$series_key)
      meta <- lapply(x, extract_metadata, metadata$concepts) |>
        rbindlist(use.names = TRUE, fill = TRUE) |>
        c(lapply(meta, function(x) if (length(x) == 1) rep(x, nseries) else x))
      meta_fct <- dapply(meta, qF, drop = FALSE)
      series_key <- d$series_key
      d$series_key <- NULL
      add_vars(d, "front") <- ss(meta_fct,
                                 ckmatch(series_key, meta_fct$series_key),
                                 check = FALSE)
      names(d) <- tolower(names(d))
      setcolorder(d, col_order)
      result <- d
    } else {
      meta$series_key <- names(x)
      meta <- lapply(x, extract_metadata, metadata$concepts, FALSE) |>
        rbindlist(use.names = TRUE, fill = TRUE) |>
        c(meta)
      get_vars(meta, fnobs(meta) == 0L) <- NULL
      names(meta) <- tolower(names(meta))
      names(d) <- tolower(names(d))
      setcolorder(meta, col_order)
      result <- list(data = d, metadata = meta)
    }
  } else {
    meta <- attr(x, "metadata")
    get_vars(meta,
             c("name",
               "agencyid",
               "id",
               "version",
               "provision-agreement")) <- NULL
    meta$data_set_name <- data_set_name
    if (!is.null(data_set_description)) {
      meta$description <- NULL
      meta$data_set_description <- data_set_description
    }
    meta$data_set_ref <- data_set_ref
    meta$provision_agreement_ref <- provision_agreement_ref
    col_order <- c("data_set_name",
                   "data_set_ref",
                   "series_key")
    if (combine) {
      meta$series_key <- names(x)[names(x) %in% levels(d$series_key)]
      nseries <- length(meta$series_key)
      meta <- lapply(x, function(x) if (length(x) != 0) attr(x, "metadata") else NULL) |>
        rbindlist(use.names = TRUE, fill = TRUE) |>
        c(lapply(meta, function(x) if (length(x) == 1) rep(x, nseries) else x))
      meta_fct <- dapply(meta, qF, drop = FALSE)
      series_key <- d$series_key
      d$series_key <- NULL
      add_vars(d, "front") <- ss(meta_fct,
                                 ckmatch(series_key, meta_fct$series_key),
                                 check = FALSE)
      names(d) <- tolower(names(d))
      setcolorder(d, col_order)
      result <- d
    } else {
      meta$series_key <- names(x)
      meta <- lapply(x, function(x) attr(x, "metadata")) |>
        rbindlist(use.names = TRUE, fill = TRUE) |>
        c(meta)
      get_vars(meta, fnobs(meta) == 0L) <- NULL
      names(meta) <- tolower(names(meta))
      names(d) <- tolower(names(d))
      setcolorder(meta, col_order)
      result <- list(data = d, metadata = meta)
    }
  }
  return(result)
}

collect_version_names <- function(x, is_release = FALSE) {
  if (!is_release) {
    versions <- sapply(x, function(y) attr(y, "metadata")$version)
  } else {
    versions <- lapply(x, function(y) attr(y, "metadata")$version)
    names(versions) <- NULL
    versions <- unlist(versions)
  }
  if (length(versions) == length(x) && !anyDuplicated(versions)) {
    return(paste0("v", versions))
  } else {
    return(NULL)
  }
}

make_label <- function(x, meta = NULL) {
  # Create a label for each series key based on the available metadata
  m <- attr(x, "metadata")
  lab <- NULL
  if (!is.null(meta)) {
    for (l in names(m)) {
      if (meta[[l]]$type == "#sdmx.infomodel.datastructure.Dimension") {
        cl <- sapply(meta[[l]]$codelist$codes,
                     function(x) c(x[[2]]$id, x[[2]]$name[[2]]))
        lab <- c(lab, cl[2L, which(cl[1L, ] == m[[l]])])
      }
    }
    lab <- paste(lab, collapse = " - ")
  } else if (length(m$LABEL)) {
    lab <- m$LABEL
  } else {
    lab <- NA
  }
  return(c(lab, null2NA(m$SOURCE_IDENTIFIER)))
}

extract_metadata <- function(x, meta = NULL, drop.empty = TRUE) {
  # Replace metadata IDs with the name of the metadata structure
  m <- attr(x, "metadata")
  if (is.null(meta)) {
    return(m)
  } else if (drop.empty && length(x) == 0L) {
    return(NULL)
  } else {
    out <- list()
    for (p in names(m)) {
      if (!is.null(meta[[p]])) {
        cc_nam <- meta[[p]]$concept$name[[2]]
        cl <- meta[[p]]$codelist
        if (!is.null(cl)) {
          cl <- sapply(meta[[p]]$codelist$codes,
                       function(x) c(x[[2]]$id, x[[2]]$name[[2]]))
          out[[gsub(" ", "_", tolower(cc_nam))]] <-
            cl[2L, which(cl[1L, ] == m[[p]])]
        } else {
          out[[gsub(" ", "_", tolower(cc_nam))]] <- m[[p]]
        }
      }
    }
    return(out)
  }
}
