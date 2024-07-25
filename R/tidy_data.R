null2NA <- function(x) if (is.null(x)) NA_character_ else x

make_label <- function(x, codelabel, meta) {
  m <- attr(x, "metadata")
  if (codelabel && !is.null(meta)) {
    lab <- NULL
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

# (Optional) list names for multi-version calls
add_version_names <- function(x, is_release = FALSE) {
  if (!is_release) {
    versions <- sapply(x, function(z) attr(z, "metadata")$version)
  } else {
    versions <- sapply(x, function(z) attr(z, "metadata")[[2]]$version)
  }
  if (length(versions) == length(x) && !anyDuplicated(versions)) {
    names(x) <- paste0("v", versions)
  }
  return(x)
}

tidy_wide <- function(x, codelabel = FALSE, prettymeta = TRUE, ...) {
  if (is.null(names(x))) return(lapply(add_version_names(x),
                                       tidy_wide,
                                       codelabel,
                                       prettymeta))
  metadata <- if (prettymeta) get_metadata(x) else NULL
  d <- unlist2d(x, "series_key", row.names = "time_period", DT = TRUE) |>
    dcast(time_period ~ series_key, value.var = "OBS_VALUE") |>
    fmutate(time_period = as.Date(time_period))
  labs <- sapply(x, make_label, codelabel, metadata$concepts)
  nam <- names(d)[-1L]
  vlabels(d) <- c("Date", labs[1L, nam])
  vlabels(d, "source_identifier")[-1L] <- labs[2L, nam]
  attr(d, "metadata") <- attr(x, "metadata")
  return(qDT(d, keep.attr = TRUE))
}


extract_metadata <- function(x, meta, allmeta = FALSE, origmeta = FALSE) {
  if (!allmeta && length(x) == 0L) return(NULL) # Omits non-observed series.
  m <- attr(x, "metadata")
  if (origmeta) return(m)
  if (!is.null(meta)) {
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
  return(m)
}

tidy_long <- function(x, combine = FALSE, allmeta = FALSE, origmeta = FALSE, prettymeta = TRUE, ...) {
  if (is.null(names(x))) {
    res <- lapply(add_version_names(x),
                  tidy_long,
                  combine,
                  allmeta,
                  origmeta,
                  prettymeta)
    return(if (combine) rbindlist(res, use.names = TRUE, fill = TRUE) else res)
  }
  metadata <- if (prettymeta) get_metadata(x) else NULL
  d <- unlist2d(x, "series_key", row.names = "time_period", DT = TRUE) |>
    fmutate(time_period = as.Date(time_period),
            series_key = qF(series_key)) |>
    frename(OBS_VALUE = "obs_value")
  M <- attr(x, "metadata")
  meta4dataset <- extract_metadata(x,
                                   metadata$concepts,
                                   allmeta && !combine,
                                   origmeta)
  meta <- lapply(x,
                 extract_metadata,
                 metadata$concepts,
                 allmeta && !combine,
                 origmeta) |>
    rbindlist(use.names = TRUE, fill = TRUE)
  if (origmeta) names(meta) <- tolower(names(meta))
  if (allmeta && !combine)  {
    meta <- c(meta4dataset, meta)
    meta$data_set_name <- M$name[[2]]
    meta$data_set_ref <- sprintf("%s:%s(%s)", M$agencyid, M$id, M$version)
    meta$data_provider_ref <- metadata$data_provider_ref
    meta$series_key <- names(x)
  } else {
    nseries <- length(levels(d$series_key))
    meta <- c(lapply(meta4dataset,
                     function(x) if (length(x) == 1) rep(x, nseries) else x),
              meta)
    meta$data_set_name <- rep(M$name[[2]], nseries)
    meta$data_set_ref <- rep(sprintf("%s:%s(%s)",
                                     M$agencyid,
                                     M$id, M$version),
                             nseries)
    meta$data_provider_ref <- rep(metadata$data_provider_ref, nseries)
    meta$series_key <- names(x)[names(x) %in% levels(d$series_key)]
  }
  if (prettymeta) {
    setcolorder(meta,
                c("data_set_name",
                  "data_set_ref",
                  "data_provider_ref",
                  "series_key"))
  } else {
    setcolorder(meta,
                c("data_set_name",
                  "data_set_ref",
                  "series_key"))
    meta[["name"]] <- NULL
    meta[["provision-agreement"]] <- NULL
  }
  if (!allmeta) get_vars(meta, fnobs(meta) == 0L) <- NULL
  if (combine) {
    meta_fct <- dapply(meta, qF, drop = FALSE) # Factors for efficient storage
    series_key <- d$series_key
    d$series_key <- NULL
    add_vars(d, "front") <- ss(meta_fct,
                               ckmatch(series_key, meta_fct$series_key),
                               check = FALSE)
    return(d)
  }
  return(list(data = d, metadata = meta))
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
  lapply(x, function(y) {
    m <- attr(y, "metadata")
    m$name <- m$name[[2]]
    m$description <- m$description[[2]]
    return(m[c("agencyid", "id", "version", "name")])
  }) |>
    rbindlist()
}

tidy_data.eds_registry <- function(x, ...) {
  return(x)
}

tidy_data <- function(x, ...) UseMethod("tidy_data", x)
