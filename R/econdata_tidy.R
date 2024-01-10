null2NA <- function(x) if(is.null(x)) NA_character_ else x

econdata_make_label <- function(x, codelabel, meta) {
  m <- attr(x, "metadata")

  if (codelabel && !is.null(meta)) {
    lab <- NULL
    for (l in names(m)) {
      if(meta[[l]]$type == "#sdmx.infomodel.datastructure.Dimension") {
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
add_version_names <- function(x, elem = NULL) {
  if (is.null(elem)) {
    versions <- sapply(x, function(z) attr(z, "metadata")$version)
  } else {
    versions <- sapply(x, function(z) attr(z, "metadata")[[elem]]$version)
  }
  if(length(versions) == length(x) && !anyDuplicated(versions)) names(x) <- paste0("v", versions)
  return(x)
}

econdata_wide <- function(x, codelabel = FALSE, prettymeta = TRUE, ...) {
  if(is.null(names(x))) return(lapply(add_version_names(x), econdata_wide, codelabel, prettymeta))
  meta <- if(prettymeta) get_metadata(x) else NULL
  d <- unlist2d(x, "data_key", row.names = "period", DT = TRUE) |>
    dcast(period ~ data_key, value.var = "OBS_VALUE") |>
    fmutate(period = as.Date(period))
  labs <- sapply(x, econdata_make_label, codelabel, meta)
  nam <- names(d)[-1L]
  vlabels(d) <- c("Date", labs[1L, nam])
  vlabels(d, "source_identifier")[-1L] <- labs[2L, nam]
  attr(d, "metadata") <- attr(x, "metadata")
  return(qDT(d, keep.attr = TRUE))
}


econdata_extract_metadata <- function(x, allmeta, origmeta, meta) {
  if(!allmeta && length(x) == 0L) return(NULL) # Omits non-observed series.
  m <- attr(x, "metadata")
  if(origmeta) return(m)
  if (!is.null(meta)) {
    out <- list()
    for (p in names(m)) {
      cc_nam <- meta[[p]]$concept$name[[2]]
      cl <- meta[[p]]$codelist
      if(!is.null(cl)) {
        cl <- sapply(meta[[p]]$codelist$codes,
                     function(x) c(x[[2]]$id, x[[2]]$name[[2]]))
        out[[gsub(" ", "_", tolower(cc_nam))]] <- cl[2L, which(cl[1L, ] == m[[p]])]
      } else {
        out[[gsub(" ", "_", tolower(cc_nam))]] <- m[[p]]
      }
    }
    return(out)
  }
 return(m)
}

econdata_long <- function(x, combine = FALSE, allmeta = FALSE, origmeta = FALSE, prettymeta = TRUE, ...) {
  if(is.null(names(x))) {
    res <- lapply(add_version_names(x), econdata_long, combine, allmeta, origmeta, prettymeta)
    return(if(combine) rbindlist(res, use.names = TRUE, fill = TRUE) else res)
  }
  meta <- if(prettymeta) get_metadata(x) else NULL
  d <- unlist2d(x, "data_key", row.names = "period", DT = TRUE) |>
       fmutate(period = as.Date(period), data_key = qF(data_key)) |>
       frename(OBS_VALUE = "value")
  m <- attr(x, "metadata")
  meta <- lapply(x, econdata_extract_metadata, allmeta && !combine, origmeta, meta) |>
          rbindlist(use.names = TRUE, fill = TRUE)
  if(origmeta) names(meta) <- tolower(names(meta))
  meta$data_key <- if(allmeta && !combine) names(x) else names(x)[names(x) %in% levels(d$data_key)]
  meta$source <- null2NA(m$DataProvider[[2L]])
  meta$dataset <- null2NA(m$Dataflow[[2L]])
  meta$source_dataset <- null2NA(m$SOURCE_DATASET)
  meta$version <- null2NA(m$Dataflow[[3L]])
  setcolorder(meta, c("source", "dataset", "source_dataset", "version", "data_key"))
  if(!allmeta) get_vars(meta, fnobs(meta) == 0L) <- NULL
  if(combine) {
    meta_fct <- dapply(meta, qF, drop = FALSE) # Factors for efficient storage
    data_key <- d$data_key
    d$data_key <- NULL
    add_vars(d, "front") <- ss(meta_fct, ckmatch(data_key, meta_fct$data_key), check = FALSE)
    return(d)
  }
  return(list(data = d, metadata = meta))
}

# Tidying the output of read_release()
econdata_tidy_release <- function(x) {
  axnull <- is.null(attributes(x))
  if(axnull && length(x) > 1L) {
    res <- lapply(x, econdata_tidy_release)
    return(add_version_names(res, elem = "data-set"))
  }
  if(axnull) x <- x[[1L]]
  res <- rbindlist(lapply(x$releases, function(x)
                     { lapply (x, function(y) as.POSIXct(y)) }))
  attr(res, "metadata") <- x[["data-set"]]
  return(qDT(res, keep.attr = TRUE))
}

# This is just needed to get rid of the wide argument for documenting this together with read_econdata()
econdata_tidy_core <- function(x, wide = TRUE, is_release = FALSE, ...) {
  if (is_release) {
    econdata_tidy_release(x) 
  } else if (wide) {
    econdata_wide(x, ...) 
  } else {
    econdata_long(x, ...) 
  }
}

econdata_tidy <- function(x, ...) econdata_tidy_core(x, ...)
