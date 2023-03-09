

unit_mult_switch <- function(x) switch(x, "3" = " Thousand", "6" = " Million", "9" = " Billion", "12" = " Trillion", x)

# http://www.southafrica-canada.ca/south-africas-nine-provinces/
province_switch <- function(x) switch(x,
      WC = "Western Cape", EC = "Eastern Cape", FS = "Free State", GP = "Gauteng", KZN = "KwaZulu-Natal",
      LM = "Limpopo", MP = "Mpumalanga", NC = "Northern Cape", NW = "North West",
      AU = "All urban areas", TC = "Total country", PU = "Primary urban", SU = "Secondary Urban", x)


# Check using app: https://www.econdata.co.za/app
econdata_make_label <- function(x, codelabel) {
  m <- attr(x, "metadata")
  PROVINCE <- if(length(m$PROVINCE)) m$PROVINCE else m$REGION

  lab <- paste0(if(codelabel && length(m$SOURCE_IDENTIFIER)) paste0(m$SOURCE_IDENTIFIER, ":= ") else "",
                m$LABEL,
                if(length(m$COMMENT) && nchar(m$COMMENT) < 80L) paste0(": ", m$COMMENT) else "",
                if(length(PROVINCE)) paste0(": ", province_switch(PROVINCE)) else "",
                if(length(m$DISTRICT)) paste0(": ", m$DISTRICT) else ""," (",
                m$UNIT_MEASURE,
                if(length(m$UNIT_MULT)) unit_mult_switch(m$UNIT_MULT) else "",
                if(length(m$BASE_PER)) paste(", Base =", m$BASE_PER) else "",
                if(length(m$SEASONAL_ADJUST) && m$SEASONAL_ADJUST == "S") ", Seasonally Adjusted)" else ")")

  lab <- c(lab, if(length(m$SOURCE_IDENTIFIER)) m$SOURCE_IDENTIFIER else NA_character_)
  return(lab)
}

# (Optional) list names for multi-version calls
add_version_names <- function(x, elem = "Dataflow") {
  versions <- sapply(x, function(z) attr(z, "metadata")[[elem]][[3L]])
  if(length(versions) == length(x) && !anyDuplicated(versions)) names(x) <- paste0("v", versions)
  return(x)
}

econdata_wide <- function(x, codelabel = FALSE, ...) {
  if(is.null(attributes(x))) return(lapply(add_version_names(x), econdata_wide, codelabel))
  d <- unlist2d(x, "code", row.names = "date", DT = TRUE) |>
    dcast(date ~ code, value.var = "OBS_VALUE") |>
    fmutate(date = as.Date(date))
  labs <- sapply(x, econdata_make_label, codelabel)
  nam <- names(d)[-1L]
  vlabels(d) <- c("Date", labs[1L, nam])
  vlabels(d, "source.code")[-1L] <- labs[2L, nam]
  attr(d, "metadata") <- attr(x, "metadata")
  return(qDT(d, keep.attr = TRUE))
}

null2NA <- function(x) if(is.null(x)) NA_character_ else x

econdata_extract_metadata <- function(x, allmeta, origmeta) {
  if(!allmeta && length(x) == 0L) return(NULL) # Omits non-observed series.
  if(origmeta) return(attr(x, "metadata"))
  m <- attr(x, "metadata")
  PROVINCE <- if(length(m$PROVINCE)) m$PROVINCE else m$REGION
  return(list(source_code = null2NA(m$SOURCE_IDENTIFIER),
              frequency = null2NA(m$FREQ),
              label = null2NA(m$LABEL),
              province = if(length(PROVINCE)) province_switch(PROVINCE) else NA_character_,
              district = null2NA(m$DISTRICT),
              unit_measure = null2NA(m$UNIT_MEASURE),
              unit_mult = if(length(m$UNIT_MULT)) unit_mult_switch(m$UNIT_MULT) else NA_character_,
              base_period = null2NA(m$BASE_PER),
              seas_adjust = null2NA(m$SEASONAL_ADJUST),
              comment = null2NA(m$COMMENT)))
}

econdata_long <- function(x, combine = FALSE, allmeta = FALSE, origmeta = FALSE, ...) {
  if(is.null(attributes(x))) {
    res <- lapply(add_version_names(x), econdata_long, combine, allmeta, origmeta)
    return(if(combine) rbindlist(res, fill = TRUE) else res)
  }
  d <- unlist2d(x, "code", row.names = "date", DT = TRUE) |>
       fmutate(date = as.Date(date), code = qF(code)) |>
       frename(OBS_VALUE = "value")
  m <- attr(x, "metadata")
  meta <- lapply(x, econdata_extract_metadata, allmeta && !combine, origmeta) |>
          rbindlist(use.names = origmeta, fill = origmeta)
  if(origmeta) names(meta) <- tolower(names(meta))
  meta$code <- if(allmeta && !combine) names(x) else names(x)[names(x) %in% levels(d$code)]
  meta$source <- null2NA(m$DataProvider[[2L]])
  meta$dataset <- null2NA(m$Dataflow[[2L]])
  meta$source_dataset <- null2NA(m$SOURCE_DATASET)
  meta$version <- null2NA(m$Dataflow[[3L]])
  setcolorder(meta, c("source", "dataset", "source_dataset", "version", "code", if(!origmeta) "source_code"))
  if(!allmeta) get_vars(meta, fnobs(meta) == 0L) <- NULL
  if(combine) {
    meta_fct <- dapply(meta, qF, drop = FALSE) # Factors for efficient storage
    code <- d$code
    d$code <- NULL
    add_vars(d, "front") <- ss(meta_fct, ckmatch(code, meta_fct$code), check = FALSE)
    return(d)
  }
  return(list(data = d, metadata = meta))
}

# Tidying the output of read_release()
econdata_tidy_release <- function(x) {
  axnull <- is.null(attributes(x))
  if(axnull && length(x) > 1L) {
    res <- lapply(x, econdata_tidy_release)
    return(add_version_names(res, elem = "Flowref"))
  }
  if(axnull) x <- x[[1L]]
  res <- rbindlist(x$Releases)
  res$Date <- as.POSIXct(res$Date)
  names(res) <- tolower(names(res))
  attr(res, "metadata") <- x$DataSet
  return(qDT(res, keep.attr = TRUE))
}

# This is just needed to get rid of the wide argument for documenting this together with read_econdata()
econdata_tidy_core <- function(x, wide = TRUE, release = FALSE, ...)
  if(release) econdata_tidy_release(x) else if(wide) econdata_wide(x, ...) else econdata_long(x, ...)

econdata_tidy <- function(x, ...) econdata_tidy_core(x, ...)
