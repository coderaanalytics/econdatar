check_found <- function(found, agencyid, id, version, what) {
  if ("all" %in% id) {
    return(invisible(NULL))
  }
  found_ids <- vapply(found, function(x) x[[2]]$id, character(1))
  missing_ids <- setdiff(id, found_ids)
  if (length(missing_ids) != 0) {
    stop("No ", what, " found for id(s): ",
         paste(missing_ids, collapse = ", "),
         " (agencyid: ", paste(agencyid, collapse = ", "),
         ", version: ", paste(version, collapse = ", "), ").",
         call. = FALSE)
  }
  return(invisible(NULL))
}
