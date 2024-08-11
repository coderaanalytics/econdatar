write_release <- function(id, version, providerid, description, method = "release", ...)  {


  # Parameters ----

  params <- list(...)
  if (!is.null(params$agencyid)) {
    agencyid  <- params$agencyid
  } else {
    agencyid <- "ECONDATA"
  }
  query_params <- list()
  query_params$description <- description
  if (!is.null(params$release)) {
    query_params$release <- params$release
  } else {
    query_params$release <- format(Sys.time(),
                                   "%Y-%m-%dT%H:%M:%S",
                                   tz = "Africa/Johannesburg")
  }
  stopifnot(length(method) == 1)
  stopifnot(method %in% c("release", "reset", "rollback"))
  env <- fromJSON(system.file("settings.json", package = "econdatar"))


  # Commit data set release ----

  if (exists("econdata_token", envir = .pkgenv)) {
    token <- unlist(strsplit(get("econdata_token", envir = .pkgenv), " "))[2]
    payload <- jwt_split(token)$payload
    if (Sys.time() > as.POSIXct(payload$exp, origin="1970-01-01")) {
      login_helper(env$auth)
    }
  } else {
    login_helper(env$auth)
  }
  dataset_ref <- paste(agencyid, id, version, sep = "-")
  if (method == "release") {
    message("Committing release: ", dataset_ref, "\n")
    response <- POST(env$repository$url,
                     path = paste(env$repository$path,
                                  "datasets",
                                  dataset_ref,
                                  "commit", sep = "/"),
                     query = query_params,
                     add_headers(authorization = get("econdata_token",
                                                     envir = .pkgenv)),
                     accept_json())
    if (response$status_code == 200) {
      message(content(response, type = "application/json")$success)
    } else {
      stop(content(response, type = "application/json"))
    }
  } else if (method == "reset") {
    message("Resetting release: ", dataset_ref, "\n")
    response <- POST(env$repository$url,
                     path = paste(env$repository$path,
                                  "datasets",
                                  dataset_ref,
                                  "reset", sep = "/"),
                     add_headers(authorization = get("econdata_token",
                                                     envir = .pkgenv)),
                     accept_json())
    if (response$status_code == 200) {
      message(content(response, type = "application/json")$success)
    } else {
      stop(content(response, type = "application/json"))
    }
  } else if (method == "rollback") {
    message("Rolling back release: ", dataset_ref, "\n")
    response <- POST(env$repository$url,
                     path = paste(env$repository$path,
                                  "datasets",
                                  dataset_ref,
                                  "rollback", sep = "/"),
                     add_headers(authorization = get("econdata_token",
                                                     envir = .pkgenv)),
                     accept_json())
    if (response$status_code == 200) {
      message(content(response, type = "application/json")$success)
    } else {
      stop(content(response, type = "application/json"))
    }
  } else {
    stop("Method not implemented.")
  }
  return(invisible(NULL))
}
