read_release <- function(id, tidy = FALSE, ...) {


  # Parameters ----

  params <- list(...)
  if (!is.null(params$agencyid)) {
    agencyid  <- params$agencyid
  } else {
    agencyid <- "ECONDATA"
  }
  if (!is.null(params$version)) {
    version  <- params$version
  } else {
    version <- "latest"
  }
  query_params <- list()
  if (!is.null(params$before)) {
    query_params$before <- strftime(params$before, "%Y-%m-%dT%H:%M:%S")
  }
  if (!is.null(params$after)) {
    query_params$after <- strftime(params$after, "%Y-%m-%dT%H:%M:%S")
  }
  if (!is.null(params$description)) {
    query_params$description <- params$description
  }
  env <- fromJSON(system.file("settings.json", package = "econdatar"))


  # Fetch release ----

  if (exists("econdata_token", envir = .pkgenv)) {
    token <- unlist(strsplit(get("econdata_token", envir = .pkgenv), " "))[2]
    payload <- jwt_split(token)$payload
    if (Sys.time() > as.POSIXct(payload$exp, origin="1970-01-01")) {
      login_helper(env$auth)
    }
  } else {
    login_helper(env$auth)
  }
  response <- GET(env$repository$url,
                  path = c(env$repository$path, "/datasets"),
                  query = list(agencyids = paste(agencyid, collapse = ","),
                               ids = paste(id, collapse = ","),
                               versions = paste(version, collapse = ",")),
                  add_headers(authorization = get("econdata_token",
                                                  envir = .pkgenv)),
                  accept_json())
  if (response$status_code != 200)
    stop(content(response, type = "application/json"))
  data_message <- content(response, type = "application/json")
  releases <- lapply(data_message[["data-sets"]], function(dataset) {
    dataset_ref <- paste(dataset$agencyid,
                         dataset$id,
                         dataset$version, sep = "-")
    response <- GET(env$repository$url,
                    path = paste(env$repository$path,
                                 "datasets", dataset_ref,
                                 "release", sep = "/"),
                    query = query_params,
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code == 200) {
      message("Fetching releases for: ", dataset_ref, "\n")
    } else {
      stop(content(response, type = "application/json"))
    }
    release <- content(response, type = "application/json")
    release$releases <- lapply(release$releases, function(r) {
      list("release" = strptime(r$release, "%Y-%m-%dT%H:%M:%S%z"),
           "start-period" = strptime(r[["start-period"]], "%Y-%m-%d"),
           "end-period" = strptime(r[["end-period"]], "%Y-%m-%d"),
           "description" = r[["description"]])
    })
    class(release) <- "eds_release"
    if (tidy) {
      tidy_data(release, ...)
    } else {
      return(release)
    }
  })
  if (length(releases) == 1) {
    return(releases[[1]])
  } else {
    if (tidy) {
      versions <- sapply(releases, function(x) attr(x, "metadata")[[2]]$version)
      names(releases) <- paste0("v", versions)
      return(releases)
    } else {
      return(releases)
    }
  }
}
