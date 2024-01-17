read_release <- function(id, ..., tidy = FALSE) {

  # Parameters ---


  env <- fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password)) {
    credentials <- paste(params$username, params$password, sep = ";")
  } else {
    credentials <- NULL
  }

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
    query_params[["before"]] <- strftime(params$before,
                                         "%Y-%m-%dT%H:%M:%S")
  }

  if (!is.null(params$after)) {
    query_params[["after"]] <- strftime(params$after,
                                        "%Y-%m-%dT%H:%M:%S")
  }

  if (!is.null(params$description)) {
    query_params$description <- params$description
  }



  # Fetch release ---


  if (!exists("econdata_session", envir = .pkgenv)) {
    login_helper(credentials, env$repository$url)
  }

  dataset_ref <- paste(agencyid, id, version, sep = "-")

  response <- GET(env$repository$url,
                  path = c(env$repository$path, "/datasets"),
                  query = list(agencyids = paste(agencyid, collapse = ","),
                               ids = paste(id, collapse = ","),
                               versions = paste(version, collapse = ",")),
                  set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                  accept_json())

  if (response$status_code != 200)
    stop(content(response, encoding = "UTF-8"))

  data_message <- content(response, encoding = "UTF-8")

  releases <- lapply(data_message[["data-sets"]], function(dataset) {

    dataset_ref <- paste(dataset$agencyid,
                         dataset$id,
                         dataset$version, sep = "-")

    response <- GET(env$repository$url,
                    path = paste(env$repository$path,
                                 "datasets", dataset_ref,
                                 "release", sep = "/"),
                    query = query_params,
                    set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code == 200) {
      message("Fetching releases for: ", dataset_ref, "\n")
    } else {
      stop(content(response, encoding = "UTF-8"))
    }

    release <- content(response, type = "application/json", encoding = "UTF-8")

    release$releases <- lapply(release$releases, function(r) {
        list("release" = strptime(r$release, "%Y-%m-%dT%H:%M:%S%z"),
             "start-period" = strptime(r[["start-period"]], "%Y-%m-%d"),
             "end-period" = strptime(r[["end-period"]], "%Y-%m-%d"),
             "description" = r[["description"]])
      })

    if (tidy) {
      econdata_tidy(release, is_release = TRUE, ...)
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
