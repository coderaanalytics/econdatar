read_dataset <- function(id, tidy = FALSE, ...) {


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
  env <- fromJSON(system.file("settings.json", package = "econdatar"))


  # Fetch data set(s) ----

  message(paste("\nFetching data set(s) -", id, "\n"))
  if (!is.null(params$file)) {
    data_message <- fromJSON(params$file, simplifyVector = FALSE)
    message("Data set(s) successfully retrieved from local storage.\n")
  } else {
    if (exists("econdata_token", envir = .pkgenv)) {
      token <- unlist(strsplit(get("econdata_token", envir = .pkgenv), " "))[2]
      payload <- jwt_split(token)$payload
      if (Sys.time() > as.POSIXct(payload$exp, origin="1970-01-01")) {
        login_helper(env$auth)
      }
    } else {
      login_helper(env$auth)
    }
    query_params <- list()
    query_params$agencyids <- paste(agencyid, collapse = ",")
    query_params$ids <- paste(id, collapse = ",")
    query_params$versions <- paste(version, collapse = ",")
    response <- GET(env$repository$url,
                    path = c(env$repository$path, "/datasets"),
                    query = query_params,
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (params$debug == TRUE) {
      message("Request URL: ", response$request$url, "\n")
    }
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
  }


  # Process data sets ----

  database <- lapply(data_message[[2]][["data-sets"]], function(raw_data_set) {
    if (!is.null(params$file)) {
      tmp_data_set <- raw_data_set[[2]]
    } else {
      data_set_ref <- paste(raw_data_set[[2]]$agencyid,
                            raw_data_set[[2]]$id,
                            raw_data_set[[2]]$version,
                            sep = "-")
      query_params <- list()
      query_params$release <- get_release(env, data_set_ref, params$release, params$debug)
      if (!is.null(params$series_key)) {
        query_params[["series-key"]] <- params$series_key
      }
      tmp_data_set <- get_data(env, data_set_ref, query_params, debug = params$debug)
    }
    series_names <- sapply(tmp_data_set$series, function(raw_series) {
      return(raw_series[["series-key"]])
    })
    data_set <- lapply(tmp_data_set$series, function(raw_series) {
      obs <- raw_series$obs
      raw_series[["series-key"]] <- NULL
      raw_series[["obs"]] <- NULL
      if (length(obs) == 0) {
        series <- data.frame()
        attr(series, "metadata") <- raw_series
        return(series)
      } else {
        fields <- lapply(obs, function(x) names(x)) |>
          unlist() |>
          unique()
        series <- lapply(fields, function(n) {
          sapply(obs, function(x) {
            ifelse(is.null(x[[n]]), NA, x[[n]])
          })
        }) |>
          as.data.frame(col.names = fields)
        rownames(series) <- series$TIME_PERIOD
        series$TIME_PERIOD <- NULL
        attr(series, "metadata") <- raw_series
        return(series)
      }
    })
    names(data_set) <- series_names
    tmp_data_set$series <- NULL
    attr(data_set, "metadata") <- tmp_data_set
    class(data_set) <- "eds_dataset"
    if (tidy) {
      tidy_data(data_set, ...)
    } else {
      return(data_set)
    }
  })
  class(database) <- "eds_database"
  if (length(database) == 1) {
    return(database[[1]])
  } else {
    if (tidy) {
      versions <- sapply(database, function(x) attr(x, "metadata")$version)
      names(database) <- paste0("v", versions)
      return(database)
    } else {
      return(database)
    }
  }
}

read_econdata <- function(id, tidy = FALSE, ...) {
  read_dataset(id = id, tidy = tidy, ...)
}

get_release <- function(env, ref, candidate_release, debug = FALSE) {
  if (is.null(candidate_release)) {
    candidate_release <- "latest"
  }
  if (candidate_release != "unreleased") {
    final_release <- tryCatch({
      if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}(T\\d{1,2}:\\d{1,2}:\\d{1,2})?$",
                candidate_release,
                perl = TRUE)) {
        strftime(candidate_release, "%Y-%m-%dT%H:%M:%S")
      } else {
        stop("Unacceptable proposed time/date format")
      }
    }, error = function(e) {
      response <- GET(env$repository$url,
                      path = paste(env$repository$path,
                                   "datasets",
                                   ref,
                                   "release", sep = "/"),
                      add_headers(authorization = get("econdata_token",
                                                      envir = .pkgenv)),
                      accept_json())
      if (debug == TRUE) {
        message("Request URL: ", response$request$url, "\n")
      }
      if (response$status_code != 200) {
        stop(content(response, type = "application/json"))
      }
      data_message <- content(response, type = "application/json")
      if (length(data_message$releases) != 0) {
        if (candidate_release == "latest") {
          release <- head(data_message$releases, n = 1)[[1]]$release |>
            as.POSIXct(tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
          attr(release, "tzone") <- "Africa/Johannesburg"
          return(strftime(release, "%Y-%m-%dT%H:%M:%S"))
        } else {
          release <- sapply(data_message$releases, function(release) {
            if (candidate_release == release$description) {
              release$release
            } else {
              NA
            }
          }) |>
            na.omit() |>
            head(n = 1)
          if (length(release) != 0) {
            release <- as.POSIXct(release,
                                  tz = "UTC",
                                  format = "%Y-%m-%dT%H:%M:%SZ")
            attr(release, "tzone") <- "Africa/Johannesburg"
            return(strftime(release, "%Y-%m-%dT%H:%M:%S"))
          } else {
            message("Release not found, returning latest release instead.")
            release <- tail(data_message$releases, n = 1)[[1]]$release |>
              as.POSIXct(tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
            attr(release, "tzone") <- "Africa/Johannesburg"
            return(strftime(release, "%Y-%m-%dT%H:%M:%S"))
          }
        }
      } else {
        stop("Data set has no historical releases, ",
             "try: release = \"unreleased\"")
      }
    })
  } else {
    final_release <- NULL
  }
  return(final_release)
}

get_data <- function(env, ref, params, links = NULL, data_set = NULL, debug = FALSE) {
  if (is.null(links)) {
    response <- GET(env$repository$url,
                    path = paste(env$repository$path,
                                 "datasets",
                                 ref, sep = "/"),
                    query = params,
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (debug == TRUE) {
      message("Request URL: ", response$request$url, "\n")
    }
    if (response$status_code == 200) {
      message("Processing data set: ", ref, "\n")
    } else {
      stop(content(response, type = "application/json"))
    }
    links <- unlist(strsplit(response$headers$link, ","))
    data_message <- content(response, type = "application/json")
    data_set <- data_message[[2]][["data-sets"]][[1]][[2]]
    if (!any(grepl("rel=next", links))) {
      return(data_set)
    } else {
      return(get_data(env, ref, params, links, data_set, debug))
    }
  } else {
    link_next <- links[grepl("rel=next", links)]
    link_match <- regexec("^(<)(.+)(>;rel=next)$", link_next)
    link_parts <- regmatches(link_next, link_match)[[1]][3] |>
      URLdecode() |>
      strsplit("\\?") |>
      unlist()
    response <-
      GET(env$repository$url,
          path = link_parts[1],
          query = strsplit(unlist(strsplit(link_parts[2], "&")), "=") |>
            lapply(function(x) {
              y <- list()
              y[[x[1]]] <- x[2]
              return(y)
            }) |>
            unlist(recursive = FALSE),
          add_headers(authorization = get("econdata_token", envir = .pkgenv)),
          accept("application/vnd.sdmx-codera.data+json"))
    if (debug == TRUE) {
      message("Request URL: ", response$request$url, "\n")
    }
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    links <- unlist(strsplit(response$headers$link, ","))
    data_message <- content(response, type = "application/json")
    data_set$series <- c(data_set$series,
                         data_message[[2]][["data-sets"]][[1]][[2]]$series)
    if (!any(grepl("rel=next", links))) {
      return(data_set)
    } else {
      return(get_data(env, ref, params, links, data_set, debug))
    }
  }
}
