read_econdata <- function(id, ..., tidy = FALSE) {

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



  # Fetch data set(s) ---


  message(paste("\nFetching data set(s) -", id, "\n"))

  if (!is.null(params$file)) {

    data_message <- fromJSON(params$file, simplifyVector = FALSE)

    message("Data set(s) successfully retrieved from local storage.\n")

  } else {

    if (!exists("econdata_session", envir = .pkgenv)) {
      login_helper(credentials, env$repository$url)
    }

    query_params <- list()
    query_params$agencyids <- paste(agencyid, collapse = ",")
    query_params$ids <- paste(id, collapse = ",")
    query_params$versions <- paste(version, collapse = ",")

    response <- GET(env$repository$url,
                    path = c(env$repository$path, "/datasets"),
                    query = query_params,
                    set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }

    data_message <- content(response, type = "application/json", encoding = "UTF-8")
  }



  # Process data sets ---


  database <- lapply(data_message[[2]][["data-sets"]], function(data_set) {

   # Fetch data structure (metadata) ---

   provision_agreement_ref <- paste(data_set[[2]][["provision-agreement"]][[2]]$agencyid,
                                    data_set[[2]][["provision-agreement"]][[2]]$id,
                                    data_set[[2]][["provision-agreement"]][[2]]$version,
                                    sep = "-")

    response <- GET(env$registry$url,
                    path = paste(c(env$registry$path,
                                   "provisionagreements",
                                   provision_agreement_ref), collapse = "/"),
                    set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, type = "application/json", encoding = "UTF-8"))
    }

    data_message <- content(response, type = "application/json", encoding = "UTF-8")

    provision_agreement <- data_message[[2]]$structures[["provision-agreements"]][[1]]

    dataflow_ref <- paste(provision_agreement[[2]][["dataflow"]][[2]]$agencyid,
                          provision_agreement[[2]][["dataflow"]][[2]]$id,
                          provision_agreement[[2]][["dataflow"]][[2]]$version,
                          sep = "-") 

    response <- GET(env$registry$url,
                    path = paste(c(env$registry$path,
                                   "dataflows",
                                   dataflow_ref), collapse = "/"),
                    query = list(relations = "references"),
                    set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, type = "application/json", encoding = "UTF-8"))
    }

    data_message <- content(response, type = "application/json", encoding = "UTF-8")

    data_structure <- data_message[[2]]$structures[["data-structures"]][[1]][[2]]

    series_ids <- sapply(data_structure$components, function(component) {
        if (component[[1]] == "#sdmx.infomodel.datastructure.Dimension") {
          component[[2]][["concept-identity"]][[2]]$id
        } else {
          NA
        }
      })

    series_pos <- sapply(data_structure$components, function(component) {
        if (component[[1]] == "#sdmx.infomodel.datastructure.Dimension") {
          component[[2]]$position
        } else {
          NA
        }
      })

    series_dims <- na.omit(series_ids[order(series_pos)])

    obs_attrs <- sapply(data_structure$components, function(component) {
        if (component[[1]] == "#sdmx.infomodel.datastructure.Attribute") {
          if (component[[2]][["attachment-level"]] == "observation") {
            component[[2]][["concept-identity"]][[2]]$id
          } else {
            NA
          }
        } else {
          NA
        }
      }) |>
      na.omit()



    # Fetch data set ---


    if (is.null(params$file)) {

      data_set_ref <- paste(data_set[[2]]$agencyid,
                            data_set[[2]]$id,
                            data_set[[2]]$version,
                            sep = "-")

      query_params <- list()

      if (is.null(params$release) || params$release != "unreleased") {

        tryCatch(query_params$release <- strftime(params$release, "%Y-%m-%dT%H:%M:%S"),
                 error = function(e) { query_params$release <- NULL })

        if (is.null(query_params$release)) {

          response <- GET(env$repository$url,
                          path = paste(env$repository$path,
                                       "datasets",
                                       data_set_ref,
                                       "release", sep = "/"),
                          set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                          accept_json())

          if (response$status_code != 200) {
            stop(content(response, type = "application/json", encoding = "UTF-8"))
          }

          data_message <- content(response, type = "application/json", encoding = "UTF-8")

          if (is.null(params$release) || params$release == "latest") {
            release <- tail(data_message$releases, n = 1)[[1]]$release |>
              as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
            attr(release, "tzone") <- "Africa/Johannesburg"
            query_params$release <- strftime(release, "%Y-%m-%dT%H:%M:%S")

          } else {
            release <- sapply(data_message$releases, function(release) {
                         if(params$release == release$description) {
                           release$release
                         } else {
                           NA
                         }
                       }) |>
              na.omit() |>
              head(n = 1)

            if (length(release) != 0) {
              release <- as.POSIXct(release, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
              attr(release, "tzone") <- "Africa/Johannesburg"
              query_params$release <- strftime(release, "%Y-%m-%dT%H:%M:%S")
            } else {
              message("Release not found, returning latest release instead.")
              release <- tail(data_message$releases, n = 1)[[1]]$release |>
                as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
              attr(release, "tzone") <- "Africa/Johannesburg"
              query_params$release <- strftime(release, "%Y-%m-%dT%H:%M:%S")
            }
          }
        }
      }

      if (!is.null(params$series_key)) {
        query_params[["series-key"]] <- params$series_key
      }

      response <- GET(env$repository$url,
                      path = paste(env$repository$path,
                                   "datasets",
                                   data_set_ref, sep = "/"),
                      query = query_params,
                      set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                      accept("application/vnd.sdmx-codera.data+json"))

      if (response$status_code == 200) {
        message("Processing data set: ", data_set_ref, "\n")
      } else {
        stop(content(response, type = "application/json", encoding = "UTF-8"))
      }

      data_message <- content(response, type = "application/json", encoding = "UTF-8")

      data_set <- data_message[[2]][["data-sets"]][[1]][[2]]
    } else {
      data_set <- data_set[[2]]
      data_set$name <- lapply(data_set$name, unbox)
      data_set[["provision-agreement"]][[1]] <-
        unbox(data_set[["provision-agreement"]][[1]])
      data_set[["provision-agreement"]][[2]] <-
        lapply(data_set[["provision-agreement"]][[2]], unbox)
    }



    # Return data ---


    out <- list()

    for (series in data_set$series) {
      obs <- series$obs
      series[["series-key"]] <- NULL
      series[["obs"]] <- NULL

      series_name <- NULL
      for (dimension in series_dims) {
        series_name <- c(series_name, series[[dimension]])
      }
      s <- paste(series_name, collapse = ".")

      if (length(obs) == 0) {
        out[[s]] <- data.frame()
      } else {
        obs_fields <- list()
        obs_fields$OBS_VALUE <- sapply(obs, function(x) as.numeric(x$OBS_VALUE))
        for (field in obs_attrs) {
          obs_fields[[field]] <-
            sapply(obs, function(x) ifelse(is.null(x[[field]]), NA, x[[field]]))
        }
        time_periods <- sapply(obs, function(x) x$TIME_PERIOD)
        out[[s]] <- data.frame(obs_fields, row.names = time_periods)
      }
      attr(out[[s]], "metadata") <- series
    }

    data_set$series <- NULL
    attr(out, "metadata") <- data_set

    if (tidy) {
      econdata_tidy(out, ...)
    } else {
      return(out)
    }
  })

  if (length(database) == 1) {
    return(database[[1]])
  } else {
    return(list("data-sets", database[[1]]))
  }
}
