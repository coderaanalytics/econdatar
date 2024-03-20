read_database <- function(id, include_series = FALSE, tidy = FALSE, ...) {


  # Parameters ----

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
  if (!is.null(params$portal)) {
    portal <- params$portal
  } else {
    portal <- "econdata"
  }
  env <- fromJSON(system.file("settings.json", package = "econdatar"))[[portal]]


  # Fetch data set(s) ----

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
                    set_cookies(.cookies = get("econdata_session",
                                               envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <- content(response,
                            type = "application/json",
                            encoding = "UTF-8")
  }


  # Process data set(s) ----

  database <- lapply(data_message[[2]][["data-sets"]], function(raw_data_set) {
    if (!is.null(params$file)) {
      tmp_data_set <- raw_data_set[[2]]
    } else {
      if (include_series) {
        data_set_ref <- paste(raw_data_set[[2]]$agencyid,
                              raw_data_set[[2]]$id,
                              raw_data_set[[2]]$version,
                              sep = "-")
        query_params <- list()
        if (!is.null(params$series_key)) {
          query_params[["series-key"]] <- params$series_key
        }
        response <- GET(env$repository$url,
                        path = paste(env$repository$path,
                                     "datasets",
                                     data_set_ref,
                                     "series", sep = "/"),
                        query = query_params,
                        set_cookies(.cookies =
                                      get("econdata_session", envir = .pkgenv)),
                        accept("application/vnd.sdmx-codera.data+json"))
        if (response$status_code == 200) {
          message("Processing data set: ", data_set_ref, "\n")
        } else {
          stop(content(response, type = "application/json", encoding = "UTF-8"))
        }
        data_message <- content(response,
                                type = "application/json",
                                encoding = "UTF-8")
        tmp_data_set <- data_message[[2]][["data-sets"]][[1]][[2]]
      } else {
        tmp_data_set <- raw_data_set[[2]]
      }
    }
    data_set <- list()
    series_names <- sapply(tmp_data_set$series, function(raw_series) {
      return(raw_series[["series-key"]])
    })
    data_set$series <- lapply(tmp_data_set$series, function(raw_series) {
      raw_series[["series-key"]] <- NULL
      raw_series[["obs"]] <- NULL
      series <- data.frame()
      attr(series, "metadata") <- raw_series
      return(series)
    })
    names(data_set$series) <- series_names
    tmp_data_set$series <- NULL
    attr(data_set, "metadata") <- tmp_data_set
    class(data_set) <- "eds_dataset"
    return(data_set)
  })
  class(database) <- "eds_database"
  if (tidy) {
    tidy_data(database, ...)
  } else {
    if (length(database) == 1) {
      return(database[[1]])
    } else {
      return(database)
    }
  }
}
