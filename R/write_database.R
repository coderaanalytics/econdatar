write_database <- function(x, method = "update", ...) {


  # Parameters ----

  params <- list(...)
  stopifnot(length(method) == 1)
  stopifnot(method %in% c("create", "update", "delete"))
  stopifnot(isa(x, "eds_dataset") || (method == "delete" && is.null(x)))
  env <- fromJSON(system.file("settings.json", package = "econdatar"))
  if (nchar(Sys.getenv("ECONDATA_URL")) != 0) {
    env$repository$url <- Sys.getenv("ECONDATA_URL")
    env$registry$url <- Sys.getenv("ECONDATA_URL")
  }
  if (method == "delete") {
    delete_database(x, env, params)
    return(invisible(NULL))
  }


  # Push data message ----

  if (is.null(params$file)) {
    if (!exists("econdata_apikey", envir = .pkgenv)) {
      login_helper()
    }
  }
  header <- list()
  header$id <-  unbox("ECONDATAR")
  header$prepared <- unbox(format(Sys.time(), format = "%Y-%m-%dT%T"))
  header$sender <- tryCatch(unbox(Sys.getenv()[["USER"]]),
                            error = function(e) unbox("Anonymous"))
  header$receiver <- unbox("EconData web application")
  if (any(names(x) == "data-sets")) {
    datasets <- x[["data-sets"]]
  } else {
    datasets <- list(x)
  }


  # Push each data set individually ----

  lapply(datasets, function(dataset) {
    dataset_ref <- paste(attr(dataset, "metadata")$agencyid,
                         attr(dataset, "metadata")$id,
                         attr(dataset, "metadata")$version, sep = "-")
    d <- lapply(attr(dataset, "metadata"), function(x) {
      if (length(x) == 1) unbox(x) else x
    })
    d$series <- list()
    for (index in seq_len(length(dataset))) {
      series <- lapply(attr(dataset[[index]], "metadata"), unbox)
      series$obs <- list()
      d$series[[index]] <- series
    }
    datasets <- list(list(unbox("#sdmx.infomodel.dataset.DataSet"), d))
    data_message <- list(unbox("#sdmx.infomodel.message.SDMXMessage"),
                         list("header" = header,
                              "data-sets" = datasets))
    if (!is.null(params$file)) {
      write(toJSON(data_message, na = "null", always_decimal = TRUE),
            file = params$file)
      message("Data set saved to local storage.\n")
    } else if (method == "create") {
      message("Creating data set: ", dataset_ref, "\n")
      response <- POST(env$repository$url,
                       path = paste(env$repository$path,
                                    "datasets", sep = "/"),
                       body = toJSON(data_message,
                                     na = "null",
                                     always_decimal = TRUE,
                                     auto_unbox = TRUE),
                       add_headers(authorization = get("econdata_apikey",
                                                       envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 201) {
        message(content(response, type = "application/json")$success)
      } else {
        error <- content(response, type = "application/json")
        if (response$status_code == 400) {
          if (!is.null(error$message) && error$message == "Validation error") {
            stop(toJSON(error, pretty = TRUE))
          } else {
            stop(error)
          }
        }  else {
          stop(error)
        }
      }
    } else if (method == "update") {
      message("Updating data set: ", dataset_ref, "\n")
      response <- PUT(env$repository$url,
                      path = paste(env$repository$path,
                                   "datasets",
                                   dataset_ref, sep = "/"),
                      body = toJSON(data_message, na = "null",
                                    always_decimal = TRUE),
                      add_headers(authorization = get("econdata_apikey",
                                                      envir = .pkgenv)),
                      content_type("application/vnd.sdmx-codera.data+json"),
                      accept_json())
      if (response$status_code == 200) {
        message(content(response, type = "application/json")$success)
      } else {
        error <- content(response, type = "application/json")
        if (response$status_code == 400) {
          if (!is.null(error$message) && error$message == "Validation error") {
            stop(toJSON(error, pretty = TRUE))
          } else {
            stop(error)
          }
        }  else {
          stop(error)
        }
      }
    } else {
      stop("Method not implemented.")
    }
  })
  return(invisible(NULL))
}

delete_database <- function(x, env, params) {
  if (!is.null(params$file)) {
    stop("Method delete is not supported when writing to a file.")
  }
  if (!exists("econdata_apikey", envir = .pkgenv)) {
    login_helper()
  }
  if (is.null(x)) {
    metadata <- list(params[c("agencyid", "id", "version")])
  } else if (any(names(x) == "data-sets")) {
    metadata <- lapply(x[["data-sets"]], attr, "metadata")
  } else {
    metadata <- list(attr(x, "metadata"))
  }
  lapply(metadata, function(m) {
    if (is.null(m$agencyid) || is.null(m$id) || is.null(m$version)) {
      stop("agencyid, id and version are required to delete a data set")
    }
    dataset_ref <- paste(m$agencyid, m$id, m$version, sep = "-")
    if (is.null(params$series_key)) {
      paths <- paste(env$repository$path, "datasets", dataset_ref, sep = "/")
      targets <- paste("data set:", dataset_ref)
    } else {
      paths <- paste(env$repository$path,
                     "datasets",
                     dataset_ref,
                     "series",
                     vapply(params$series_key, URLencode, character(1),
                            reserved = TRUE, USE.NAMES = FALSE),
                     sep = "/")
      targets <- paste0("series: ", dataset_ref, " ", params$series_key)
    }
    for (i in seq_along(paths)) {
      message("Deleting ", targets[i], "\n")
      response <- DELETE(env$repository$url,
                         path = paths[i],
                         add_headers(authorization = get("econdata_apikey",
                                                         envir = .pkgenv)),
                         accept_json())
      if (response$status_code == 200) {
        message(content(response, type = "application/json")$success)
      } else {
        stop(content(response, type = "application/json"))
      }
    }
  })
}
