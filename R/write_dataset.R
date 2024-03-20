write_dataset <- function(x, method = "stage", ...) {


  # Parameters ----

  env <- fromJSON(system.file("settings.json", package = "econdatar"))
  params <- list(...)
  if (!is.null(params$username) && !is.null(params$password)) {
    credentials <- paste(params$username, params$password, sep = ";")
  } else {
    credentials <- NULL
  }
  stopifnot(length(method) == 1)
  stopifnot(method %in% c("stage", "validate"))
  env <- fromJSON(system.file("settings.json",
                              package = "econdatar"))[[x$agencyid]]


  # Push data message ----

  if (is.null(params$file) && !exists("econdata_session", envir = .pkgenv)) {
    login_helper(credentials, env$repository$url)
  }
  header <- list()
  header$id <- unbox("ECONDATAR")
  header$prepared <- unbox(format(Sys.time(), format = "%Y-%m-%dT%T"))
  header$sender <- tryCatch(unbox(Sys.getenv()[["USER"]]),
                            error = function(e) unbox("Anonymous"))
  header$receiver <- unbox("EconData web application")
  if (any(names(x) == "data-sets")) {
    data_sets <- x[["data-sets"]]
  } else {
    data_sets <- list(x)
  }


  # Push each data set individually ----

  lapply(data_sets, function(data_set) {
    metadata <- attr(data_set, "metadata")
    data_set_ref <- paste(metadata$agencyid,
                          metadata$id,
                          metadata$version, sep = "-")
    data_message <-
      list(unbox("#sdmx.infomodel.message.SDMXMessage"),
           list("header" = header,
                "structures" = NULL,
                "data-sets" =
                  list(list(unbox("#sdmx.infomodel.dataset.DataSet"),
                            validate_series(data_set_ref, data_set)))))
    if (!is.null(params$file)) {
      write(toJSON(data_message, na = "null", always_decimal = TRUE),
            file = params$file)
      message("Data set saved to local storage.\n")
    } else if (method == "stage") {
      message("Staging release: ", data_set_ref, "\n")
      response <- POST(env$repository$url,
                       path = paste(env$repository$path,
                                    "datasets",
                                    data_set_ref,
                                    "stage", sep = "/"),
                       body = toJSON(data_message,
                                     na = "null",
                                     always_decimal = TRUE),
                       set_cookies(.cookies = get("econdata_session",
                                                  envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 200) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        error <- content(response, encoding = "UTF-8")
        if (response$status_code == 400) {
          if (error$message == "Validation error") {
            stop(toJSON(error, pretty = TRUE))
          } else {
            stop(error)
          }
        }  else {
          stop(error)
        }
      }
    } else if (method == "validate") {
      message("Validating release: ", data_set_ref, "\n")
      response <- POST(env$repository$url,
                       path = paste(env$repository$path,
                                    "datasets",
                                    data_set_ref,
                                    "validate", sep = "/"),
                       body = toJSON(data_message,
                                     na = "null",
                                     always_decimal = TRUE),
                       set_cookies(.cookies = get("econdata_session",
                                                  envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 200) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        error <- content(response, encoding = "UTF-8")
        if (response$status_code == 400) {
          if (error$message == "Validation error") {
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

write_econdata <- function(x, create = FALSE, update = FALSE, stage = TRUE, ...) {
  if (create || update) {
    stop("Create and update no longer supported, please use 'write_dataset'")
  }
  if (!stage) {
    warning("'stage=FALSE' ignored, staging anyway...")
  }
  write_dataset(x = x, ...)
}

validate_series <- function(data_set_ref, data_set) {
  d <- lapply(attr(data_set, "metadata"),
              function(x) if (length(x) == 1) unbox(x) else x)
  d$series <- lapply(seq_len(length(data_set)), function(index) {
    series <- lapply(attr(data_set[[index]], "metadata"), unbox)
    freq <- series$FREQ
    x <- data_set[[index]]
    y <- rownames(x)
    if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", y, perl = TRUE))) {
      z <- tryCatch(as.Date(y), error = function(e) {
        stop("Unable to coerce some row names to as.Date\n",
             "Error in data set ", data_set_ref,
             " at index ", index)
      })
      if (!is.null(freq) && any(freq == c("A", "S", "Q"))) {
        month <- as.integer(substr(y, 6, 7))
      }
      if (!is.null(freq) && any(freq == c("A", "S", "Q", "M"))) {
        day <- as.integer(substr(y, 9, 10))
      }
      periods_valid <-
        switch(freq,
               "A" = all(month == 1) && all(day == 1),
               "S" = all(month %in% c(1, 7)) && all(day == 1),
               "Q" = all(month %in% c(1, 4, 7, 10)) && all(day == 1),
               "M" = all(day == 1),
               "W" = all(weekdays(z, TRUE) == "Mon"),
               "B" = !any(weekdays(z, TRUE) %in% c("Sat", "Sun")),
               "D" = TRUE,
               "I" = TRUE,
               FALSE)
      if (!periods_valid) {
        stop("Some dates (row names) are not valid\n",
             "Error in data set ", data_set_ref, " at index ", index)
      }
    } else {
      stop("Row names must have format: '%Y-%m-%d'\n",
           "Error in data set ", data_set_ref, " at index ", index)
    }
    series$obs <- data.frame(TIME_PERIOD = y, x, row.names = NULL)
    return(series)
  })
  return(d)
}
