write_econdata <- function(x, create = FALSE, update = FALSE, stage = TRUE, ...) {

  # Parameters ---


  env <- fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password)) {
    credentials <- paste(params$username, params$password, sep = ";")
  } else {
    credentials <- NULL
  }



  # Push data message ---


  if (!is.null(params$file)) {

    data_message <- toJSON(x, na = "null")

    write(data_message, file = params$file)

    message("Data set saved to local storage.\n")

  } else {

    if (!exists("econdata_session", envir = .pkgenv)) {
      login_helper(credentials, env$repository$url)
    }

    header <- list()

    header$id <- tryCatch(
                          unbox(paste0("ECONDATAR-V",
                                       sessionInfo()[[7]]$econdatar[[4]])),
                          error = function(e)
                            unbox("Unknown"))

    header$prepared <- unbox(format(Sys.time(), format = "%Y-%m-%dT%T"))

    header$sender <- tryCatch(
                              unbox(Sys.getenv()[["USER"]]),
                              error = function(e)
                                unbox("Anonymous"))

    header$receiver <- unbox("EconData web application")


    if (any(names(x) == "data-sets")) {
      datasets <- x[["data-sets"]]
    } else {
      datasets <- list(x)
    }



    # Push each data set individually ---


    lapply(datasets, function(dataset) {

      dataset_ref <- paste(attr(dataset, "metadata")$agencyid,
                           attr(dataset, "metadata")$id,
                           attr(dataset, "metadata")$version, sep = "-")

      d <- lapply(attr(dataset, "metadata"),
                  function(x) if (length(x) == 1) unbox(x) else return(x))
      d$series <- list()

      for (index in seq_len(length(dataset))) {
        series <- lapply(attr(dataset[[index]], "metadata"), unbox)

        freq <- series$FREQ

        x <- dataset[[index]]

        y <- rownames(x)

        if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", y, perl = TRUE))) {
          z <- tryCatch(as.Date(y), error = function(e) {
                          stop("Unable to coerce some row names to as.Date\n",
                               "Error in data set ", dataset_ref,
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
                   "B" = !any(weekdays(z, TRUE) %in% c("Sat", "Sun")))
          if (!is.null(periods_valid) && !periods_valid) {
            stop("Some dates (row names) are not valid\n",
                 "Error in data set ", dataset_ref, " at index ", index)
          }
        } else {
          stop("Row names must have format: '%Y-%m-%d'\n",
               "Error in data set ", dataset_ref, " at index ", index)
        }

        series$obs <- data.frame(TIME_PERIOD = y, x, row.names = NULL)

        d$series[[index]] <- series
      }

      datasets <- list(list(unbox("#sdmx.infomodel.dataset.DataSet"), d))

      data_message <- list(unbox("#sdmx.infomodel.message.SDMXMessage"),
                           list("header" = header,
                                "structures" = NULL,
                                "data-sets" = datasets))

      if (create) {
        message("Creating data set: ", dataset_ref, "\n")

        response <- POST(env$repository$url,
                         path = paste(env$repository$path,
                                      "datasets", sep = "/"),
                         body = toJSON(data_message, na = "null"),
                         set_cookies(.cookies = get("econdata_session",
                                                    envir = .pkgenv)),
                         content_type("application/vnd.sdmx-codera.data+json"),
                         accept_json())

        if (response$status_code == 201) {
          message(content(response, encoding = "UTF-8")$success)
        } else {
          stop(content(response, encoding = "UTF-8"))
        }
      } else {

        if (update) {
          message("Updating data set: ", dataset_ref, "\n")

          response <- PUT(env$repository$url,
                          path = paste(env$repository$path,
                                       "datasets",
                                       dataset_ref, sep = "/"),
                          body = toJSON(data_message, na = "null"),
                          set_cookies(.cookies = get("econdata_session",
                                                     envir = .pkgenv)),
                          content_type("application/vnd.sdmx-codera.data+json"),
                          accept_json())

          if (response$status_code == 200) {
            message(content(response, encoding = "UTF-8")$success)
          } else {
            stop(content(response, encoding = "UTF-8"))
          }
        }

        if (stage) {
          message("Staging release: ", dataset_ref, "\n")

          response <- POST(env$repository$url,
                           path = paste(env$repository$path,
                                        "datasets",
                                        dataset_ref,
                                        "stage", sep = "/"),
                           body = toJSON(data_message, na = "null"),
                           set_cookies(.cookies = get("econdata_session",
                                                      envir = .pkgenv)),
                           content_type("application/vnd.sdmx-codera.data+json"),
                           accept_json())

          if (response$status_code == 200) {
            message(content(response, encoding = "UTF-8")$success)
          } else {
            stop(content(response, encoding = "UTF-8"))
          }
        }
      }
    })
  }
}
