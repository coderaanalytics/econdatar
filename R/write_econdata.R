write_econdata <- function(db, stage = TRUE, update = FALSE, reset = FALSE, ...) {

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

    data_message <- toJSON(db, na = "null")

    write(data_message, file = params$file)

    message("Data set saved to local storage.\n")

  } else {

    if (!exists("econdata_session", envir = .pkgenv)) {
      login_helper(credentials, env$repository$url)
    }

    if (!reset) {
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
    }


    if (any(names(db) == "data-sets")) {
      datasets <- db[["data-sets"]]
    } else {
      datasets <- list(db)
    }



    # Push each data set individually ---


    lapply(datasets, function(dataset) {

      dataset_ref <- paste(attr(dataset, "metadata")$agencyid,
                           attr(dataset, "metadata")$id,
                           attr(dataset, "metadata")$version, sep = "-")

      if (reset) {

      } else {
        d <- lapply(attr(dataset, "metadata"),
                    function(x) if (length(x) == 1) unbox(x) else return(x))
        d$series <- list()

        for (index in seq_len(length(dataset))) {
          series <- lapply(attr(dataset[[index]], "metadata"), unbox)
          series$obs <- data.frame(TIME_PERIOD = rownames(dataset[[index]]),
                                   dataset[[index]],
                                   row.names = NULL)
          d$series[[index]] <- series
        }

        datasets <- list(list(unbox("#sdmx.infomodel.dataset.DataSet"), d))

        data_message <- list(unbox("#sdmx.infomodel.message.SDMXMessage"),
                             list("header" = header,
                                  "structures" = NULL,
                                  "data-sets" = datasets))

        if (update) {
          message("Updating data set: ", dataset_ref, "\n")

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
            message(content(response, encoding = "UTF-8"))
          } else {
            stop(content(response, encoding = "UTF-8"))
          }
        }
      }
    })
  }
}
