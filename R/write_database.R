write_database <- function(x, method = "update", ...) {


  # Parameters ----

  params <- list(...)
  stopifnot(length(method) == 1)
  stopifnot(method %in% c("create", "update"))
  env <- fromJSON(system.file("settings.json", package = "econdatar"))


  # Push data message ----

  if (is.null(params$file)) {
    if (exists("econdata_token", envir = .pkgenv)) {
      token <- unlist(strsplit(get("econdata_token", envir = .pkgenv), " "))[2]
      payload <- jwt_split(token)$payload
      if (Sys.time() > as.POSIXct(payload$exp, origin="1970-01-01")) {
        login_helper(env$auth)
      }
    } else {
      login_helper(env$auth)
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
      if (length(x) == 1) unbox(x) else return(x)
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
                              "structures" = NULL,
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
                       body = toJSON(data_message, na = "null",
                                     always_decimal = TRUE),
                       add_headers(authorization = get("econdata_token",
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
                      add_headers(authorization = get("econdata_token",
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
