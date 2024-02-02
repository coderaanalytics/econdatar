write_database <- function(x, create = FALSE, ...) {


  # Parameters ---

  env <- fromJSON(system.file("settings.json", package = "econdatar"))
  params <- list(...)
  if (!is.null(params$username) && !is.null(params$password)) {
    credentials <- paste(params$username, params$password, sep = ";")
  } else {
    credentials <- NULL
  }


  # Push data message ---

  if (is.null(params$file) && !exists("econdata_session", envir = .pkgenv)) {
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
    d <- lapply(attr(dataset, "metadata"), function(x) {
      if (length(x) == 1) { unbox(x) } else { return(x) }
    })
    d$series <- list()
    for (index in seq_len(length(dataset))) {
      series <- lapply(attr(dataset[[index]], "metadata"), unbox)
      d$series[[index]] <- series
    }
    datasets <- list(list(unbox("#sdmx.infomodel.dataset.DataSet"), d))
    data_message <- list(unbox("#sdmx.infomodel.message.SDMXMessage"),
                         list("header" = header,
                              "structures" = NULL,
                              "data-sets" = datasets))
    if (!is.null(params$file)) {
      write(toJSON(data_message, na = "null"), file = params$file)
      message("Data set saved to local storage.\n")
    } else if (create) {
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
  })
  return(invisible(NULL))
}
