write_release <- function(id, version, providerid, description, rollback = FALSE, ...)  {

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

  query_params <- list()

  query_params$description <- description

  if (!is.null(params$release)) {
    query_params$release <- release
  } else {
    query_params$release <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  }



  # Commit data set release ---


  if (!exists("econdata_session", envir = .pkgenv))
    login_helper(credentials, env$repository$url)

  dataset_ref <- paste(agencyid, id, version, sep = "-")

  if (rollback) {

    message("Rolling back release: ", dataset_ref, "\n")

    response <- POST(env$repository$url,
                     path = paste(env$repository$path,
                                  "datasets",
                                  dataset_ref,
                                  "rollback", sep = "/"),
                     set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                     accept_json())

    if (response$status_code == 200) {
      message(content(response, encoding = "UTF-8")$success)
    } else {
      stop(content(response, encoding = "UTF-8"))
    }
  } else {

    message("Committing release: ", dataset_ref, "\n")

    response <- POST(env$repository$url,
                     path = paste(env$repository$path,
                                  "datasets",
                                  dataset_ref,
                                  "commit", sep = "/"),
                     query = query_params,
                     set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                     accept_json())

    if (response$status_code == 200) {
      message(content(response, encoding = "UTF-8")$success)
    } else {
      stop(content(response, encoding = "UTF-8"))
    }
  }
}
