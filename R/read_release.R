read_release <- function(id, ..., tidy = FALSE) {

  # Parameters ---


  env <- fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password))
    params$credentials <- paste(params$username, params$password, sep = ";")

  query_params <- list()

  if (!is.null(params$newest))
    query_params$newest <- if (params$newest) "true" else "false"
  if (!is.null(params$oldest))
    query_params$oldest <- if (params$oldest) "true" else "false"
  if (!is.null(params$before))
    query_params$beforeDateTime <- strftime(params$before, "%Y-%m-%dT%H:%M:%S")
  if (!is.null(params$after))
    query_params$afterDateTime <- strftime(params$after, "%Y-%m-%dT%H:%M:%S")
  if (!is.null(params$includestext))
    query_params$includesText <- params$includestext
  if (!is.null(params$releasedescription))
    query_params$releaseDescription <- params$releasedescription
  if (!is.null(params$returnrange))
    query_params$returnRange <- if (params$returnrange) "true" else "false"


  # Fetch release ---


  if (!is.null(params$credentials)) {
    credentials <- unlist(strsplit(params$credentials, ";"))
  } else if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
    credentials <- unlist(strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";"))
  } else {
    credentials <- econdata_credentials()
  }

  if (!is.null(params$version) &&
      params$version != "latest" &&
      params$version != "all")
    params$version <- paste0(params$version, ".0")

  query_params_datasets <- list()
  query_params_datasets[["nested-flow-ref"]] <-
    paste(c(params$agencyid, id, params$version), collapse = ",")
  if (!is.null(params$providerid)) {
    query_params_datasets[["nested-provider-ref"]] <-
      paste(c(params$provideragencyid,
              params$providerid), collapse = ",")
  }

  response <- GET(env$repository$url,
                        path = c(env$repository$path, "/datasets"),
                        query = query_params_datasets,
                        authenticate(credentials[1], credentials[2]),
                        accept_json())

  if (response$status_code != 200)
    stop(content(response, encoding = "UTF-8"))

  data_message <- content(response, encoding = "UTF-8")

  releases <- lapply(data_message$DataSets, function(dataset) {

    response <- GET(env$repository$url,
                          path = paste(env$repository$path,
                                       "datasets", dataset$DataSetID,
                                       "releases", sep = "/"),
                          query = query_params,
                          authenticate(credentials[1], credentials[2]),
                          accept_json())

    if (response$status_code == 200) {
      message("Fetching releases for: ",
              paste(dataset$Dataflow, collapse = ","), " - ",
              paste(dataset$DataProvider, collapse = ","), "\n")
    } else {
      stop(content(response, encoding = "UTF-8"))
    }

    return(content(response, encoding = "UTF-8")$Result$Success$Message)
  })

  if(tidy) return(econdata_tidy_release(releases))

  return(releases)
}
