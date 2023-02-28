write_release <- function(agencyid, id, version, provideragencyid, providerid, releasedescription, ...)  {

  # Parameters ---


  env <- jsonlite::fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password))
    params$credentials <- paste(params$username, params$password, sep = ";")

  query_params <- list()

  query_params$releaseDescription <- releasedescription

  if (!is.null(params$releasedatetime))
    query_params$releaseDateTime <- params$releasedatetime



  # Update data set ---


  if (!is.null(params$credentials)) {
    credentials <- unlist(strsplit(params$credentials, ";"))
  } else if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
    credentials <- unlist(strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";"))
  } else {
    credentials <- econdata_credentials()
  }

  version <- paste0(version, ".0")

  query_params_datasets <- list()
  query_params_datasets[["nested-flow-ref"]] <-
    paste(agencyid, id, version, sep = ",")
  query_params_datasets[["nested-provider-ref"]] <-
    paste(provideragencyid, providerid, sep = ",")

  response <- httr::GET(env$repository$url,
                  path = c(env$repository$path, "/datasets"),
                  query = query_params_datasets,
                  httr::authenticate(credentials[1], credentials[2]),
                  httr::accept_json())

  if (response$status_code != 200)
    stop(httr::content(response, encoding = "UTF-8"))

  meta_dataset <- httr::content(response, encoding = "UTF-8")

  dataset_id <- meta_dataset$DataSets[[1]]$DataSetID

  dataflow <- paste(meta_dataset$DataSets[[1]]$Dataflow, collapse = ",")
  dataprovider <- paste(meta_dataset$DataSets[[1]]$DataProvider,
                        collapse = ",")

  message("Committing release: ", dataflow, " - ", dataprovider, "\n")

  response <- httr::POST(env$repository$url,
                         path = paste(env$repository$path,
                                      "datasets",
                                      dataset_id,
                                      "releases",
                                      "commit-release", sep = "/"),
                         query = query_params,
                         httr::authenticate(credentials[1], credentials[2]),
                         httr::accept_json())

  if (response$status_code == 201)
    message(httr::content(response, encoding = "UTF-8")$Result$Success$Message)
  else
    stop(httr::content(response, encoding = "UTF-8"))
}
