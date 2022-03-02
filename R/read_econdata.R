read_econdata <- function(id, ...) {

  # Parameters ---


  env <- fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password))
    params$credentials <- paste(params$username, params$password, sep = ";")

  query_params <- list()

  if (!is.null(params$releasedescription))
    query_params$releaseDescription <- params$releasedescription



  # Fetch data ---


  message(paste("\nFetching dataset -", id, "\n"))

  if (!is.null(params$file)) {

    data_message <- fromJSON(params$file, simplifyVector = FALSE)

    message("Data set successfully retrieved from local storage.\n")

  } else {

    if (!is.null(params$credentials)) {
      credentials <- unlist(strsplit(params$credentials, ";"))
    } else if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
      credentials <- unlist(strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";"))
    } else {
      credentials <- econdata_credentials()
    }

    dataflow <- paste(c(params$agencyid, id, params$version), collapse = ",")
    data_provider <- paste(c(params$provideragencyid,
                             params$providerid), collapse = ",")

    if (nchar(data_provider) == 0)
      data_provider <- NULL

    response <- GET(env$repository$url,
                    path = paste(c(env$repository$path, "data",
                                   dataflow,
                                   params$key,
                                   data_provider), collapse = "/"),
                    query = query_params,
                    authenticate(credentials[1], credentials[2]),
                    accept_json())

    if (response$status_code == 200)
      message("Data set successfully retrieved from EconData.\n")
    else
      tryCatch(stop(content(response, encoding = "UTF-8")),
               error = function(e) stop(response))

    data_message <- content(response, encoding = "UTF-8")
  }



  # Fetch data structure (metadata) ---


  datastructure <- data_message$Header$DataStructure

  dataflow <- paste(datastructure$agencyID,
                   datastructure$id,
                   datastructure$version,
                   sep = "/")

  data_structure <- content(GET(env$registry$url,
                                path = paste(c(env$registry$path,
                                               "datastructure",
                                               dataflow), collapse = "/"),
                                query = list(format = "sdmx-2.0")),
                            type = "application/xml",
                            encoding = "UTF-8")

  series_dims <- NULL
  all_dimensions <- xml_find_all(data_structure, "//str:Dimension")
  for (dimension in all_dimensions)
    series_dims <- c(series_dims, xml_attr(dimension, "conceptRef"))

  all_attributes <- xml_find_all(data_structure, "//str:Attribute")
  obs_attrs <- NULL
  for (attribute in all_attributes) {
    if (xml_attr(attribute, "attachmentLevel") == "Observation")
      obs_attrs <- c(obs_attrs, xml_attr(attribute, "conceptRef"))
  }



  # Return data ---


  database <- list()

  if (length(data_message$DataSets) > 1)
    stop("Multiple datasets not currently supported by econdatar.")

  dataset <- data_message$DataSets[[1]]

  for (series in dataset$Series) {
    obs <- series$Obs
    series$Obs <- NULL

    series_name <- NULL
    for (dimension in series_dims)
      series_name <- c(series_name, series[[dimension]])
    s <- paste(series_name, collapse = ".")

    if (length(obs) == 0) {
      database[[s]] <- data.frame()
    } else {
      obs_fields <- list()
      obs_fields$OBS_VALUE <- sapply(obs, function(x) as.numeric(x$OBS_VALUE))
      for (field in obs_attrs)
        obs_fields[[field]] <- sapply(obs, function(x) x[[field]])
      time_periods <- sapply(obs, function(x) x$TIME_PERIOD)
      database[[s]] <- data.frame(obs_fields, row.names = time_periods)
    }
    attr(database[[s]], "metadata") <- series
  }

  dataset$Series <- NULL
  attr(database, "metadata") <- dataset

  return(database)
}
