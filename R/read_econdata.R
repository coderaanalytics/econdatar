read_econdata <- function(id, ...) {

  # Parameters ---


  env <- fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password))
    params$credentials <- paste(params$username, params$password, sep = ";")

  query_params <- list()

  if (!is.null(params$key))
    query_params$key <- params$key
  if (!is.null(params$releasedescription))
    query_params$releaseDescription <- params$releasedescription

  credentials <- NULL



  # Fetch dataset(s) ---


  message(paste("\nFetching dataset(s) -", id, "\n"))

  if (!is.null(params$file)) {

    data_message <- fromJSON(params$file, simplifyVector = FALSE)

    message("Data set(s) successfully retrieved from local storage.\n")

  } else {

    if (!is.null(params$credentials)) {
      credentials <- unlist(strsplit(params$credentials, ";"))
    } else if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
      credentials <- unlist(strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";"))
    } else {
      credentials <- econdata_credentials()
    }

    if (!is.null(params$version) && params$version != "latest")
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

    if (response$status_code == 200) {
      message("Data set(s) successfully retrieved from EconData.\n")
    }
    else
      stop(content(response, encoding = "UTF-8"))

    data_message <- content(response, encoding = "UTF-8")
  }



  # Process data sets ---


  database <- lapply(data_message$DataSets, function(dataset) {

    # Fetch data structure (metadata) ---

   if (is.null(dataset$Dataflow))
     stop("Dataset data flow reference not available, ",
          "unable to fetch data set metadata")

    dataflow <- paste(dataset$Dataflow, collapse = "/")

    response <- GET(env$registry$url,
                    path = paste(c(env$registry$path,
                                   "dataflow",
                                   dataflow), collapse = "/"),
                    query = list(references = "children",
                                 format = "sdmx-2.0"))

    if (response$status_code == 200)
      message("Data structure successfully retrieved for data flow: ",
              paste(dataset$Dataflow, collapse = ","), "\n")
    else
      stop(content(response, encoding = "UTF-8"))

    datastructure <- content(response,
                             type = "application/xml",
                             encoding = "UTF-8")

    series_dims <- NULL
    all_dimensions <- xml_find_all(datastructure, "//str:Dimension")
    for (dimension in all_dimensions)
      series_dims <- c(series_dims, xml_attr(dimension, "conceptRef"))

    all_attributes <- xml_find_all(datastructure, "//str:Attribute")
    obs_attrs <- NULL
    for (attribute in all_attributes) {
      if (xml_attr(attribute, "attachmentLevel") == "Observation")
        obs_attrs <- c(obs_attrs, xml_attr(attribute, "conceptRef"))
    }



    # Fetch data set ---


    if (!is.null(params$file)) {
      dataset_1 <- dataset
    } else {

      dataset$Dataflow[3] <- paste0(dataset$Dataflow[3], ".0")

      query_params[["nested-flow-ref"]] <- paste(dataset$Dataflow,
                                                 collapse = ",")

      response <- GET(env$repository$url,
                      path = paste(env$repository$path,
                                   "datasets",
                                   dataset$DataSetID, sep = "/"),
                      query = query_params,
                      authenticate(credentials[1], credentials[2]),
                      accept_json())


      if (response$status_code == 200)
        message("Processing data set: ",
                paste(dataset$Dataflow, collapse = ","), " - ",
                paste(dataset$DataProvider, collapse = ","), "\n")
      else
        stop(content(response, encoding = "UTF-8"))

      data_message_1 <- content(response, encoding = "UTF-8")

      dataset_1 <- data_message_1$DataSets[[1]]
    }



    # Return data ---


    out <- list()

    for (series in dataset_1$Series) {
      obs <- series$Obs
      series$Obs <- NULL

      series_name <- NULL
      for (dimension in series_dims)
        series_name <- c(series_name, series[[dimension]])
      s <- paste(series_name, collapse = ".")

      if (length(obs) == 0) {
        out[[s]] <- data.frame()
      } else {
        obs_fields <- list()
        obs_fields$OBS_VALUE <- sapply(obs, function(x) as.numeric(x$OBS_VALUE))
        for (field in obs_attrs)
          obs_fields[[field]] <- sapply(obs, function(x) x[[field]])
        time_periods <- sapply(obs, function(x) x$TIME_PERIOD)
        out[[s]] <- data.frame(obs_fields, row.names = time_periods)
      }
      attr(out[[s]], "metadata") <- series
    }

    dataset_1$Series <- NULL
    attr(out, "metadata") <- dataset_1

    return(out)
  })

  if (length(database) == 1)
    return(database[[1]])
  else
    return(database)
}
