write_econdata <- function(db, agencyid, id, version, provideragencyid, providerid, ...) {

  # Parameters ---


  env <- fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password))
    params$credentials <- paste(params$username, params$password, sep = ";")

  query_params <- list()



  # Data message ---


  dataset <- list()

  Header <- list()

  Header$ID <- unbox(tryCatch(paste0("ECONDATAR-V",
                                     sessionInfo()[[7]]$econdatar[[4]]),
                               error = function(e) "Unknown"))
  Header$Test <- unbox(FALSE)
  Header$Prepared <- unbox(format(Sys.time(), format = "%Y-%m-%dT%T"))
  Header$Sender$id <- unbox(tryCatch(Sys.getenv()[["USER"]],
                                      error = function(e) "Anonymous"))
  Header$DataStructure <- list(agencyID = unbox(agencyid),
                               id = unbox(id),
                               version = unbox(version))
  Header$DataProvider <- list(agencyID = unbox(provideragencyid),
                              id = unbox(providerid))

  dataset$Header <- Header

  dataset$DataSets[[1]] <- lapply(attributes(db)$metadata, unbox)

  for (index in seq_len(length(names(db)))) {
    series <- names(db)[index]

    time_period <- row.names(db[[series]])
    row.names(db[[series]]) <- NULL

    dataset$DataSets[[1]]$Series[[index]] <-
      lapply(attributes(db[[series]])$metadata, unbox)
    dataset$DataSets[[1]]$Series[[index]]$Obs <-
      data.frame(db[[series]], TIME_PERIOD = time_period)
  }

  data_message <- toJSON(dataset, na = "null")



  # Push data message ---


  message(paste("\nPushing dataset -", id, "\n"))

  if (!is.null(params$file)) {

    write(data_message, file = params$file)

    message("Data set saved to local storage.\n")

  } else {

    if (!is.null(params$credentials)) {
      credentials <- unlist(strsplit(params$credentials, ";"))
    } else if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
      credentials <- unlist(strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";"))
    } else {
      credentials <- econdata_credentials()
    }

    tmp <- tempfile()
    write(data_message, file = tmp)

    dataflow <- paste(agencyid, id, version, sep = ",")
    data_provider <- paste(provideragencyid, providerid, sep = ",")

    response <- POST(env$repository$url,
                     path = paste(env$repository$path, "modify/data",
                                  dataflow,
                                  data_provider, sep = "/"),
                     query = query_params,
                     body = list("file" = upload_file(tmp, "application/json")),
                     encode = "multipart",
                     authenticate(credentials[1], credentials[2]),
                     accept_json())

    if (response$status_code == 200)
      message(content(response, encoding = "UTF-8")$Result$Success$Message)
    else
      tryCatch(stop(content(response, encoding = "UTF-8")),
               error = function(e) stop(response))
  }
}
