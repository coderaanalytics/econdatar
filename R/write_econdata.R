write_econdata <- function(db, ...) {

  # Parameters ---


  env <- fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password))
    credentials <- paste(params$username, params$password, sep = ";")
  if (!is.null(params$agencyid))
    agencyid  <- params$agencyid
  if (!is.null(params$provideragencyid))
    provideragencyid <- params$provideragencyid

  query_params <- list()



  # Data message ---


  Header <- list()

  Header$ID <- unbox(tryCatch(paste0("ECONDATAR-V", sessionInfo()[[7]]$econdatar[[4]]),
                                        error = function(e) "Unknown"))
  Header$Test <- unbox(FALSE)
  Header$Prepared <- unbox(format(Sys.time(), format = "%Y-%m-%dT%T"))
  Header$Sender$id <- unbox(tryCatch(Sys.getenv()[["USER"]],
                                               error = function(e) "Anonymous"))

  dataset <- list()

  dataset$Header <- Header

  num_datasets <- if (!is.null(attributes(db)$metadata)) 1 else length(db)

  db_list <- list()
  if (num_datasets == 1) db_list[[1]] <- db else db_list <- db

  for (i in seq_len(num_datasets)) {

    dataset$DataSets[[i]] <- lapply(attributes(db_list[[i]])$metadata,
                                    function(x) {
                                      if (length(x) == 1) {
                                        unbox(x)
                                      } else if (is.list(x)) {
                                        unlist(x)
                                      } else {
                                        return(x)
                                      }
                                    })

    for (index in seq_len(length(names(db_list[[i]])))) {
      series <- names(db_list[[i]])[index]

      time_period <- row.names(db_list[[i]][[series]])
      row.names(db_list[[i]][[series]]) <- NULL

      dataset$DataSets[[i]]$Series[[index]] <-
        lapply(attributes(db_list[[i]][[series]])$metadata, unbox)
      dataset$DataSets[[i]]$Series[[index]]$Obs <-
        data.frame(db_list[[i]][[series]], TIME_PERIOD = time_period)
    }
  }



  # Push data message ---


  if (!is.null(params$file)) {

    data_message <- toJSON(dataset, na = "null")

    write(data_message, file = params$file)

    message("Data set saved to local storage.\n")

  } else {

    if (!exists("econdata_session", envir = .pkgenv))
      login_helper(credentials, env$repository$url)



    # Push each data set individually ---


    for (i in seq_len(num_datasets)) {

      single_dataset <- list()
      single_dataset$Header <- dataset$Header
      single_dataset$DataSets <- list()
      single_dataset$DataSets[[1]] <- dataset$DataSets[[i]]

      if (!is.null(agencyid) &&
          !is.null(params$id) &&
          !is.null(params$version) &&
          !is.null(provideragencyid) &&
          !is.null(params$providerid)) {

        params$version <- paste0(params$version, ".0")

        query_params_datasets <- list()
        query_params_datasets[["nested-flow-ref"]] <-
          paste(agencyid,
                params$id,
                params$version, sep = ",")
        query_params_datasets[["nested-provider-ref"]] <-
          paste(provideragencyid,
                params$providerid, sep = ",")

        response <- GET(env$repository$url,
                              path = c(env$repository$path, "/datasets"),
                              query = query_params_datasets,
                              set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                              accept_json())

        if (response$status_code != 200)
          stop(content(response, encoding = "UTF-8"))

        meta_dataset <- content(response, encoding = "UTF-8")

        dataset_id <- meta_dataset$DataSets[[1]]$DataSetID

        dataflow <- paste(meta_dataset$DataSets[[1]]$Dataflow, collapse = ",")
        dataprovider <- paste(meta_dataset$DataSets[[1]]$DataProvider,
                              collapse = ",")

        message("Writing dataset: ", dataflow, " - ", dataprovider, "\n")

      } else if (!is.null(single_dataset$DataSets[[1]]$DataSetID)) {

        dataset_id  <- single_dataset$DataSets[[1]]$DataSetID

        dataflow <- paste(single_dataset$DataSets[[1]]$Dataflow, collapse = ",")
        dataprovider <- paste(single_dataset$DataSets[[1]]$DataProvider,
                              collapse = ",")

        message("Writing dataset: ", dataflow, " - ", dataprovider, "\n")

      } else {
        stop(paste("Unable to identify data set,",
                   "please provide data flow and provider references"))
      }

      data_message <- toJSON(single_dataset, na = "null")

      tmp <- tempfile()
      write(data_message, file = tmp)

      response <- PUT(env$repository$url,
                            path = paste(env$repository$path,
                                         "datasets",
                                         dataset_id, sep = "/"),
                            query = query_params,
                            body = list("file" = upload_file(tmp, "application/json")),
                            encode = "multipart",
                            set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                            accept_json())

      if (response$status_code == 200)
        message(content(response, encoding = "UTF-8")$Result$Success$Message)
      else
        stop(content(response, encoding = "UTF-8"))
    }
  }
}
