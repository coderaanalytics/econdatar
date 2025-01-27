read_registry <- function(structure, tidy = TRUE, ...) {


  # Parameters ----

  params <- list(...)
  if (is.null(params$id) && is.null(params$file)) {
    stop("At least one of either: 'id' or 'file' parameter required.")
  }
  if (!is.null(params$agencyid)) {
    agencyid  <- params$agencyid
  } else {
    agencyid <- "ECONDATA"
  }
  if (!is.null(params$id)) {
    id  <- params$id
  } else {
    id <- NULL
  }
  if (!is.null(params$version)) {
    version  <- params$version
  } else {
    version <- "latest"
  }
  env <- fromJSON(system.file("settings.json", package = "econdatar"))
  if (nchar(Sys.getenv("ECONDATA_URL")) != 0) {
    env$repository$url <- Sys.getenv("ECONDATA_URL")
    env$registry$url <- Sys.getenv("ECONDATA_URL")
  }
  if (nchar(Sys.getenv("ECONDATA_AUTH_URL")) != 0) {
    env$auth$url <- Sys.getenv("ECONDATA_AUTH_URL")
  }
  params$env <- env


  # Fetch structure(s) ----

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
  agencyids <- paste(agencyid, collapse = ",")
  ids <- paste(id, collapse = ",")
  versions <- paste(version, collapse = ",")
  structure_data <-
    switch(structure,
           "agency-scheme" =
           read_agency_schemes(agencyids, ids, versions, params),
           "category-scheme" =
           read_category_schemes(agencyids, ids, versions, params),
           "codelist" =
           read_codelists(agencyids, ids, versions, params),
           "concept-scheme" =
           read_concept_schemes(agencyids, ids, versions, params),
           "data-consumer-scheme" =
           read_data_consumer_schemes(agencyids, ids, versions, params),
           "data-provider-scheme" =
           read_data_provider_schemes(agencyids, ids, versions, params),
           "dataflow" =
           read_dataflows(agencyids, ids, versions, params),
           "data-structure" =
           read_data_structures(agencyids, ids, versions, params),
           "memberlist" =
           read_memberlist(agencyids, ids, versions, params),
           "consumption-agreement" =
           read_cons_agreement(agencyids, ids, versions, params),
           "provision-agreement" =
           read_prov_agreement(agencyids, ids, versions, params),
           stop("Specified structure, ", structure, ", is not supported."))


  # Process structures ----

  structures <- lapply(structure_data, function(x) {
    switch(structure,
           "agency-scheme" = process_agency_scheme(x, params),
           "category-scheme" = process_category_scheme(x, params),
           "codelist" = process_codelist(x, params),
           "concept-scheme" = process_concept_scheme(x, params),
           "data-consumer-scheme" = process_data_consumer_scheme(x, params),
           "data-provider-scheme" = process_data_provider_scheme(x, params),
           "dataflow" = process_dataflow(x, params),
           "data-structure" = process_data_structure(x, params),
           "memberlist" = process_memberlist(x, params),
           "consumption-agreement" = process_cons_agreement(x, params),
           "provision-agreement" = process_prov_agreement(x, params),
           stop("Specified structure, ", structure, ", is not supported."))
  })
  if (length(structures) == 1) {
    return(structures[[1]])
  } else {
    return(structures)
  }
}



# Agency schemes ----


read_agency_schemes <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching agency scheme(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "agencyschemes"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    agency_schemes <- data_message[[2]][["structures"]][["agency-schemes"]]
    return(agency_schemes)
  } else {
    message(paste("\nFetching agency scheme(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    agencies <- read_ods(path = params$file,
                         sheet = "agencies",
                         na = na,
                         as_tibble = FALSE)
    agency_scheme <- as.list(read_ods(path = params$file,
                                      sheet = "agency_scheme",
                                      na = na,
                                      as_tibble = FALSE))
    agency_scheme$agencies <- agencies
    return(list(agency_scheme))
  }
}

process_agency_scheme <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing agency scheme: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    agency_scheme <- list(agencyid = structure[[2]]$agencyid,
                          id = structure[[2]]$id,
                          version = structure[[2]]$version,
                          name = structure[[2]]$name[[2]],
                          description = description)
    agencies <- lapply(structure[[2]]$agencies, function(agency) {
      description <- if (is.null(agency[[2]]$description[[2]])) {
        NA
      } else {
        agency[[2]]$description[[2]]
      }
      if (length(agency[[2]]$contacts) == 0) {
        list(id = agency[[2]]$id,
             name = agency[[2]]$name[[2]],
             description = description,
             contact_name = NA,
             contact_department = NA,
             contact_email = NA)
      } else {
        lapply(agency[[2]]$contacts, function(contact) {
          department <- if (is.null(contact$department[[2]])) {
            NA
          } else {
            contact$department[[2]]
          }
          email <- if (is.null(contact$email)) {
            NA
          } else {
            contact$email
          }
          list(id = agency[[2]]$id,
               name = agency[[2]]$name[[2]],
               description = description,
               contact_name = contact$name[[2]],
               contact_department = department,
               contact_email = email)
        }) |>
          do.call(rbind.data.frame, args = _)
      }
    }) |>
    do.call(rbind.data.frame, args = _)
  agency_scheme$agencies <- agencies
  class(agency_scheme) <- c(class(agency_scheme), "eds_agency_scheme")
  return(agency_scheme)
  } else {
    message("Processing agency scheme: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_agency_scheme")
    return(structure)
  }
}



# Category schemes ----


read_category_schemes <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching category scheme(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "categoryschemes"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    category_schemes <- data_message[[2]][["structures"]][["category-schemes"]]
    return(category_schemes)
  } else {
    message(paste("\nFetching category scheme(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    categories <- read_ods(path = params$file,
                           sheet = "categories",
                           na = na,
                           as_tibble = FALSE)
    category_scheme <- as.list(read_ods(path = params$file,
                                        sheet = "category_scheme",
                                        na = na,
                                        as_tibble = FALSE))
    category_scheme$categories <- categories
    return(list(category_scheme))
  }
}

process_category_scheme <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing category scheme: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    category_scheme <- list(agencyid = structure[[2]]$agencyid,
                            id = structure[[2]]$id,
                            version = structure[[2]]$version,
                            name = structure[[2]]$name[[2]],
                            description = description)
    categories <- lapply(structure[[2]]$categories, function(category) {
      description <- if (is.null(category[[2]]$description[[2]])) {
        NA
      } else {
        category[[2]]$description[[2]]
      }
      lapply(category[[2]]$references, function(reference) {
        list(id = category[[2]]$id,
             name = category[[2]]$name[[2]],
             description = description,
             reference_agencyid = reference[[2]]$agencyid,
             reference_id = reference[[2]]$id,
             reference_version = reference[[2]]$version)
      }) |>
        do.call(rbind.data.frame, args = _)
    }) |>
      do.call(rbind.data.frame, args = _)
    category_scheme$categories <- categories
    class(category_scheme) <- c(class(category_scheme), "eds_category_scheme")
    return(category_scheme)
  } else {
    message("Processing category scheme: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_category_scheme")
    return(structure)
  }
}



# Codelists ----


read_codelists <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching codelist(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "codelists"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    codelists <- data_message[[2]][["structures"]][["codelists"]]
    return(codelists)
  } else {
    message(paste("\nFetching codelist(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    codes <- read_ods(path = params$file,
                      sheet = "codes",
                      na = na,
                      as_tibble = FALSE)
    codelist <- as.list(read_ods(path = params$file,
                                 sheet = "codelist",
                                 na = na,
                                 as_tibble = FALSE))
    codelist$codes <- codes
    return(list(codelist))
  }
}

process_codelist <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing codelist: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    codelist <- list(agencyid = structure[[2]]$agencyid,
                     id = structure[[2]]$id,
                     version = structure[[2]]$version,
                     name = structure[[2]]$name[[2]],
                     description = description)
    codes <- lapply(structure[[2]]$codes, function(code) {
      description <- if (is.null(code[[2]]$description[[2]])) {
        NA
      } else {
        code[[2]]$description[[2]]
      }
      list(id = code[[2]]$id,
           name = code[[2]]$name[[2]],
           description = description)
    }) |>
      do.call(rbind.data.frame, args = _)
    codelist$codes <- codes
    class(codelist) <- c(class(codelist), "eds_codelist")
    return(codelist)
  } else {
    message("Processing codelist: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_codelist")
    return(structure)
  }
}



# Concept schemes ----


read_concept_schemes <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching concept scheme(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "conceptschemes"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    concept_schemes <- data_message[[2]][["structures"]][["concept-schemes"]]
    return(concept_schemes)
  } else {
    message(paste("\nFetching concept scheme(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    concepts <- read_ods(path = params$file,
                         sheet = "concepts",
                         na = na,
                         as_tibble = FALSE)
    concept_scheme <- as.list(read_ods(path = params$file,
                                       sheet = "concept_scheme",
                                       na = na,
                                       as_tibble = FALSE))
    concept_scheme$concepts <- concepts
    return(list(concept_scheme))
  }
}

process_concept_scheme <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing concept scheme: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    concept_scheme <- list(agencyid = structure[[2]]$agencyid,
                           id = structure[[2]]$id,
                           version = structure[[2]]$version,
                           name = structure[[2]]$name[[2]],
                           description = description)
    concepts <- lapply(structure[[2]]$concepts, function(concept) {
      description <- if (is.null(concept[[2]]$description[[2]])) {
        NA
      } else {
        concept[[2]]$description[[2]]
      }
      representation <- if (is.list(concept[[2]][["core-representation"]])) {
        codelist_ref <- concept[[2]][["core-representation"]][[2]]
        list(representation = "codelist",
             codelist_agencyid = codelist_ref$agencyid,
             codelist_id = codelist_ref$id,
             codelist_version = codelist_ref$version)
      } else {
        list(representation = concept[[2]][["core-representation"]],
             codelist_agencyid = NA,
             codelist_id = NA,
             codelist_version = NA)
      }
      c(list(id = concept[[2]]$id,
             name = concept[[2]]$name[[2]],
             description = description),
        representation)
    }) |>
      do.call(rbind.data.frame, args = _)
    concept_scheme$concepts <- concepts
    class(concept_scheme) <- c(class(concept_scheme), "eds_concept_scheme")
    return(concept_scheme)
  } else {
    message("Processing concept scheme: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_concept_scheme")
    return(structure)
  }
}



# Data consumer schemes ----


read_data_consumer_schemes <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching data consumer scheme(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "dataconsumerschemes"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    data_consumer_schemes <- data_message[[2]][["structures"]][["data-consumer-schemes"]]
    return(data_consumer_schemes)
  } else {
    message(paste("\nFetching data consumer scheme(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    data_consumers <- read_ods(path = params$file,
                               sheet = "data_consumers",
                               na = na,
                               as_tibble = FALSE)
    data_consumer_scheme <- as.list(read_ods(path = params$file,
                                             sheet = "data_consumer_scheme",
                                             na = na,
                                             as_tibble = FALSE))
    data_consumer_scheme$data_consumers <- data_consumers
    return(list(data_consumer_scheme))
  }
}

process_data_consumer_scheme <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing data consumer scheme: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    data_consumer_scheme <- list(agencyid = structure[[2]]$agencyid,
                                 id = structure[[2]]$id,
                                 version = structure[[2]]$version,
                                 name = structure[[2]]$name[[2]],
                                 description = description)
    data_consumers <- lapply(structure[[2]][["data-consumers"]], function(data_consumer) {
      description <- if (is.null(data_consumer[[2]]$description[[2]])) {
        NA
      } else {
        data_consumer[[2]]$description[[2]]
      }
      if (length(data_consumer[[2]]$contacts) == 0) {
        list(id = data_consumer[[2]]$id,
             name = data_consumer[[2]]$name[[2]],
             description = description,
             contact_name = NA,
             contact_department = NA,
             contact_email = NA)
      } else {
        lapply(data_consumer[[2]]$contacts, function(contact) {
          department <- if (is.null(contact$department[[2]])) {
            NA
          } else {
            contact$department[[2]]
          }
          email <- if (is.null(contact$email)) {
            NA
          } else {
            contact$email
          }
          list(id = data_consumer[[2]]$id,
               name = data_consumer[[2]]$name[[2]],
               description = description,
               contact_name = contact$name[[2]],
               contact_department = department,
               contact_email = email)
        }) |>
          do.call(rbind.data.frame, args = _)
      }
    }) |>
      do.call(rbind.data.frame, args = _)
    data_consumer_scheme$data_consumers <- data_consumers
    class(data_consumer_scheme) <-
      c(class(data_consumer_scheme), "eds_data_consumer_scheme")
    return(data_consumer_scheme)
  } else {
    message("Processing data consumer scheme: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_data_consumer_scheme")
    return(structure)
  }
}



# Data provider schemes ----


read_data_provider_schemes <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching data provider scheme(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "dataproviderschemes"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    data_provider_schemes <- data_message[[2]][["structures"]][["data-provider-schemes"]]
    return(data_provider_schemes)
  } else {
    message(paste("\nFetching data provider scheme(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    data_providers <- read_ods(path = params$file,
                               sheet = "data_providers",
                               na = na,
                               as_tibble = FALSE)
    data_provider_scheme <- as.list(read_ods(path = params$file,
                                             sheet = "data_provider_scheme",
                                             na = na,
                                             as_tibble = FALSE))
    data_provider_scheme$data_providers <- data_providers
    return(list(data_provider_scheme))
  }
}

process_data_provider_scheme <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing data provider scheme: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    data_provider_scheme <- list(agencyid = structure[[2]]$agencyid,
                                 id = structure[[2]]$id,
                                 version = structure[[2]]$version,
                                 name = structure[[2]]$name[[2]],
                                 description = description)
    data_providers <- lapply(structure[[2]][["data-providers"]], function(data_provider) {
      description <- if (is.null(data_provider[[2]]$description[[2]])) {
        NA
      } else {
        data_provider[[2]]$description[[2]]
      }
      if (length(data_provider[[2]]$contacts) == 0) {
        list(id = data_provider[[2]]$id,
             name = data_provider[[2]]$name[[2]],
             description = description,
             contact_name = NA,
             contact_department = NA,
             contact_email = NA)
      } else {
        lapply(data_provider[[2]]$contacts, function(contact) {
          department <- if (is.null(contact$department[[2]])) {
            NA
          } else {
            contact$department[[2]]
          }
          email <- if (is.null(contact$email)) {
            NA
          } else {
            contact$email
          }
          list(id = data_provider[[2]]$id,
               name = data_provider[[2]]$name[[2]],
               description = description,
               contact_name = contact$name[[2]],
               contact_department = department,
               contact_email = email)
        }) |>
          do.call(rbind.data.frame, args = _)
      }
    }) |>
      do.call(rbind.data.frame, args = _)
    data_provider_scheme$data_providers <- data_providers
    class(data_provider_scheme) <-
      c(class(data_provider_scheme), "eds_data_provider_scheme")
    return(data_provider_scheme)
  } else {
    message("Processing data provider scheme: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_data_provider_scheme")
    return(structure)
  }
}



# Dataflow ----


read_dataflows <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching dataflow(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "dataflows"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    dataflows <- data_message[[2]][["structures"]][["dataflows"]]
    return(dataflows)
  } else {
    message(paste("\nFetching dataflow(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    data_structure <- as.list(read_ods(path = params$file,
                                       sheet = "data_structure",
                                       na = na,
                                       as_tibble = FALSE))
    dataflow <- as.list(read_ods(path = params$file,
                                 sheet = "dataflow",
                                 na = na,
                                 as_tibble = FALSE))
    dataflow$data_structure <- data_structure
    return(list(dataflow))
  }
}

process_dataflow <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing dataflow: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    dataflow <- list(agencyid = structure[[2]]$agencyid,
                     id = structure[[2]]$id,
                     version = structure[[2]]$version,
                     name = structure[[2]]$name[[2]],
                     description = description)
    data_structure <-
      list(agencyid = structure[[2]][["data-structure"]][[2]]$agencyid,
           id = structure[[2]][["data-structure"]][[2]]$id,
           version = structure[[2]][["data-structure"]][[2]]$version)
    dataflow$data_structure <- data_structure
    class(dataflow) <- c(class(dataflow), "eds_dataflow")
    return(dataflow)
  } else {
    message("Processing dataflow: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_dataflow")
    return(structure)
  }
}


# Data structures ----


read_data_structures <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching data structure(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "datastructures"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    data_structures <- data_message[[2]][["structures"]][["data-structures"]]
    return(data_structures)
  } else {
    message(paste("\nFetching codelist(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    dimensions <- read_ods(path = params$file,
                           sheet = "dimensions",
                           na = na,
                           as_tibble = FALSE)
    attrs <- read_ods(path = params$file,
                      sheet = "attributes",
                      na = na,
                      as_tibble = FALSE)
    time_dimension <- read_ods(path = params$file,
                               sheet = "time_dimension",
                               na = na,
                               as_tibble = FALSE)
    primary_measure <- read_ods(path = params$file,
                                sheet = "primary_measure",
                                na = na,
                                as_tibble = FALSE)
    data_structure <- as.list(read_ods(path = params$file,
                                       sheet = "data_structure",
                                       na = na,
                                       as_tibble = FALSE))
    data_structure$dimensions <- dimensions
    data_structure$attributes <- attrs
    data_structure$time_dimension <- time_dimension
    data_structure$primary_measure <- primary_measure
    return(list(data_structure))
  }
}

process_data_structure <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing data structure: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    data_structure <- list(agencyid = structure[[2]]$agencyid,
                           id = structure[[2]]$id,
                           version = structure[[2]]$version,
                           name = structure[[2]]$name[[2]],
                           description = description)
    dimensions <- lapply(structure[[2]]$components, function(component) {
      if (component[[1]] == "#sdmx.infomodel.datastructure.Dimension") {
        concept_ref <- component[[2]][["concept-identity"]][[2]]
        concept <- list(concept_agencyid = concept_ref$agencyid,
                        concept_parentid = concept_ref$parentid,
                        concept_parentversion = concept_ref$parentversion,
                        concept_id = concept_ref$id)
        representation <-
          if (is.list(component[[2]][["local-representation"]])) {
            codelist_ref <- component[[2]][["local-representation"]][[2]]
            list(representation = "codelist",
                 codelist_agencyid = codelist_ref$agencyid,
                 codelist_id = codelist_ref$id,
                 codelist_version = codelist_ref$version)
          } else {
            list(representation = component[[2]][["local-representation"]],
                 codelist_agencyid = NA,
                 codelist_id = NA,
                 codelist_version = NA)
          }
        c(list(id = component[[2]]$id,
               position = component[[2]]$position),
          concept,
          representation)
      }
    }) |>
      do.call(rbind.data.frame, args = _)
    dimensions_ordered <- dimensions[order(dimensions$position), ]
    rownames(dimensions_ordered) <- NULL
    attrs <- lapply(structure[[2]]$components, function(component) {
      if (component[[1]] == "#sdmx.infomodel.datastructure.Attribute") {
        concept_ref <- component[[2]][["concept-identity"]][[2]]
        concept <- list(concept_agencyid = concept_ref$agencyid,
                        concept_parentid = concept_ref$parentid,
                        concept_parentversion = concept_ref$parentversion,
                        concept_id = concept_ref$id)
        representation <-
          if (is.list(component[[2]][["local-representation"]])) {
            codelist_ref <- component[[2]][["local-representation"]][[2]]
            list(representation = "codelist",
                 codelist_agencyid = codelist_ref$agencyid,
                 codelist_id = codelist_ref$id,
                 codelist_version = codelist_ref$version)
          } else {
            list(representation = component[[2]][["local-representation"]],
                 codelist_agencyid = NA,
                 codelist_id = NA,
                 codelist_version = NA)
          }
        c(list(id = component[[2]]$id,
               level = component[[2]][["attachment-level"]],
               mandatory = component[[2]][["assignment-mandatory"]]),
          concept,
          representation)
      }
    }) |>
      do.call(rbind.data.frame, args = _)
    attrs_ordered <- attrs[order(attrs$level), ]
    rownames(attrs_ordered) <- NULL
    time_dimension <- lapply(structure[[2]]$components, function(component) {
      if (component[[1]] == "#sdmx.infomodel.datastructure.TimeDimension") {
        concept_ref <- component[[2]][["concept-identity"]][[2]]
        concept <- list(concept_agencyid = concept_ref$agencyid,
                        concept_parentid = concept_ref$parentid,
                        concept_parentversion = concept_ref$parentversion,
                        concept_id = concept_ref$id)
        representation <-
          list(representation = component[[2]][["local-representation"]])
        c(list(id = component[[2]]$id),
          concept,
          representation)
      }
    }) |>
      do.call(rbind.data.frame, args = _)
    primary_measure <- lapply(structure[[2]]$components, function(component) {
      if (component[[1]] == "#sdmx.infomodel.datastructure.PrimaryMeasure") {
        concept_ref <- component[[2]][["concept-identity"]][[2]]
        concept <- list(concept_agencyid = concept_ref$agencyid,
                        concept_parentid = concept_ref$parentid,
                        concept_parentversion = concept_ref$parentversion,
                        concept_id = concept_ref$id)
        representation <-
          list(representation = component[[2]][["local-representation"]])
        c(list(id = component[[2]]$id),
          concept,
          representation)
      }
    }) |>
      do.call(rbind.data.frame, args = _)
    data_structure$dimensions <- dimensions_ordered
    data_structure$attributes <- attrs_ordered
    data_structure$primary_measure <- primary_measure
    data_structure$time_dimension <- time_dimension
    class(data_structure) <- c(class(data_structure), "eds_data_structure")
    return(data_structure)
  } else {
    message("Processing codelist: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_data_structure")
    return(structure)
  }
}



# Memberlist ----


read_memberlist <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching memberlist(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "memberlists"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    add_headers(authorization = get("econdata_token",
                                                    envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <- content(response, type = "application/json")
    memberlists <- data_message[[2]][["structures"]][["memberlists"]]
    return(memberlists)
  } else {
    message(paste("\nFetching memberlist(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    members <- read_ods(path = params$file,
                        sheet = "members",
                        na = na,
                        as_tibble = FALSE)
    memberlist <- as.list(read_ods(path = params$file,
                                   sheet = "memberlist",
                                   na = na,
                                   as_tibble = FALSE))
    memberlist$members <- members
    return(list(memberlist))
  }
}

process_memberlist <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing memberlist: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    memberlist <- list(agencyid = structure[[2]]$agencyid,
                       id = structure[[2]]$id,
                       version = structure[[2]]$version,
                       name = structure[[2]]$name[[2]],
                       description = description)
    members <- lapply(structure[[2]]$members, function(member) {
      lapply(member[[2]]$memberships, function(membership) {
        m <- list(id = member[[2]]$id,
                  email = member[[2]]$email,
                  firstname = member[[2]]$firstname,
                  lastname = member[[2]]$lastname,
                  annotations = toJSON(lapply(member[[2]]$annotations, unbox)))
        if (membership[[1]] == "#sdmx.infomodel.base.DataConsumerRef") {
          c(m,
            list(membership_type = "data consumer",
                 membership_agencyid = membership[[2]]$agencyid,
                 membership_parentid = membership[[2]]$parentid,
                 membership_parentversion = membership[[2]]$parentversion,
                 membership_id = membership[[2]]$id))
        } else if (membership[[1]] == "#sdmx.infomodel.base.DataProviderRef") {
          c(m,
            list(membership_type = "data provider",
                 membership_agencyid = membership[[2]]$agencyid,
                 membership_parentid = membership[[2]]$parentid,
                 membership_parentversion = membership[[2]]$parentversion,
                 membership_id = membership[[2]]$id))
        } else {
          stop("Unable to parse reference type: ", membership[[1]])
        }
      }) |>
        do.call(rbind.data.frame, args = _)
    }) |>
      do.call(rbind.data.frame, args = _)
    memberlist$members <- members
    class(memberlist) <- c(class(memberlist), "memberlist")
    return(memberlist)
  } else {
    message("Processing memberlist: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_memberlist")
    return(structure)
  }
}



# Consumption agreement ----


read_cons_agreement <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching consumption agreement(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <-
      GET(params$env$registry$url,
          path = paste(c(params$env$registry$path, "consumptionagreements"),
                       collapse = "/"),
          query = list(agencyids = agencyids,
                       ids = ids,
                       versions = versions),
          add_headers(authorization = get("econdata_token", envir = .pkgenv)),
          accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <-
      content(response, type = "application/json")
    cons_agreements <-
      data_message[[2]][["structures"]][["consumption-agreements"]]
    return(cons_agreements)
  } else {
    message(paste("\nFetching consumption agreement(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    dataflow <- as.list(read_ods(path = params$file,
                                 sheet = "dataflow",
                                 na = na,
                                 as_tibble = FALSE))
    data_consumer <- as.list(read_ods(path = params$file,
                                      sheet = "data_consumer",
                                      na = na,
                                      as_tibble = FALSE))
    cons_agreement <- as.list(read_ods(path = params$file,
                                       sheet = "consumption_agreement",
                                       na = na,
                                       as_tibble = FALSE))
    cons_agreement$dataflow <- dataflow
    cons_agreement$data_consumer <- data_consumer
    return(list(cons_agreement))
  }
}

process_cons_agreement <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing consumption agreement: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    cons_agreement <- list(agencyid = structure[[2]]$agencyid,
                           id = structure[[2]]$id,
                           version = structure[[2]]$version,
                           name = structure[[2]]$name[[2]],
                           description = description)
    dataflow <- list(agencyid = structure[[2]][["dataflow"]][[2]]$agencyid,
                     id = structure[[2]][["dataflow"]][[2]]$id,
                     version = structure[[2]][["dataflow"]][[2]]$version)
    data_consumer <-
      list(agencyid = structure[[2]][["data-consumer"]][[2]]$agencyid,
           parentid = structure[[2]][["data-consumer"]][[2]]$parentid,
           parentversion = structure[[2]][["data-consumer"]][[2]]$parentversion,
           id = structure[[2]][["data-consumer"]][[2]]$id)
    cons_agreement$dataflow <- dataflow
    cons_agreement$data_consumer <- data_consumer
    class(cons_agreement) <- c(class(cons_agreement),
                               "eds_consumption_agreement")
    return(cons_agreement)
  } else {
    message("Processing consumption agreement: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_consumption_agreement")
    return(structure)
  }
}



# Provision agreement ----


read_prov_agreement <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching provision agreement(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <-
      GET(params$env$registry$url,
          path = paste(c(params$env$registry$path, "provisionagreements"),
                       collapse = "/"),
          query = list(agencyids = agencyids,
                       ids = ids,
                       versions = versions),
          add_headers(authorization = get("econdata_token", envir = .pkgenv)),
          accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, type = "application/json"))
    }
    data_message <-
      content(response, type = "application/json")
    prov_agreements <-
      data_message[[2]][["structures"]][["provision-agreements"]]
    return(prov_agreements)
  } else {
    message(paste("\nFetching provision agreement(s) -", params$file, "\n"))
    na <- c("", "NA", "#N/A")
    dataflow <- as.list(read_ods(path = params$file,
                                 sheet = "dataflow",
                                 na = na,
                                 as_tibble = FALSE))
    data_provider <- as.list(read_ods(path = params$file,
                                      sheet = "data_provider",
                                      na = na,
                                      as_tibble = FALSE))
    prov_agreement <- as.list(read_ods(path = params$file,
                                       sheet = "provision_agreement",
                                       na = na,
                                       as_tibble = FALSE))
    prov_agreement$dataflow <- dataflow
    prov_agreement$data_provider <- data_provider
    return(list(prov_agreement))
  }
}

process_prov_agreement <- function(structure, params) {
  if (is.null(params$file)) {
    structure_ref <- paste(structure[[2]]$agencyid,
                           structure[[2]]$id,
                           structure[[2]]$version,
                           sep = "-")
    message("Processing provision agreement: ", structure_ref, "\n")
    description <- if (is.null(structure[[2]]$description[[2]])) {
      NA
    } else {
      structure[[2]]$description[[2]]
    }
    prov_agreement <- list(agencyid = structure[[2]]$agencyid,
                           id = structure[[2]]$id,
                           version = structure[[2]]$version,
                           name = structure[[2]]$name[[2]],
                           description = description)
    dataflow <- list(agencyid = structure[[2]][["dataflow"]][[2]]$agencyid,
                     id = structure[[2]][["dataflow"]][[2]]$id,
                     version = structure[[2]][["dataflow"]][[2]]$version)
    data_provider <-
      list(agencyid = structure[[2]][["data-provider"]][[2]]$agencyid,
           parentid = structure[[2]][["data-provider"]][[2]]$parentid,
           parentversion = structure[[2]][["data-provider"]][[2]]$parentversion,
           id = structure[[2]][["data-provider"]][[2]]$id)
    prov_agreement$dataflow <- dataflow
    prov_agreement$data_provider <- data_provider
    class(prov_agreement) <- c(class(prov_agreement), "eds_provsion_agreement")
    return(prov_agreement)
  } else {
    message("Processing provision agreement: ", params$file, "\n")
    class(structure) <- c(class(structure), "eds_provision_agreement")
    return(structure)
  }
}
