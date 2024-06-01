read_registry <- function(structure, tidy = FALSE, ...) {


  # Parameters ----

  params <- list(...)
  if (is.null(params$id) && is.null(params$file)) {
    stop("At least one of either: 'id' or 'file' parameter required.")
  }
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
  params$env <- env


  # Fetch structure(s) ----

  if (!exists("econdata_session", envir = .pkgenv)) {
    login_helper(credentials, env$repository$url)
  }
  agencyids <- paste(agencyid, collapse = ",")
  ids <- paste(id, collapse = ",")
  versions <- paste(version, collapse = ",")
  structure_data <-
    switch(structure,
           "category-scheme" =
           read_category_schemes(agencyids, ids, versions, params),
           "codelist" =
           read_codelists(agencyids, ids, versions, params),
           "concept-scheme" =
           read_concept_schemes(agencyids, ids, versions, params),
           "dataflow" =
           read_dataflow(agencyids, ids, versions, params),
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
           "category-scheme" = process_category_scheme(x, params),
           "codelist" = process_codelist(x, params),
           "concept-scheme" = process_concept_scheme(x, params),
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
                    set_cookies(.cookies =
                                  get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <- content(response,
                            type = "application/json",
                            encoding = "UTF-8")
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
                    set_cookies(.cookies =
                                  get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <- content(response,
                            type = "application/json",
                            encoding = "UTF-8")
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
                    set_cookies(.cookies =
                                  get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <- content(response,
                            type = "application/json",
                            encoding = "UTF-8")
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



# Dataflow ----


read_dataflow <- function(agencyids, ids, versions, params) {
  if (is.null(params$file)) {
    message(paste("\nFetching dataflow(s) -",
                  paste(ids, collapse = ", "), "\n"))
    response <- GET(params$env$registry$url,
                    path = paste(c(params$env$registry$path, "dataflows"),
                                 collapse = "/"),
                    query = list(agencyids = agencyids,
                                 ids = ids,
                                 versions = versions),
                    set_cookies(.cookies =
                                  get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <- content(response,
                            type = "application/json",
                            encoding = "UTF-8")
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
                    set_cookies(.cookies =
                                  get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <- content(response,
                            type = "application/json",
                            encoding = "UTF-8")
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
                    set_cookies(.cookies =
                                  get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))
    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <- content(response,
                            type = "application/json",
                            encoding = "UTF-8")
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
          set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
          accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <-
      content(response, type = "application/json", encoding = "UTF-8")
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
          set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
          accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }
    data_message <-
      content(response, type = "application/json", encoding = "UTF-8")
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
