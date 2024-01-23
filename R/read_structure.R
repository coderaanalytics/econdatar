read_structure <- function(structure, ...) {


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

  if (!is.null(params$file)) {
    file  <- params$file
  } else {
    file <- NULL
  }

  if (is.null(id) && is.null(file)) {
    stop("At least one of either: 'id' or 'file' parameter required.")
  }


  # Fetch structure(s) ---

  if (!exists("econdata_session", envir = .pkgenv)) {
    login_helper(credentials, env$repository$url)
  }

  agencyids <- paste(agencyid, collapse = ",")
  ids <- paste(id, collapse = ",")
  versions <- paste(version, collapse = ",")

  structure_data <-
    switch(structure,
           "codelist" = read_codelists(env, agencyids, ids, versions, file),
           "concept-scheme" = read_concept_schemes(env, agencyids, ids, versions, file),
           "data-structure" = read_data_structures(env, agencyids, ids, versions, file),
           stop("Specified structure, ", structure, ", is not supported."))


  # Process structures ---

  structures <- lapply(structure_data, function(x) {
    switch(structure,
           "codelist" = process_codelist(x, file),
           "concept-scheme" = process_concept_scheme(x, file),
           "data-structure" = process_data_structure(x, file),
           stop("Specified structure, ", structure, ", is not supported."))
  })

  if (length(structures) == 1) {
    return(structures[[1]])
  } else {
    return(structures)
  }
}



# Codelists ---


read_codelists <- function(env, agencyids, ids, versions, file) {
  if (is.null(file)) {
    message(paste("\nFetching codelist(s) -", paste(ids, collapse = ", "), "\n"))

    response <- GET(env$registry$url,
                    path = paste(c(env$registry$path, "codelists"), collapse = "/"),
                    query = list(agencyids = agencyids, ids = ids, versions = versions),
                    set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }

    data_message <- content(response, type = "application/json", encoding = "UTF-8")

    codelists <- data_message[[2]][["structures"]][["codelists"]]

    return(codelists)
  } else {
    message(paste("\nFetching codelist(s) -", file, "\n"))

    na <- c("","NA", "#N/A")

    codes <- read_ods(path = file, sheet = "codes", na = na)
    codelist <- as.list(read_ods(path = file, sheet = "codelist", na = na))

    codelist$codes <- codes

    return(list(codelist))
  }
}

process_codelist <- function(structure, file) {
  if (is.null(file)) {
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

    return(codelist)
  } else {
    message("Processing codelist: ", file, "\n")

    return(structure)
  }
}



# Concept schemes ---


read_concept_schemes <- function(env, agencyids, ids, versions, file) {
  if (is.null(file)) {
    message(paste("\nFetching concept scheme(s) -", paste(ids, collapse = ", "), "\n"))

    response <- GET(env$registry$url,
                    path = paste(c(env$registry$path, "conceptschemes"), collapse = "/"),
                    query = list(agencyids = agencyids, ids = ids, versions = versions),
                    set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }

    data_message <- content(response, type = "application/json", encoding = "UTF-8")

    concept_schemes <- data_message[[2]][["structures"]][["concept-schemes"]]

    return(concept_schemes)
  } else {
    message(paste("\nFetching codelist(s) -", file, "\n"))

    na <- c("","NA", "#N/A")

    concepts <- read_ods(path = file, sheet = "concepts", na = na)
    concept_scheme <- as.list(read_ods(path = file, sheet = "concept_scheme", na = na))

    concept_scheme$conceps <- concepts

    return(list(concept_scheme))
  }
}

process_concept_scheme <- function(structure, file) {
  if (is.null(file)) {
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
          codelist_ref <- component[[2]][["core-representation"]][[2]]
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

    return(concept_scheme)
  } else {
    message("Processing codelist: ", file, "\n")

    return(structure)
  }
}



# Data structures ---


read_data_structures <- function(env, agencyids, ids, versions, file) {
  if (is.null(file)) {
    message(paste("\nFetching data structure(s) -", paste(ids, collapse = ", "), "\n"))

    response <- GET(env$registry$url,
                    path = paste(c(env$registry$path, "datastructures"), collapse = "/"),
                    query = list(agencyids = agencyids, ids = ids, versions = versions),
                    set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                    accept("application/vnd.sdmx-codera.data+json"))

    if (response$status_code != 200) {
      stop(content(response, encoding = "UTF-8"))
    }

    data_message <- content(response, type = "application/json", encoding = "UTF-8")

    data_structures <- data_message[[2]][["structures"]][["data-structures"]]

    return(data_structures)
  } else {
    message(paste("\nFetching codelist(s) -", file, "\n"))

    na <- c("","NA", "#N/A")

    dimensions <- read_ods(path = file, sheet = "dimensions", na = na)
    attrs <- read_ods(path = file, sheet = "attributes", na = na)
    time_dimension <- read_ods(path = file, sheet = "time_dimension", na = na)
    primary_measure <- read_ods(path = file, sheet = "primary_measure", na = na)
    data_structure <- as.list(read_ods(path = file, sheet = "data_structure", na = na))

    data_structure$dimensions <- dimensions
    data_structure$attrs <- attrs
    data_structure$time_dimension <- time_dimension
    data_structure$primary_measure <- primary_measure

    return(list(data_structure))
  }
}

process_data_structure <- function(structure, file) {
  if (is.null(file)) {
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

          representation <- if (is.list(component[[2]][["local-representation"]])) {
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

    dimensions_ordered <- dimensions[order(dimensions$position),]
    rownames(dimensions_ordered) <- NULL

    attrs <- lapply(structure[[2]]$components, function(component) {
        if (component[[1]] == "#sdmx.infomodel.datastructure.Attribute") {
          concept_ref <- component[[2]][["concept-identity"]][[2]]
          concept <- list(concept_agencyid = concept_ref$agencyid,
                          concept_parentid = concept_ref$parentid,
                          concept_parentversion = concept_ref$parentversion,
                          concept_id = concept_ref$id)

          representation <- if (is.list(component[[2]][["local-representation"]])) {
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

    attrs_ordered <- attrs[order(attrs$level),]
    rownames(attrs_ordered) <- NULL

    time_dimension <- lapply(structure[[2]]$components, function(component) {
        if (component[[1]] == "#sdmx.infomodel.datastructure.TimeDimension") {
          concept_ref <- component[[2]][["concept-identity"]][[2]]
          concept <- list(concept_agencyid = concept_ref$agencyid,
                          concept_parentid = concept_ref$parentid,
                          concept_parentversion = concept_ref$parentversion,
                          concept_id = concept_ref$id)

          representation <- list(representation = component[[2]][["local-representation"]])

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

          representation <- list(representation = component[[2]][["local-representation"]])
        
          c(list(id = component[[2]]$id),
            concept,
            representation)
        }
      }) |>
      do.call(rbind.data.frame, args = _)

    components <- list(dimensions = dimensions_ordered,
                       attributes = attrs_ordered,
                       primary_measure = primary_measure,
                       time_dimension = time_dimension)

    data_structure$components <- components

    return(data_structure)
  } else {
    message("Processing codelist: ", file, "\n")

    return(structure)
  }
}
