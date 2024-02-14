write_registry <- function(structure, x, create = FALSE, ...) {


  # Parameters ---

  env <- fromJSON(system.file("settings.json", package = "econdatar"))
  params <- list(...)
  params$env <- env
  if (!is.null(params$username) && !is.null(params$password)) {
    credentials <- paste(params$username, params$password, sep = ";")
  } else {
    credentials <- NULL
  }


  # Fetch structure(s) ---

  if (!exists("econdata_session", envir = .pkgenv)) {
    login_helper(credentials, env$repository$url)
  }
  header <- list()
  if(is.null(params$file)) {
    header$id <- unbox("ECONDATAR")
    header$prepared <- unbox(format(Sys.time(), format = "%Y-%m-%dT%T"))
    header$sender <- tryCatch(unbox(Sys.getenv()[["USER"]]),
                              error = function(e) unbox("Anonymous"))
    header$receiver <- unbox("EconData web application")
  }
  params$header <- header
  structure_data <-
    switch(structure,
           "codelist" = write_codelist(x, create, params),
           "concept-scheme" = write_concept_scheme(x, create, params),
           "dataflow" = write_dataflow(x, create, params),
           "data-structure" = write_data_structure(x, create, params),
           "consumption-agreement" = write_cons_agreement(x, create, params),
           "provision-agreement" = write_prov_agreement(x, create, params),
           stop("Specified structure, ", structure, ", is not supported."))
}



# Codelist ---


write_codelist <- function(codelist, create, params) {
  if(is.null(params$file)) {
    codelist_ref <- paste(codelist$agencyid,
                          codelist$id,
                          codelist$version,
                          sep = "-")
    data_message <-
      list(unbox("#sdmx.infomodel.message.SDMXMessage"),
           list(header = params$header,
                structures =
                  list(codelists =
                     list(
                       list(unbox("#sdmx.infomodel.codelist.Codelist"),
                          list(agencyid = unbox(codelist$agencyid),
                               id = unbox(codelist$id),
                               version = unbox(codelist$version),
                               name = c("en", codelist$name),
                               codes = list()))))))
    if (!is.na(codelist$description)) {
      data_message[[2]]$structures$codelists[[1]][[2]]$description <-
        c("en", codelist$description)
    }
    for (i in seq_len(NROW(codelist$codes))) {
      tmp <- as.list(codelist$codes[i,])
      code <- list(unbox("#sdmx.infomodel.codelist.Code"),
                   list(id = unbox(tmp$id),
                        name = c("en", tmp$name)))
      if (!is.na(tmp$description)) {
        code[[2]]$description <- c("en", tmp$description)
      }
      data_message[[2]]$structures$codelists[[1]][[2]]$codes[[i]] <- code
    }
    if (create) {
      message("Creating codelist: ", codelist_ref, "\n")
      response <- POST(params$env$repository$url,
                       path = paste(params$env$repository$path,
                                    "codelists", sep = "/"),
                       body = toJSON(data_message, na = "null"),
                       set_cookies(.cookies = get("econdata_session",
                                                  envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 201) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    } else {
      message("Updating codelist: ", codelist_ref, "\n")
      response <- PUT(params$env$repository$url,
                      path = paste(params$env$repository$path,
                                   "codelists",
                                   codelist_ref, sep = "/"),
                      body = toJSON(data_message, na = "null"),
                      set_cookies(.cookies = get("econdata_session",
                                                 envir = .pkgenv)),
                      content_type("application/vnd.sdmx-codera.data+json"),
                      accept_json())
      if (response$status_code == 200) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    }
  } else {
    codes <- codelist$codes
    codelist$codes <- NULL
    write_ods(as.data.frame(codelist), path = params$file, sheet = "codelist")
    write_ods(codes, path = params$file, sheet = "codes", append = TRUE)
    message("Codelist successfully written to: ", params$file, "\n")
  }
}



# Concept scheme ---


write_concept_scheme <- function(concept_scheme, create, params) {
  if(is.null(params$file)) {
    concept_scheme_ref <- paste(concept_scheme$agencyid,
                                concept_scheme$id,
                                concept_scheme$version,
                                sep = "-")
    data_message <-
      list(unbox("#sdmx.infomodel.message.SDMXMessage"),
           list(header = params$header,
                structures =
                  list("concept-schemes" =
                     list(
                       list(unbox("#sdmx.infomodel.conceptscheme.ConceptScheme"),
                          list(agencyid = unbox(concept_scheme$agencyid),
                               id = unbox(concept_scheme$id),
                               version = unbox(concept_scheme$version),
                               name = c("en", concept_scheme$name),
                               concepts = list()))))))
    if (!is.na(concept_scheme$description)) {
      data_message[[2]]$structures[["concept-schemes"]][[1]][[2]]$description <-
        c("en", concept_scheme$description)
    }
    for (i in seq_len(NROW(concept_scheme$concepts))) {
      tmp <- as.list(concept_scheme$concepts[i,])
      concept <- list(unbox("#sdmx.infomodel.conceptscheme.Concept"),
                      list(id = unbox(tmp$id),
                           name = c("en", tmp$name)))
      if (!is.na(tmp$description)) {
        concept[[2]]$description <- c("en", tmp$description)
      }
      if (tmp$representation == "codelist") {
        codelist_ref <- list(unbox("#sdmx.infomodel.codelist.CodelistRef"),
                             list(agencyid = unbox(tmp$codelist_agencyid),
                                  id = unbox(tmp$codelist_id),
                                  version = unbox(tmp$codelist_version)))
        concept[[2]][["core-representation"]] <- codelist_ref
      } else {
        concept[[2]][["core-representation"]] <- unbox(tmp$representation)
      }
      data_message[[2]]$structures[["concept-schemes"]][[1]][[2]]$concepts[[i]] <- concept
    }
    if (create) {
      message("Creating concept scheme: ", concept_scheme_ref, "\n")
      response <- POST(params$env$repository$url,
                       path = paste(params$env$repository$path,
                                    "conceptschemes", sep = "/"),
                       body = toJSON(data_message, na = "null"),
                       set_cookies(.cookies = get("econdata_session",
                                                  envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 201) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    } else {
      message("Updating concept scheme: ", concept_scheme_ref, "\n")
      response <- PUT(params$env$repository$url,
                      path = paste(params$env$repository$path,
                                   "conceptschemes",
                                   concept_scheme_ref, sep = "/"),
                      body = toJSON(data_message, na = "null"),
                      set_cookies(.cookies = get("econdata_session",
                                                 envir = .pkgenv)),
                      content_type("application/vnd.sdmx-codera.data+json"),
                      accept_json())
      if (response$status_code == 200) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    }
  } else {
    concepts <- concept_scheme$concepts
    concept_scheme$concepts <- NULL
    write_ods(as.data.frame(concept_scheme), path = params$file, sheet = "concept_scheme")
    write_ods(concepts, path = params$file, sheet = "concepts", append = TRUE)
    message("Concept scheme successfully written to: ", params$file, "\n")
  }
}



# Dataflow ---


write_dataflow <- function(dataflow, create, params) {
  if(is.null(params$file)) {
    dataflow_ref <- paste(dataflow$agencyid,
                          dataflow$id,
                          dataflow$version,
                          sep = "-")
    data_message <-
      list(unbox("#sdmx.infomodel.message.SDMXMessage"),
           list(header = params$header,
                structures =
                  list(dataflows =
                     list(
                       list(unbox("#sdmx.infomodel.datastructure.Dataflow"),
                          list(agencyid = unbox(dataflow$agencyid),
                               id = unbox(dataflow$id),
                               version = unbox(dataflow$version),
                               name = c("en", dataflow$name)))))))
    if (!is.na(dataflow$description)) {
      data_message[[2]]$structures$dataflows[[1]][[2]]$description <-
        c("en", dataflow$description)
    }
    data_message[[2]]$structures$dataflows[[1]][[2]][["data-structure"]] <-
      list(unbox("#sdmx.infomodel.datastructure.DataStructureRef"),
           list(agencyid = unbox(dataflow$data_structure$agencyid),
                id = unbox(dataflow$data_structure$id),
                version = unbox(dataflow$data_structure$version)))
    if (create) {
      message("Creating dataflow: ", dataflow_ref, "\n")
      response <- POST(params$env$repository$url,
                       path = paste(params$env$repository$path,
                                    "dataflows", sep = "/"),
                       body = toJSON(data_message, na = "null"),
                       set_cookies(.cookies = get("econdata_session",
                                                  envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 201) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    } else {
      message("Updating dataflow: ", dataflow_ref, "\n")
      response <- PUT(params$env$repository$url,
                      path = paste(params$env$repository$path,
                                   "dataflows",
                                   dataflow_ref, sep = "/"),
                      body = toJSON(data_message, na = "null"),
                      set_cookies(.cookies = get("econdata_session",
                                                 envir = .pkgenv)),
                      content_type("application/vnd.sdmx-codera.data+json"),
                      accept_json())
      if (response$status_code == 200) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    }
  } else {
    data_structure <- dataflow$data_structure
    dataflow$data_structure <- NULL
    write_ods(as.data.frame(dataflow), path = params$file, sheet = "dataflow")
    write_ods(as.data.frame(data_structure), path = params$file, sheet = "data_structure", append = TRUE)
    message("Dataflow successfully written to: ", params$file, "\n")
  }
}



# Data structure ---


write_data_structure <- function(data_structure, create, params) {
  if(is.null(params$file)) {
    data_structure_ref <- paste(data_structure$agencyid,
                                data_structure$id,
                                data_structure$version,
                                sep = "-")
    data_message <-
      list(unbox("#sdmx.infomodel.message.SDMXMessage"),
           list(header = params$header,
                structures =
                  list("data-structures" =
                     list(
                       list(unbox("#sdmx.infomodel.datastructure.DataStructure"),
                          list(agencyid = unbox(data_structure$agencyid),
                               id = unbox(data_structure$id),
                               version = unbox(data_structure$version),
                               name = c("en", data_structure$name),
                               components = list()))))))
    if (!is.na(data_structure$description)) {
      data_message[[2]]$structures[["data-structures"]][[1]][[2]]$description <-
        c("en", data_structure$description)
    }


    # Dimensions ---

    for (i in seq_len(NROW(data_structure$dimensions))) {
      tmp <- as.list(data_structure$dimensions[i,])
      dimension <- list(unbox("#sdmx.infomodel.datastructure.Dimension"),
                        list(id = unbox(tmp$id),
                             position = unbox(tmp$position)))
      concept_ref <- list(unbox("#sdmx.infomodel.conceptscheme.ConceptRef"),
                          list(agencyid = unbox(tmp$concept_agencyid),
                               parentid = unbox(tmp$concept_parentid),
                               parentversion = unbox(tmp$concept_parentversion),
                               id = unbox(tmp$concept_id)))
      dimension[[2]][["concept-identity"]] <- concept_ref
      if (tmp$representation == "codelist") {
        codelist_ref <- list(unbox("#sdmx.infomodel.codelist.CodelistRef"),
                             list(agencyid = unbox(tmp$codelist_agencyid),
                                  id = unbox(tmp$codelist_id),
                                  version = unbox(tmp$codelist_version)))
        dimension[[2]][["local-representation"]] <- codelist_ref
      } else {
        dimension[[2]][["local-representation"]] <- unbox(tmp$representation)
      }
      data_message[[2]]$structures[["data-structures"]][[1]][[2]]$components[[i]] <-
        dimension
    }


    # Attributes ---

    for (i in seq_len(NROW(data_structure$attributes))) {
      tmp <- as.list(data_structure$attributes[i,])
      attribute <- list(unbox("#sdmx.infomodel.datastructure.Attribute"),
                        list(id = unbox(tmp$id),
                             "attachment-level" = unbox(tmp$level),
                             "assignment-mandatory" = unbox(tmp$mandatory)))
      concept_ref <- list(unbox("#sdmx.infomodel.conceptscheme.ConceptRef"),
                          list(agencyid = unbox(tmp$concept_agencyid),
                               parentid = unbox(tmp$concept_parentid),
                               parentversion = unbox(tmp$concept_parentversion),
                               id = unbox(tmp$concept_id)))
      attribute[[2]][["concept-identity"]] <- concept_ref
      if (tmp$representation == "codelist") {
        codelist_ref <- list(unbox("#sdmx.infomodel.codelist.CodelistRef"),
                             list(agencyid = unbox(tmp$codelist_agencyid),
                                  id = unbox(tmp$codelist_id),
                                  version = unbox(tmp$codelist_version)))
        attribute[[2]][["local-representation"]] <- codelist_ref
      } else {
        attribute[[2]][["local-representation"]] <- unbox(tmp$representation)
      }
      j <- i + NROW(data_structure$dimensions)
      data_message[[2]]$structures[["data-structures"]][[1]][[2]]$components[[j]] <-
        attribute
    }


    # Time dimension ---

    tmp <- as.list(data_structure$time_dimension[1,])
    time_dimension <- list(unbox("#sdmx.infomodel.datastructure.TimeDimension"),
                      list(id = unbox(tmp$id)))
    concept_ref <- list(unbox("#sdmx.infomodel.conceptscheme.ConceptRef"),
                        list(agencyid = unbox(tmp$concept_agencyid),
                             parentid = unbox(tmp$concept_parentid),
                             parentversion = unbox(tmp$concept_parentversion),
                             id = unbox(tmp$concept_id)))
    time_dimension[[2]][["concept-identity"]] <- concept_ref
    if (tmp$representation == "codelist") {
      codelist_ref <- list(unbox("#sdmx.infomodel.codelist.CodelistRef"),
                           list(agencyid = unbox(tmp$codelist_agencyid),
                                id = unbox(tmp$codelist_id),
                                version = unbox(tmp$codelist_version)))
      time_dimension[[2]][["local-representation"]] <- codelist_ref
    } else {
      time_dimension[[2]][["local-representation"]] <- unbox(tmp$representation)
    }
    i <- NROW(data_structure$dimensions) + NROW(data_structure$attributes) + 1
    data_message[[2]]$structures[["data-structures"]][[1]][[2]]$components[[i]] <-
      time_dimension


    # Primary measure ---

    tmp <- as.list(data_structure$primary_measure[1,])
    primary_measure <- list(unbox("#sdmx.infomodel.datastructure.PrimaryMeasure"),
                            list(id = unbox(tmp$id)))
    concept_ref <- list(unbox("#sdmx.infomodel.conceptscheme.ConceptRef"),
                        list(agencyid = unbox(tmp$concept_agencyid),
                             parentid = unbox(tmp$concept_parentid),
                             parentversion = unbox(tmp$concept_parentversion),
                             id = unbox(tmp$concept_id)))
    primary_measure[[2]][["concept-identity"]] <- concept_ref
    if (tmp$representation == "codelist") {
      codelist_ref <- list(unbox("#sdmx.infomodel.codelist.CodelistRef"),
                           list(agencyid = unbox(tmp$codelist_agencyid),
                                id = unbox(tmp$codelist_id),
                                version = unbox(tmp$codelist_version)))
      primary_measure[[2]][["local-representation"]] <- codelist_ref
    } else {
      primary_measure[[2]][["local-representation"]] <- unbox(tmp$representation)
    }
    i <- NROW(data_structure$dimensions) + NROW(data_structure$attributes) + 2
    data_message[[2]]$structures[["data-structures"]][[1]][[2]]$components[[i]] <-
      primary_measure


    # Push data message ---

   if (create) {
      message("Creating data structure: ", data_structure_ref, "\n")
      response <- POST(params$env$repository$url,
                       path = paste(params$env$repository$path,
                                    "datastructures", sep = "/"),
                       body = toJSON(data_message, na = "null"),
                       set_cookies(.cookies = get("econdata_session",
                                                  envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 201) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    } else {
      message("Updating data structure: ", data_structure_ref, "\n")
      response <- PUT(params$env$repository$url,
                      path = paste(params$env$repository$path,
                                   "datastructures",
                                   data_structure_ref, sep = "/"),
                      body = toJSON(data_message, na = "null"),
                      set_cookies(.cookies = get("econdata_session",
                                                 envir = .pkgenv)),
                      content_type("application/vnd.sdmx-codera.data+json"),
                      accept_json())
      if (response$status_code == 200) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    }
  } else {
    dimensions <- data_structure$components$dimensions
    attrs <- data_structure$components$attributes
    time_dimension <- data_structure$components$time_dimension
    primary_measure <- data_structure$components$primary_measure
    data_structure$components <- NULL
    write_ods(as.data.frame(data_structure), path = params$file, sheet = "data_structure")
    write_ods(dimensions, path = params$file, sheet = "dimensions", append = TRUE)
    write_ods(attrs, path = params$file, sheet = "attributes", append = TRUE)
    write_ods(time_dimension, path = params$file, sheet = "time_dimension", append = TRUE)
    write_ods(primary_measure, path = params$file, sheet = "primary_measure", append = TRUE)
    message("Data structure successfully written to: ", params$file, "\n")
  }
}



# Consumption agreement ---


write_cons_agreement <- function(cons_agreement, create, params) {
  if(is.null(params$file)) {
    cons_agreement_ref <- paste(cons_agreement$agencyid,
                                cons_agreement$id,
                                cons_agreement$version,
                                sep = "-")
    data_message <-
      list(unbox("#sdmx.infomodel.message.SDMXMessage"),
           list(header = params$header,
                structures =
                  list("consumption-agreements" =
                     list(
                       list(unbox("#sdmx.infomodel.datastructure.Dataflow"),
                          list(agencyid = unbox(cons_agreement$agencyid),
                               id = unbox(cons_agreement$id),
                               version = unbox(cons_agreement$version),
                               name = c("en", cons_agreement$name)))))))
    if (!is.na(cons_agreement$description)) {
      data_message[[2]]$structures[["consumption-agreements"]][[1]][[2]]$description <-
        c("en", cons_agreement$description)
    }
    data_message[[2]]$structures[["consumption-agreements"]][[1]][[2]][["dataflow"]] <-
      list(unbox("#sdmx.infomodel.datastructure.DataflowRef"),
           list(agencyid = unbox(cons_agreement$dataflow$agencyid),
                id = unbox(cons_agreement$dataflow$id),
                version = unbox(cons_agreement$dataflow$version)))
    data_message[[2]]$structures[["consumption-agreements"]][[1]][[2]][["data-consumer"]] <-
      list(unbox("#sdmx.infomodel.base.DataConsumerRef"),
           list(agencyid = unbox(cons_agreement$data_consumer$agencyid),
                id = unbox(cons_agreement$data_consumer$id),
                version = unbox(cons_agreement$data_consumer$version)))
    if (create) {
      message("Creating consumption agreement: ", cons_agreement_ref, "\n")
      response <- POST(params$env$repository$url,
                       path = paste(params$env$repository$path,
                                    "consumptionagreements", sep = "/"),
                       body = toJSON(data_message, na = "null"),
                       set_cookies(.cookies = get("econdata_session",
                                                  envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 201) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    } else {
      message("Updating consumption agreement: ", cons_agreement_ref, "\n")
      response <- PUT(params$env$repository$url,
                      path = paste(params$env$repository$path,
                                   "consumptionagreements",
                                   cons_agreement_ref, sep = "/"),
                      body = toJSON(data_message, na = "null"),
                      set_cookies(.cookies = get("econdata_session",
                                                 envir = .pkgenv)),
                      content_type("application/vnd.sdmx-codera.data+json"),
                      accept_json())
      if (response$status_code == 200) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    }
  } else {
    dataflow <- cons_agreement$dataflow
    data_consumer <- cons_agreement$data_consumer
    cons_agreement$dataflow <- NULL
    cons_agreement$data_consumer <- NULL
    write_ods(as.data.frame(cons_agreement), path = params$file, sheet = "consumption_agreement")
    write_ods(as.data.frame(dataflow), path = params$file, sheet = "dataflow", append = TRUE)
    write_ods(as.data.frame(data_consumer), path = params$file, sheet = "data_consumer", append = TRUE)
    message("Consumption agreement successfully written to: ", params$file, "\n")
  }
}



# Provision agreement ---


write_prov_agreement <- function(prov_agreement, create, params) {
  if(is.null(params$file)) {
    prov_agreement_ref <- paste(prov_agreement$agencyid,
                                prov_agreement$id,
                                prov_agreement$version,
                                sep = "-")
    data_message <-
      list(unbox("#sdmx.infomodel.message.SDMXMessage"),
           list(header = params$header,
                structures =
                  list("provision-agreements" =
                     list(
                       list(unbox("#sdmx.infomodel.datastructure.Dataflow"),
                          list(agencyid = unbox(prov_agreement$agencyid),
                               id = unbox(prov_agreement$id),
                               version = unbox(prov_agreement$version),
                               name = c("en", prov_agreement$name)))))))
    if (!is.na(prov_agreement$description)) {
      data_message[[2]]$structures[["provision-agreements"]][[1]][[2]]$description <-
        c("en", prov_agreement$description)
    }
    data_message[[2]]$structures[["provision-agreements"]][[1]][[2]][["dataflow"]] <-
      list(unbox("#sdmx.infomodel.datastructure.DataflowRef"),
           list(agencyid = unbox(prov_agreement$dataflow$agencyid),
                id = unbox(prov_agreement$dataflow$id),
                version = unbox(prov_agreement$dataflow$version)))
    data_message[[2]]$structures[["provision-agreements"]][[1]][[2]][["data-provider"]] <-
      list(unbox("#sdmx.infomodel.base.DataConsumerRef"),
           list(agencyid = unbox(prov_agreement$data_provider$agencyid),
                id = unbox(prov_agreement$data_provider$id),
                version = unbox(prov_agreement$data_provider$version)))
    if (create) {
      message("Creating provision agreement: ", prov_agreement_ref, "\n")
      response <- POST(params$env$repository$url,
                       path = paste(params$env$repository$path,
                                    "provisionagreements", sep = "/"),
                       body = toJSON(data_message, na = "null"),
                       set_cookies(.cookies = get("econdata_session",
                                                  envir = .pkgenv)),
                       content_type("application/vnd.sdmx-codera.data+json"),
                       accept_json())
      if (response$status_code == 201) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    } else {
      message("Updating provision agreement: ", prov_agreement_ref, "\n")
      response <- PUT(params$env$repository$url,
                      path = paste(params$env$repository$path,
                                   "provisionagreements",
                                   prov_agreement_ref, sep = "/"),
                      body = toJSON(data_message, na = "null"),
                      set_cookies(.cookies = get("econdata_session",
                                                 envir = .pkgenv)),
                      content_type("application/vnd.sdmx-codera.data+json"),
                      accept_json())
      if (response$status_code == 200) {
        message(content(response, encoding = "UTF-8")$success)
      } else {
        stop(content(response, encoding = "UTF-8"))
      }
    }
  } else {
    dataflow <- prov_agreement$dataflow
    data_provider <- prov_agreement$data_provider
    prov_agreement$dataflow <- NULL
    prov_agreement$data_provider <- NULL
    write_ods(as.data.frame(prov_agreement), path = params$file, sheet = "provision_agreement")
    write_ods(as.data.frame(dataflow), path = params$file, sheet = "dataflow", append = TRUE)
    write_ods(as.data.frame(data_provider), path = params$file, sheet = "data_provider", append = TRUE)
    message("Consumption agreement successfully written to: ", params$file, "\n")
  }
}
