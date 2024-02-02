get_metadata <- function(x) {
  env <- fromJSON(system.file("settings.json", package = "econdatar"))


  # Fetch data structure definition (metadata) ---

  attrs <- attr(x, "metadata")
  provision_agreement_ref <- paste(attrs[["provision-agreement"]][[2]]$agencyid,
                                   attrs[["provision-agreement"]][[2]]$id,
                                   attrs[["provision-agreement"]][[2]]$version,
                                   sep = "-")
  response <- GET(env$registry$url,
                  path = paste(c(env$registry$path,
                                 "provisionagreements",
                                 provision_agreement_ref), collapse = "/"),
                  set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                  accept("application/vnd.sdmx-codera.data+json"))
  if (response$status_code != 200) {
    stop(content(response, type = "application/json", encoding = "UTF-8"))
  }
  data_message <- content(response, type = "application/json", encoding = "UTF-8")
  provision_agreement <- data_message[[2]]$structures[["provision-agreements"]][[1]]
  dataflow_ref <- paste(provision_agreement[[2]][["dataflow"]][[2]]$agencyid,
                        provision_agreement[[2]][["dataflow"]][[2]]$id,
                        provision_agreement[[2]][["dataflow"]][[2]]$version,
                        sep = "-") 
  response <- GET(env$registry$url,
                  path = paste(c(env$registry$path,
                                 "dataflows",
                                 dataflow_ref), collapse = "/"),
                  set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                  accept("application/vnd.sdmx-codera.data+json"))
  if (response$status_code != 200) {
    stop(content(response, type = "application/json", encoding = "UTF-8"))
  }
  data_message <- content(response, type = "application/json", encoding = "UTF-8")
  dataflow <- data_message[[2]]$structures[["dataflows"]][[1]]
  data_structure_ref <- paste(dataflow[[2]][["data-structure"]][[2]]$agencyid,
                              dataflow[[2]][["data-structure"]][[2]]$id,
                              dataflow[[2]][["data-structure"]][[2]]$version,
                              sep = "-") 
  response <- GET(env$registry$url,
                  path = paste(c(env$registry$path,
                                 "datastructures",
                                 dataflow_ref), collapse = "/"),
                  query = list(relations = "references"),
                  set_cookies(.cookies = get("econdata_session", envir = .pkgenv)),
                  accept("application/vnd.sdmx-codera.data+json"))
  if (response$status_code != 200) {
    stop(content(response, type = "application/json", encoding = "UTF-8"))
  }
  data_message <- content(response, type = "application/json", encoding = "UTF-8")
  data_structure <- data_message[[2]]$structures[["data-structures"]][[1]][[2]]
  concept_schemes <- data_message[[2]]$structures[["concept-schemes"]]
  codelists <- data_message[[2]]$structures[["codelists"]]


  # Compile metadata ---

  metadata <- list()
  for (component in data_structure$components) {
    id <- component[[2]][["concept-identity"]][[2]]
    repr <- component[[2]][["local-representation"]]
    metadata[[id$id]] <- list(type = component[[1]],
                              concept = NULL,
                              codelist = NULL)
    for (concept_scheme in concept_schemes) {
      if (id$agencyid == concept_scheme[[2]]$agencyid &&
          id$parentid == concept_scheme[[2]]$id &&
          id$parentversion == concept_scheme[[2]]$version) {
        for (concept in concept_scheme[[2]]$concepts) {
          if (id$id == concept[[2]]$id) {
            metadata[[id$id]]$concept <- concept[[2]]
          }
        }
      }
    }
    if (is.list(repr)) {
      for (codelist in codelists) {
        if (repr[[2]]$agencyid == codelist[[2]]$agencyid &&
            repr[[2]]$id == codelist[[2]]$id &&
            repr[[2]]$version == codelist[[2]]$version) {
          metadata[[id$id]]$codelist <- codelist[[2]]
        }
      }
    }
  }
  return(metadata)
}
