get_metadata <- function(x) {

  env <- fromJSON(system.file("settings.json", package = "econdatar"))



  # Fetch dataflow ---


  dataflow <- paste(attr(x, "metadata")$Dataflow, collapse = "/")

  response <- GET(env$registry$url,
                  path = paste(c(env$registry$path,
                                 "dataflow",
                                 dataflow), collapse = "/"),
                  query = list(format = "sdmx-2.1"))

  if (response$status_code != 200)
    stop(content(response, encoding = "UTF-8"))



  # Fetch data structure definition (metadata) ---


  datastructure <- content(response,
                           type = "application/xml",
                           encoding = "UTF-8") |>
    xml_find_first("//Ref[@package='datastructure']") |>
    xml_attrs() |>
    as.list()

  response <- GET(env$registry$url,
                  path = paste(c(env$registry$path,
                                 "datastructure",
                                 as.vector(datastructure[c("agencyID",
                                                           "id",
                                                           "version")])),
                               collapse = "/"),
                  query = list(references = "children",
                               format = "sdmx-2.1"))

  if (response$status_code != 200)
    stop(content(response, encoding = "UTF-8"))

  structures <- content(response,
                        type = "application/xml",
                        encoding = "UTF-8")



  # Extract metadata references from data defninition ---


  metadata <- list()

  for (d in xml_find_all(structures, "//str:DimensionList/str:Dimension")) {
    metadata[[xml_attr(d, "id")]] <-
      list(is_dimension = TRUE,
           concept = xml_find_first(d, "./str:ConceptIdentity/Ref") |>
                       xml_attrs() |>
                       as.list(),
           codelist = xml_find_first(d, paste0("./str:LocalRepresentation",
                                               "/str:Enumeration/Ref")) |>
                        xml_attrs() |>
                        as.list())
  }

  for (a in xml_find_all(structures, "//str:AttributeList/str:Attribute")) {
    metadata[[xml_attr(a, "id")]] <-
      list(is_dimension = FALSE,
           concept = xml_find_first(a, "./str:ConceptIdentity/Ref") |>
                       xml_attrs() |>
                       as.list(),
           codelist = xml_find_first(a, paste0("./str:LocalRepresentation",
                                               "/str:Enumeration/Ref")) |>
                        xml_attrs() |>
                        as.list())
  }



  # Compile metadata from extracted references ---


  out <- list()

  for (p in names(metadata)) {
    concept_scheme <-
      xml_find_first(structures,
                     paste0("//str:Concepts",
                            "/str:ConceptScheme[@agencyID='",
                            metadata[[p]]$concept$agencyID,
                            "' and @id='",
                            metadata[[p]]$concept$maintainableParentID,
                            "' and @version='",
                            metadata[[p]]$concept$maintainableParentVersion,
                            "']"))

    concept <- xml_find_first(concept_scheme,
                              paste0("./str:Concept[@id='",
                                     metadata[[p]]$concept$id, "']"))

    out[[p]]$is_dimension <- metadata[[p]]$is_dimension

    out[[p]]$concept <-
      list(name = xml_text(xml_find_first(concept, "./com:Name")),
           description = xml_text(xml_find_first(concept, "./com:Description")))

    if (!any(is.na(metadata[[p]]$codelist))) {
      codelist <-
        xml_find_first(structures,
                       paste0("//str:Codelist[@agencyID='",
                              metadata[[p]]$codelist$agencyID,
                              "' and @id='",
                              metadata[[p]]$codelist$id,
                              "' and @version='",
                              metadata[[p]]$codelist$version,
                              "']"))

      codes <- sapply(xml_find_all(codelist, "./str:Code"), xml_attr, "id")
      code_names <- sapply(xml_find_all(codelist, "./str:Code/com:Name"), xml_text)

      out[[p]]$codelist <-
        list(name = xml_text(xml_find_first(codelist, "./com:Name")),
             description = xml_text(xml_find_first(codelist, "./com:Description")),
             codes = data.frame(codes = codes,
                                code_names = code_names))
    }
  }

  return(out)
}
