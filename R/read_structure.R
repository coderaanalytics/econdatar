read_structure <- function(structure, id, ...) {


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


  # Fetch structure(s) ---

  if (!exists("econdata_session", envir = .pkgenv)) {
    login_helper(credentials, env$repository$url)
  }

  agencyids <- paste(agencyid, collapse = ",")
  ids <- paste(id, collapse = ",")
  versions <- paste(version, collapse = ",")

  structure_data <-
    switch(structure,
           "codelist" = read_codelists(env, agencyids, ids, versions),
           "concept-scheme" = read_concept_schemes(env, agencyids, ids, versions),
           "data-structure" = read_data_structures(env, agencyids, ids, versions),
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


read_codelists <- function(env, agencyids, ids, versions) {
  message(paste("\nFetching data set(s) -", paste(ids, collapse = ", "), "\n"))

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
}

process_codelist <- function(structure, file) {
  structure_ref <- paste(structure[[2]]$agencyid, 
                         structure[[2]]$id,
                         structure[[2]]$version,
                         sep = "-")

  message("Processing structure: ", structure_ref, "\n")

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

  if(!is.null(file)) {
    write_ods(as.data.frame(codelist), path = file, sheet = "codelist")
    write_ods(codes, path = file, sheet = "codes", append = TRUE)
  }

  codelist$codes <- codes
  return(codelist)
}



# Concept schemes ---


read_concept_schemes <- function(env, agencyids, ids, versions) {
  stop("Not implemented")
}

process_concept_scheme <- function(structure, file) {
  stop("Not implemented")
}



# Data structures ---


read_data_structures <- function(env, agencyids, ids, versions) {
  stop("Not implemented")
}

process_data_structure <- function(structure, file) {
  stop("Not implemented")
}
