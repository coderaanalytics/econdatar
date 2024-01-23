write_structure <- function(structure, x, ...) {


  # Parameters ---

  env <- fromJSON(system.file("settings.json", package = "econdatar"))

  params <- list(...)

  if (!is.null(params$username) && !is.null(params$password)) {
    credentials <- paste(params$username, params$password, sep = ";")
  } else {
    credentials <- NULL
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

  structure_data <-
    switch(structure,
           "codelist" = write_codelist(x, env, file),
           "concept-scheme" = write_concept_scheme(x, env, file),
           "data-structure" = write_data_structure(x, env, file),
           stop("Specified structure, ", structure, ", is not supported."))
}



# Codelist ---

write_codelist <- function(codelist, env, file) {
  if(!is.null(file)) {
    codes <- codelist$codes
    codelist$codes <- NULL
    write_ods(as.data.frame(codelist), path = file, sheet = "codelist")
    write_ods(codes, path = file, sheet = "codes", append = TRUE)
    message("Codelist successfully written to: ", file, "\n")
  }
}


# Concept scheme ---


write_concept_scheme <- function(concept_scheme, env, file) {
  if(!is.null(file)) {
    concepts <- concept_scheme$concepts
    concept_scheme$concepts <- NULL
    write_ods(as.data.frame(concept_scheme), path = file, sheet = "concept_scheme")
    write_ods(concepts, path = file, sheet = "concepts", append = TRUE)
    message("Concept scheme successfully written to: ", file, "\n")
  }
}



# Data structure ---


write_data_structure <- function(data_structure, env, file) {
  if(!is.null(file)) {
    dimensions <- data_structure$components$dimensions
    attrs <- data_structure$components$attributes
    time_dimension <- data_structure$components$time_dimension
    primary_measure <- data_structure$components$primary_measure
    data_structure$components <- NULL
    write_ods(as.data.frame(data_structure), path = file, sheet = "data_structure")
    write_ods(dimensions, path = file, sheet = "dimensions", append = TRUE)
    write_ods(attrs, path = file, sheet = "attributes", append = TRUE)
    write_ods(time_dimension, path = file, sheet = "time_dimension", append = TRUE)
    write_ods(primary_measure, path = file, sheet = "primary_measure", append = TRUE)
    message("Data structure successfully written to: ", file, "\n")
  }
}
