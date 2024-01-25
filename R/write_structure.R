write_structure <- function(structure, x, create = FALSE, ...) {


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

  header <- list()

  if(is.null(file)) {
    header$id <- tryCatch(
                          unbox(paste0("ECONDATAR-V",
                                       sessionInfo()[[7]]$econdatar[[4]])),
                          error = function(e)
                            unbox("Unknown"))

    header$prepared <- unbox(format(Sys.time(), format = "%Y-%m-%dT%T"))

    header$sender <- tryCatch(
                              unbox(Sys.getenv()[["USER"]]),
                              error = function(e)
                                unbox("Anonymous"))

    header$receiver <- unbox("EconData web application")
  }

  structure_data <-
    switch(structure,
           "codelist" = write_codelist(x, create, header, env, file),
           "concept-scheme" = write_concept_scheme(x, create, header, env, file),
           "data-structure" = write_data_structure(x, create, header, env, file),
           stop("Specified structure, ", structure, ", is not supported."))
}



# Codelist ---

write_codelist <- function(codelist, create, header, env, file) {
  if(is.null(file)) {
    codelist_ref <- paste(codelist$agencyid,
                          codelist$id,
                          codelist$version,
                          sep = "-")

    data_message <-
      list(unbox("#sdmx.infomodel.message.SDMXMessage"),
           list(header = header,
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

      response <- POST(env$repository$url,
                       path = paste(env$repository$path,
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

      response <- PUT(env$repository$url,
                      path = paste(env$repository$path,
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
    write_ods(as.data.frame(codelist), path = file, sheet = "codelist")
    write_ods(codes, path = file, sheet = "codes", append = TRUE)
    message("Codelist successfully written to: ", file, "\n")
  }
}


# Concept scheme ---


write_concept_scheme <- function(concept_scheme, create, header, env, file) {
  if(is.null(file)) {

  } else {
    concepts <- concept_scheme$concepts
    concept_scheme$concepts <- NULL
    write_ods(as.data.frame(concept_scheme), path = file, sheet = "concept_scheme")
    write_ods(concepts, path = file, sheet = "concepts", append = TRUE)
    message("Concept scheme successfully written to: ", file, "\n")
  }
}



# Data structure ---


write_data_structure <- function(data_structure, create, header, env, file) {
  if(is.null(file)) {

  } else {
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
