write_release <- function(agencyid, id, version, provideragencyid, providerid, releasedescription, ...) 
{
  # PARAMETERS ----
  
  
  env <- fromJSON(system.file("settings.json",package="econdatar"))
  
  params <- list(...)
  
  if( !is.null(params$username) && !is.null(params$password) ) 
    params$credentials <- paste(params$username, params$password, sep = ";")
  
  query_params <- list()
  
  query_params$releaseDescription <- releasedescription


  # ADD RELEASE ----
  
  
  if( !is.null(params$credentials) ) {
    credentials = unlist( strsplit(params$credentials, ";") )
  } else if( Sys.getenv("ECONDATA_CREDENTIALS") != "" ) {
    credentials = unlist( strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";") )
  } else {
    credentials = econdata_credentials()
  }
  
  dataflow <- paste(agencyid, id, version, sep = ",")
  data_provider <- paste(provideragencyid, providerid, sep = ",")
  
  response <- POST(env$repository$url, 
                   path = paste(env$repository$path, "modify/release", 
                                dataflow,
                                data_provider, sep = "/"), 
                   query = query_params,
                   authenticate(credentials[1], credentials[2]),
                   accept_json())
  
  if( response$status_code == 200 )
    message( content(response, encoding="UTF-8")$Result$Success$Message )
  else
    tryCatch(stop(content(response, encoding="UTF-8")),
             error = function(e) { stop(response) })
}
