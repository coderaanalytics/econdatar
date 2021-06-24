read_release <- function(id, ...) 
{
  # PARAMETERS ----
  
  
  env <- fromJSON(system.file("settings.json",package="econdatar"))
  
  params <- list(...)
  
  if( !is.null(params$username) && !is.null(params$password) ) 
    params$credentials <- paste(params$username, params$password, sep = ";")
  
  query_params <- list()
  
  if ( !is.null(params$newest) )
    query_params$newest <- "true"
  if ( !is.null(params$oldest) )
    query_params$oldest <- "true"
  if ( !is.null(params$before) )
    query_params$beforeDateTime <- strftime(params$before, "%Y-%m-%dT%H:%M:%S")
  if ( !is.null(params$after) )
    query_params$afterDateTime <- strftime(params$after, "%Y-%m-%dT%H:%M:%S")
  if ( !is.null(params$includestext) )
    query_params$includesText <- params$includestext
  if ( !is.null(params$releasedescription) )
    query_params$releaseDescription <- params$releasedescription
  


  # FETCH RELEASE ----
  
  
  if( !is.null(params$credentials) ) {
    credentials = unlist( strsplit(params$credentials, ";") )
  } else if( Sys.getenv("ECONDATA_CREDENTIALS") != "" ) {
    credentials = unlist( strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";") )
  } else {
    credentials = econdata_credentials()
  }
  
  dataflow <- paste(c(params$agencyid, id, params$version), collapse = ",")
  data_provider <- paste(c(params$provideragencyid, 
                           params$providerid), collapse = ",")
  
  if( nchar(data_provider) == 0 ) 
    data_provider <- NULL 
  
  response <- GET(env$repository$url, 
                  path = paste(c(env$repository$path, "release", 
                                 dataflow, 
                                 params$key, 
                                 data_provider), collapse = "/"), 
                  query = query_params,
                  authenticate(credentials[1], credentials[2]),
                  accept_json())  
  
  if( response$status_code == 200 )
    message("Releases successfully retrieved from EconData.\n")
  else
    tryCatch(stop(content(response, encoding="UTF-8")),
             error = function(e) { stop(response) })
    
  return( content(response, encoding="UTF-8")$Result$Success$Message )
}
