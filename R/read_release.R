get_release.edata <- function(agencyid, id, version, ...) 
{
  # PARAMETERS ----
  
  params <- list(...)
  query_params <- list()
  
  if(is.null(params$credentials)) 
    params$credentials <- NULL
  if( is.null(params$debug) ) 
    params$debug <- FALSE
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
  
  env <- fromJSON(system.file("settings.json",package="econdatar"))
  
  if( !is.null(params$credentials) ) {
    credentials = params$credentials
  } else if( Sys.getenv("ECONDATA") != "" ) {
    credentials = strsplit(Sys.getenv("ECONDATA"), ";")[[1]]
  } else {
    credentials = econdata_credentials()
  }

  # FETCH RELEASE ----
  
  dataflow <- paste(agencyid,id,version,sep=",")
  
  response <- GET(env$repository$url, 
                  path=paste(env$repository$path,"release/data/",dataflow,sep=""), 
                  query=query_params,
                  authenticate(credentials[1], credentials[2]))  
  
  if( response$status_code != 200 && !params$debug )
    stop( content(response, encoding="UTF-8") )
  
  return( content(response, encoding="UTF-8") )
  
}
