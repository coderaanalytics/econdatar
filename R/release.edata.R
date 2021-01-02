release.edata <- function(agencyid, id, version, ...) 
{
  
  # PARAMETERS ----
  
  params <- list(...)
    
  query_params <- list()
  
  if(is.null(params$credentials)) 
    params$credentials <- NULL
  if( is.null(params$addrelease) ) 
    params$addrelease <- FALSE
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
  
  env <- fromJSON(system.file("settings.json",package="databotr"))
  
  if( !is.null(params$credentials) ) {
    credentials = params$credentials
  } else if( Sys.getenv("DATABOT") != "" ) {
    credentials = strsplit(Sys.getenv("DATABOT"), ";")[[1]]
  } else {
    credentials = econdata_credentials()
  }

  if( params$addrelease ) {
    
    # ADD RELEASE ----
    
    dataflow <- paste(agencyid,id,version,sep=",")
    
    response <- POST(env$econdata$url, 
                     path=paste(env$econdata$path,"release/data/",dataflow,sep=""), 
                     query=query_params,
                     authenticate(credentials[1], credentials[2]))
    
    if( response$status_code != 200 && !params$debug )
      stop(xml_text(xml_find_first(content(response, encoding="UTF-8"), "//d:Text")))
    
    return()
    
  } else {
    
    # FETCH RELEASE ----
    
    dataflow <- paste(agencyid,id,version,sep=",")
    
    response <- GET(env$econdata$url, 
                    path=paste(env$econdata$path,"release/data/",dataflow,sep=""), 
                    query=query_params,
                    authenticate(credentials[1], credentials[2]))  
    
    if( response$status_code != 200 && !params$debug )
      stop(xml_text(xml_find_first(content(response, encoding="UTF-8"), "//d:Text")))
    
    release_nodeset <- xml_find_all(content(response,encoding="UTF-8"), "//Release")
    
    description = rep(NA, length(release_nodeset))
    release_date = rep(NA, length(release_nodeset))
    
    
    # RETURN RELEASES ----
    
    for ( index in 1:length(release_nodeset) ) {
      description[index] = xml_attr(release_nodeset[index], "Description")
      release_date[index] = strftime(xml_attr(release_nodeset[index], "Date"), "%Y-%m-%dT%H:%M:%S")
    }
    
    return( data.frame(description, release_date) )
    
  }
  
}
