read.edata <- function(id, ...) 
{
 
  # ---- PARAMETERS ----

  env <- fromJSON(system.file("settings.json",package="econdatar"))

  params <- list(...)
  
  if(is.null(params$credentials)) 
    params$credentials <- NULL
  if(is.null(params$debug)) 
    params$debug <- FALSE
  if(is.null(params$agencyid)) 
    params$agencyid <- env$repository$default_agency
  if(is.null(params$version)) 
    params$version <- "latest"
  if(is.null(params$key)) 
    params$key <- "all"
  if(is.null(params$provideragencyid)) 
    params$dataprovideragencyid <- env$repository$default_agency
  if(is.null(params$providerid)) 
    params$dataproviderid <- env$repository$default_provider
  
  query_params <- list()
  
  if ( !is.null(params[["releasedescription"]]) )
    query_params[["releaseDescription"]] <- params[["releasedescription"]]
  
  # ---- DATA STRUCTURE ----
  
  dataflow <- paste(params$agencyid,id,params$version,sep="/")
  
  data_structure <- content( GET(env$registry$url, 
                                 path=paste(env$registry$path, "datastructure/",dataflow,sep=""), 
                                 query=list("format"="sdmx-2.0")),
                             type="application/xml",
                             encoding="UTF-8")

  series_dims <- NULL
  
  all_dimensions <- xml_find_all(data_structure, "//str:Dimension")
  
  for( dimension in all_dimensions ) {
    series_dims <- c(series_dims, xml_attr(dimension,"conceptRef"))
  }
    
  all_attributes <- xml_find_all(data_structure, "//str:Attribute")
  
  dataset_attrs <- NULL
  series_attrs <- series_dims
  obs_attrs <- NULL
  
  for( attribute in all_attributes ) {
    if( xml_attr(attribute,"attachmentLevel") == "DataSet" )
      dataset_attrs <- c(dataset_attrs, xml_attr(attribute,"conceptRef"))
    else if( xml_attr(attribute,"attachmentLevel") == "Series" )
      series_attrs <- c(series_attrs, xml_attr(attribute,"conceptRef"))
    else if( xml_attr(attribute,"attachmentLevel") == "Observation" )
      obs_attrs <- c(obs_attrs, xml_attr(attribute,"conceptRef"))
  }

  # FETCH DATA ----
  
  message(paste("\nLoading dataset - urn:sdmx:org.sdmx.infomodel.datastructure.DataStructure=",
                params$agencyid, ":", id, "(", params$version, ").\n", sep=""))
  
  if ( !is.null(params[["file"]]) ) 
  {
    
    data_message <- fromJSON(params[["file"]])
      
    database <- json2edata(data_message, params, series_dims, obs_attrs)
    
  } else {
    
    if( !is.null(params$credentials) ) {
      credentials = params$credentials
    } else if( Sys.getenv("ECONDATA") != "" ) {
      credentials = strsplit(Sys.getenv("ECONDATA"), ";")[[1]]
    } else {
      credentials = econdata_credentials()
    }

    dataflow <- paste(params$agencyid,id,params$version,sep=",")
    data_provider <- paste(params$dataprovideragencyid,params$dataproviderid,sep=",")

    response <- GET(env$repository$url, 
                    path=paste(env$repository$path,"data/",dataflow,"/",params$key,"/",data_provider,sep=""), 
                    query=query_params,
                    authenticate(credentials[1], credentials[2]),
                    accept_json())
    
    data_message <- content(response, encoding="UTF-8")
    
    if( response$status_code != 200 && !params$debug )
      stop( data_message )

    database <- json2edata(data_message, params, series_dims, obs_attrs)
    
  }
  
  return(database)
}