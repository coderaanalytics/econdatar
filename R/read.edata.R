write.edata <- function(agencyid, id, version, ...) 
{
 
  # ---- PARAMETERS ----

  env <- fromJSON(system.file("settings.json",package="econdatar"))

  params <- list(...)
  
  if(is.null(params$credentials)) 
    params$credentials <- NULL
  if(is.null(params$debug)) 
    params$debug <- FALSE
  if(is.null(params$key)) 
    params$key <- "all"
  if(is.null(params$provider_id)) 
    params$dataprovider <- env$econdata$default_provider$id
  if(is.null(params$provider_agencyid)) 
    params$dataprovideragencyid <- env$econdata$default_provider$agencyid
  
  query_params <- list()
  
  if ( !is.null(params[["releasedescription"]]) )
    query_params[["releaseDescription"]] <- params[["releasedescription"]]
  
  # ---- DATA STRUCTURE ----
  
  dataflow <- paste(agencyid,id,version,sep="/")
  
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
                agencyid, ":", id, "(", version, ").\n", sep=""))
  
  if ( !is.null(params[["file"]]) ) 
  {
    
    data_message <- read_xml(params[["file"]])
    
    ns <- paste("urn:sdmx:org.sdmx.infomodel.datastructure.Dataflow=",
                agencyid, ":", id, "(", version, "):compact", sep="")
      
    database <- xml2edata(data_message, params, ns, dataset_attrs, series_attrs, obs_attrs)
    
  } else {
    
    if( !is.null(params$credentials) ) {
      credentials = params$credentials
    } else if( Sys.getenv("DATABOT") != "" ) {
      credentials = strsplit(Sys.getenv("DATABOT"), ";")[[1]]
    } else {
      credentials = econdata_credentials()
    }

    dataflow <- paste(agencyid,id,version,sep=",")
    data_provider <- paste(params$dataprovideragencyid,params$dataprovider,sep=",")


    response <- GET(env$econdata$url, 
                    path=paste(env$econdata$path,"data/",dataflow,"/",params$key,"/",data_provider,sep=""), 
                    query=query_params,
                    authenticate(credentials[1], credentials[2]),
                    accept_json())
    
    if( response$status_code != 200 && !params$debug )
      stop(xml_text(xml_find_first(content(response, encoding="UTF-8"), "//d:Text")))

    data_message <- content(response, encoding="UTF-8")

    database <- json2edata(data_message, params, series_dims, obs_attrs)
    
  }
  
  return(database)
}