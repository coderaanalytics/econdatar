write.edata <- function(db, agencyid, id, version, ...) 
{

  # ---- PARAMETERS ----
  
  params <- list(...)
  
  query_params <- list()
  
  if(is.null(params$credentials)) 
    params$credentials <- NULL
  if ( !is.null(params[["releasedatetime"]]) ) {
    query_params[["releaseDateTime"]] <- params[["releasedatetime"]]
    params[["historicalrelease"]] <- TRUE
  }
  if ( !is.null(params[["releasedescription"]]) )
    query_params[["releaseDescription"]] <- params[["releasedescription"]]
  
  env <- fromJSON(system.file("settings.json",package="databotr"))
  
  # ---- DOCUMENT ROOT ----
  
  doc <- xml_new_root("message:CompactData")
  
  xml_attr(doc, "xmlns:message") <- "http://www.SDMX.org/resources/SDMXML/schemas/v2_0/message"
  xml_attr(doc, "xmlns:ns1") <- paste("urn:sdmx:org.sdmx.infomodel.datastructure.Dataflow=",
                                      agencyid,":",id,"(",version,"):compact",sep="")
  #xml_attr(doc, "xmlns:xml") <- "http://www.w3.org/XML/1998/namespace"
  xml_attr(doc, "xmlns:xsi") <- "http://www.w3.org/2001/XMLSchema-instance"
  
  
  # ---- HEADER ----
  
  header <- xml_add_child(doc, "message:Header")
  
  messageid <- xml_add_child(header, "message:ID")
  xml_text(messageid) <- paste("IREF", floor(runif(1)*1e6), sep="")
  
  test <- xml_add_child(header, "message:Test")
  xml_text(test) <- "false"
  
  prepared <- xml_add_child(header, "message:Prepared")
  xml_text(prepared) <- strftime(Sys.time(), format="%Y-%m-%dT%H:%M:%S")
  
  sender <- xml_add_child(header, "message:Sender")
  xml_attr(sender, "id") <- "R"
  
  receiver <- xml_add_child(header, "message:Receiver")
  xml_attr(receiver, "id") <- "www.econdata.co.za"
  
  
  ## ---- DATASET ----
  
  dataset <- xml_add_child(doc, "ns1:DataSet")
  if( !is.null(db$meta) ) {
    for( datasetattr in names(db$meta) )
      xml_attr(dataset, datasetattr) <- db$meta[[datasetattr]]
    db$meta <- NULL
  }
  
  seriesnames <- names(db)
  
  for( s in seriesnames ) {

    series <- xml_add_child(dataset, "ns1:Series")
    timetable <- db[[s]]
    attrs <- summary(timetable)
    for ( seriesattr in names(attrs) )
      xml_attr(series, seriesattr) <- attrs[[seriesattr]]

    for( rowtime in time(timetable) ) {
      obs <- xml_add_child(series, "ns1:Obs")
      xml_attr(obs, "TIME_PERIOD") <- strftime(rowtime, format="%Y-%m-%d")
      for( obsattr in names(timetable) )
          xml_attr(obs, obsattr) <- timetable[rowtime, obsattr]
    }
  }
  
  
  # ---- PUSH DATA ----
  
  if( !is.null(params[["file"]]) ) {
    write_xml(doc, params[["file"]])
    
    message("Data set saved to disk.")
  } else {
    
    if( !is.null(params$credentials) ) {
      credentials = params$credentials
    } else if( Sys.getenv("DATABOT") != "" ) {
      credentials = strsplit(Sys.getenv("DATABOT"), ";")[[1]]
    } else {
      credentials = econdata_credentials()
    }
    
    tmp <- tempfile()
    write_xml(doc,tmp)
    
    dataflow <- paste(agencyid,id,version,sep=",")
    
    if( is.null(params$historicalrelease) ) {
      response <- POST(env$econdata$url,
                       path=paste(env$econdata$path,"modify/data/",dataflow,sep=""),
                       query=query_params, 
                       body=list("file"=upload_file(tmp,"application/xml")), 
                       encode="multipart",
                       authenticate(credentials[1], credentials[2]))
    } else {
      response <- POST(env$econdata$url,
                       path=paste(env$econdata$path,"modify/data/",dataflow,"/historical",sep=""),
                       query=query_params, 
                       body=list("file"=upload_file(tmp,"application/xml")), 
                       encode="multipart",
                       authenticate(credentials[1], credentials[2]))
    }
    
    if( response$status_code == 200 )
      message(xml_text(xml_find_first(content(response, encoding="UTF-8"), "//Text")))
    else
      stop(xml_text(xml_find_first(content(response, encoding="UTF-8"), "//d:Text")))
    
  }
}
