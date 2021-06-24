write.edata <- function(db, agencyid, id, version, provideragencyid, providerid, ...) 
{

  # ---- PARAMETERS ----
  
  env <- fromJSON(system.file("settings.json",package="econdatar"))
  
  params <- list(...)
  query_params <- list()
  
  if(is.null(params$credentials)) 
    params$credentials <- NULL
  
  # ---- DATA MESSAGE ----
  
  doc <- asJSON(db)
  
  # ---- PUSH DATA ----
  
  if( !is.null(params[["file"]]) ) {
    write(doc, file = params[["file"]])
    
    message("Data message saved to disk.")
  } else {
    
    if( !is.null(params$credentials) ) {
      credentials = params$credentials
    } else if( Sys.getenv("ECONDATA") != "" ) {
      credentials = strsplit(Sys.getenv("ECONDATA"), ";")[[1]]
    } else {
      credentials = econdata_credentials()
    }
    
    tmp <- tempfile()
    write(doc, file = tmp)
    
    dataflow <- paste(agencyid, id, version, sep=",")
    data_provider <- paste(provideragencyid, providerid, sep=",")
    
    response <- POST(env$repository$url,
                     path=paste0(env$repository$path, "modify/data/", dataflow, "/", data_provider),
                     query=query_params, 
                     body=list("file"=upload_file(tmp,"application/json")), 
                     encode="multipart",
                     authenticate(credentials[1], credentials[2]),
                     accept_json())

    if( response$status_code == 200 )
      message( content(response, encoding="UTF-8") )
    else
      stop( content(response, encoding="UTF-8") )
    
  }
}
