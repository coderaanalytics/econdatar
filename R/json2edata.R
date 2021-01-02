json2edata <- function(data_message, params, series_dims, obs_attrs) {
  database <- list()
  
  if( length(data_message$DataSets) > 1 )
    stop("Multiple datasets not supported by databotr.")
  
  dataset <- data_message$DataSets[[1]]
  
  for( series in dataset$Series) {
    obs <- series$Obs
    series$Obs <- NULL
    
    if( !is.null(params[["nameby"]]) ) {
      if( all(params[["nameby"]] == "key") ) {
        series_name <- NULL
        for( n in series_dims )
          series_name <- c(series_name, series[[n]])
        s <- paste(series_name, collapse=".")
      } else {
        series_name <- NULL
        for( n in params[["nameby"]] )
          series_name <- c(series_name, series[[n]])
        s <- paste(series_name, collapse="_")
        
        seriesnames <- names(database)
        variations <- sum(grepl(s,seriesnames))
        if( variations != 0 )
          s <- paste0(s, "_V", variations)
      }
    } else {
      s <- paste("series", length(database)+1, sep="")
    }
    
    if( length(obs) == 0 ) {
      database[[s]] <- edata(NULL, properties=series)
    } else {
      obs_fields <- list()
      obs_fields$OBS_VALUE <- sapply(obs, function(x) as.numeric(x$OBS_VALUE))
      for (field in obs_attrs)
        obs_fields[[field]] <- sapply(obs, function(x) x[[field]])
      
      series$Obs <- NULL
      
      database[[s]] <- edata(sapply(obs, function(x) x$TIME_PERIOD), obs_fields, properties=series)
    }
  }
  
  dataset$Series <- NULL
  
  if(!is.null(params$metadata) && params$metadata == TRUE)
    database$meta <- dataset
  
  return(database)
}