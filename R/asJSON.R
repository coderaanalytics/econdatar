asJSON <- function(db, meta=NULL, header=NULL) {

  dataset <- list()
  
  if( !is.null(header) ) {
    dataset$Header <- lapply(header, unbox)
  } else {
    dataset$Header$Prepared <- format(Sys.time(), format = "%Y-%m-%dT%T")
  }
  
  if( !is.null(header) ) {
    dataset$DataSets[[1]] <- lapply(meta, unbox)
  }
  
  for( index in 1:length(names(db)) ) {
    series <- names(db)[index]
    
    # if( class(db[[series]]) != "edata" ) {
    #   stop("Input must be a list of object of class edata only.")
    # }
    
    dataset$DataSets[[1]]$Series[[index]] <- lapply(summary(db[[series]]), unbox)
    dataset$DataSets[[1]]$Series[[index]]$Obs <- as.data.frame(db[[series]])
  }
  
  toJSON(dataset)
}