edata <- function(rowtimes, ..., properties=NULL)
{
  variables <- list(...)
  if( length(variables) != 0 ) {
    if( is.list(variables[[1]]) ) {
      variables <- variables[[1]]
    }
  }
  
  if( (length(rowtimes) != 0) && (length(variables) == 0) ) {
    variables$Var1 <- rep(NaN, length(rowtimes))
  }
   
  x <- list()
  x$properties <- if( is.list(properties) ) properties else list()
  x$observations <- data.frame(variables, row.names=strftime(rowtimes,"%Y-%m-%d"))
  
  class(x) <- "edata"
  
  return( x )
}

print.edata <- function(x, ...) 
{
  if( sum(dim(x$observations)) == 0 ) {
    cat("edata with 0 columns and 0 rows")
  } else {
    print(x$observations)
  }
}

summary.edata <- function(object, ...) 
{
  return( object$properties )
}

"[.edata" <- function(x, i, j)
{
  return( x$observations[i, j] )
}

"[<-.edata" <- function(x, i, j, value)
{
  x$observations[i, j] <- value
  return( x )
}

"$.edata" <- function(x, name)
{
  if( any(name == c("observations","properties")) ) {
    NextMethod()
  } else {
    x$observations[[name, exact=FALSE]]
  }
}

rbind.edata <- function(..., deparse.level=1) 
{
  stop("Method not implemented, use edata$observations.")  
}

cbind.edata <- function(..., deparse.level=1) 
{
  stop("Method not implemented, use edata$observations.")  
}

dim.edata <- function(x)
{
  return( dim(x$observation) )
}

nrow.edata <- function(x)
{
  return( nrow(x$observation) )
}

ncol.edata <- function(x)
{
  return( ncol(x$observation) )
}

names.edata <- function(x) 
{
  names( x$observations )
}

"names<-.edata" <- function(x, value) 
{
  names(x$observations) <- value
  return( x )
}

time.edata <- function(x, ...)
{
  return( rownames(x$observations) )
}

as.data.frame.edata <- function(x, row.names = NULL, optional = FALSE, ...) {
  return( x$observations )
}