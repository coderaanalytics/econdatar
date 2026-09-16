.pkgenv <- new.env(parent = emptyenv())

login_helper <- function() {
  if (Sys.getenv("ECONDATA_APIKEY") != "") {
    apikey <- Sys.getenv("ECONDATA_APIKEY")
  } else if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
    stop("Credentials login has been deprecated. ",
         "Please obtain an API key from econdata.co.za, ",
         "and set the environment var ECONDATA_APIKEY=your_key")
  } else {
    apikey <- econdata_apikey()
  }
  assign("econdata_apikey", paste("Bearer", apikey), envir = .pkgenv)
}
