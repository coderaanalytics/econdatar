.pkgenv <- new.env(parent = emptyenv())

login_helper <- function(auth) {
  if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
    creds <- unlist(strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";"))
    response <- POST(auth$url,
                     path = auth$path,
                     body = list(grant_type = "client_credentials",
                                 client_id = creds[1],
                                 client_secret = creds[2],
                                 scope = "econdata.co.za/read econdata.co.za/write"),
                     encode = "form",
                     accept_json())
    if (response$status_code != 200)
      stop(content(response))
    token <- content(response)$access_token
  } else {
    token <- econdata_credentials()
  }
  assign("econdata_token", paste("Bearer", token), envir = .pkgenv)
}
