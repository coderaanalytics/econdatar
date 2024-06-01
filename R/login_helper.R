.pkgenv <- new.env(parent = emptyenv())

login_helper <- function(login_url) {
  if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
    creds <- unlist(strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";"))
    response <- POST(login_url,
                     path = "/oauth2/token",
                     body = list(grant_type = "client_credentials",
                                 client_id = creds[1],
                                 client_secret = creds[2]),
                     encode = "form",
                     accept_json())

    if (response$status_code != 200)
      stop(content(response))
    token <- content(response)$access_token
  } else {
    token <- econdata_credentials()
  }
  assign("econdata_token", paste("Token", token), envir = .pkgenv)
  lockBinding("econdata_token", .pkgenv)
}
