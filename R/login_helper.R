.pkgenv <- new.env(parent = emptyenv())

login_helper <- function(credentials, login_url) {
  if (!is.null(credentials)) {
    creds <- unlist(strsplit(credentials, ";"))
  } else if (Sys.getenv("ECONDATA_CREDENTIALS") != "") {
    creds <- unlist(strsplit(Sys.getenv("ECONDATA_CREDENTIALS"), ";"))
  } else {
    creds <- econdata_credentials()
  }

  response <- POST(login_url,
                   path = "/signin",
                   body = list(username = creds[1],
                               password = creds[2]),
                   encode = "form")

  if (response$status_code != 200)
    stop(content(response, encoding = "UTF-8"))

  cookie_jar <- cookies(response)
  domain <- substr(login_url, 9, nchar(login_url))
  session <- cookie_jar[which(cookie_jar[, 1] == paste0("#HttpOnly_", domain) &
                                cookie_jar[, 6] == "ring-session"), ]
  assign("econdata_session", as.character(session), envir = .pkgenv)
  lockBinding("econdata_session", .pkgenv)
}
