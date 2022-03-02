econdata_credentials <- function() {

  username   <- tclVar(Sys.info()["user"])
  password   <- tclVar("")

  tt <- tktoplevel()
  tkwm.title(tt, "www.econdata.co.za credentials")
  user.entry <- tkentry(tt, textvariable = username)
  pswd.entry <- tkentry(tt, textvariable = password, show = "*")

  reset <- function() {
    tclvalue(username)   <- Sys.info()["user"]
    tclvalue(password)   <- ""
  }
  reset.but <- tkbutton(tt, text = "Reset", command = reset)

  submit <- function() {
    user <- tclvalue(username)
    pswd <- tclvalue(password)
    e <- parent.env(environment())
    e$user <- user
    e$pswd <- pswd
    tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text = "submit", command = submit)

  tkgrid(tklabel(tt, text = "Enter User Details"), columnspan = 2)
  tkgrid(tklabel(tt, text = "Username"), user.entry, pady = 10, padx = 10)
  tkgrid(tklabel(tt, text = "Password"), pswd.entry, pady = 10, padx = 10)
  tkgrid(submit.but, reset.but, pady = 10, padx = 50)

  tkwait.window(tt)
  return(c(user, pswd))
}
