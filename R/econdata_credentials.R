econdata_credentials <- function() {
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    stop("Package \"tcltk\" needed for this function to work.",
         call. = FALSE)
  }
  user <- pswd <- NULL # Need to add global bindings for variables
  username   <- tcltk::tclVar(Sys.info()["user"])
  password   <- tcltk::tclVar("")

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "www.econdata.co.za credentials")
  user.entry <- tcltk::tkentry(tt, textvariable = username)
  pswd.entry <- tcltk::tkentry(tt, textvariable = password, show = "*")

  reset <- function() {
    tcltk::tclvalue(username)   <- Sys.info()["user"]
    tcltk::tclvalue(password)   <- ""
  }
  reset.but <- tcltk::tkbutton(tt, text = "Reset", command = reset)

  submit <- function() {
    user <- tcltk::tclvalue(username)
    pswd <- tcltk::tclvalue(password)
    e <- parent.env(environment())
    e$user <- user
    e$pswd <- pswd
    tcltk::tkdestroy(tt)
  }
  submit.but <- tcltk::tkbutton(tt, text = "submit", command = submit)

  tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter User Details"),
                columnspan = 2)
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Username"),
                user.entry, pady = 10, padx = 10)
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Password"),
                pswd.entry, pady = 10, padx = 10)
  tcltk::tkgrid(submit.but, reset.but, pady = 10, padx = 50)

  tcltk::tkwait.window(tt)
  return(c(user, pswd))
}
