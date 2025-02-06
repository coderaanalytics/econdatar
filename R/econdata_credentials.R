econdata_credentials <- function() {
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    stop("Package \"tcltk\" needed for this function to work.",
         call. = FALSE)
  }
  tkn <- NULL # Need to add global bindings for variables
  token   <- tcltk::tclVar("")

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "econdata.co.za credentials")
  tkn.entry <- tcltk::tkentry(tt, textvariable = token)

  reset <- function() tcltk::tclvalue(token) <- ""
  reset.but <- tcltk::tkbutton(tt, text = "Reset", command = reset)

  submit <- function() {
    tkn <- tcltk::tclvalue(token)
    e <- parent.env(environment())
    e$tkn <- tkn
    tcltk::tkdestroy(tt)
  }
  submit.but <- tcltk::tkbutton(tt, text = "Submit", command = submit)

  tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter Token Details"),
                columnspan = 2)
  tcltk::tkgrid(tcltk::tklabel(tt, text = "API Token"),
                tkn.entry, pady = 10, padx = 10)
  tcltk::tkgrid(submit.but, reset.but, pady = 10, padx = 50)

  tcltk::tkwait.window(tt)
  return(tkn)
}
