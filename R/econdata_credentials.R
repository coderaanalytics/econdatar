econdata_apikey <- function() {
  key <- NULL # Need to add global bindings for variables

  tryCatch({
    apikey   <- tcltk::tclVar("")

    tt <- tcltk::tktoplevel()
    tcltk::tkwm.title(tt, "econdata.co.za credentials")
    key.entry <- tcltk::tkentry(tt, textvariable = apikey)

    reset <- function() tcltk::tclvalue(apikey) <- ""
    reset.but <- tcltk::tkbutton(tt, text = "Reset", command = reset)

    submit <- function() {
      key <- tcltk::tclvalue(apikey)
      e <- parent.env(environment())
      e$key <- key
      tcltk::tkdestroy(tt)
    }
    submit.but <- tcltk::tkbutton(tt, text = "Submit", command = submit)

    tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter EconData API Key Details"),
                  columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(tt, text = "API Key"),
                  key.entry, pady = 10, padx = 10)
    tcltk::tkgrid(submit.but, reset.but, pady = 10, padx = 50)

    tcltk::tkwait.window(tt)
  }, error = function(e) {
  })

  if (is.null(key) || identical(key, "")) {
    key <- readline(prompt = "Please provide your EconData API key: ")
  }

  return(key)
}
