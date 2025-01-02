\name{write_econdata}
\alias{write_econdata}
\title{
write_econdata
}
\description{
This function is deprecated, in favour of \code{\link{write_dataset}}. During development, please replace this function with the newer function name.

\strong{Stages} or saves the data set. Available data sets can be looked up from the \href{http://www.econdata.co.za}{web platform}.
}
\usage{
write_econdata(x, create = FALSE, update = FALSE, stage = TRUE, \dots)
}
\arguments{
  \item{x}{Data set to upload.}
  \item{create}{Should a new data set be created. If true, ignores \code{update} and \code{stage} arguments.}
  \item{update}{Should the time series metadata be updated.}
  \item{stage}{Should the time series data be staged for release (added to unreleased data).}

\item{\dots}{Further \emph{optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as JSON data to disk. \cr
}
}
}
\details{
An \href{http://econdata.co.za/}{EconData} account is required to use this function. The user must provide an API token, which can be found on the \emph{Account} page of the online portal. A GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.

The functionality provided by \emph{write_econdata} is to save the data set according to the function arguments. As this makes modifications to the database the user calling this function requires higher privileges than needed for other \emph{econdatar} functions---the user requires membership with the relevant data provider.
}
\value{
No return value, called for side-effects---writes data to server.
}
\seealso{
\code{\link{write_dataset}}
\code{\link{read_econdata}}
\code{\link{write_release}}
}
\examples{
\dontrun{
x <- read_econdata("MINING")

tmp <- tempdir()

write_econdata(x, file =  file.path(tmp, "mining.json"))
}
}
\keyword{ save }
\keyword{ upload }
