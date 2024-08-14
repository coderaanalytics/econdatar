\name{write_dataset}
\alias{write_dataset}
\title{
write_dataset
}
\description{
Saves the data set. Available data sets can be looked up from the web platform (http://econdata.co.za).
}
\usage{
write_dataset(x, method = "stage", \dots)
}
\arguments{
  \item{x}{Data set to upload.}
  \item{method}{Desired method. One of \code{stage} or \code{validate}. \code{stage} will stage the given data making it ready for release. \code{validate} will validate the given data against the schema derived from the data structure definition.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as JSON data to disk. \cr
}
}
}
\details{
An EconData account (http://econdata.co.za) is required to use this function. The user must provide an API token that can be found on the \emph{Account} page of the online portal, a GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.

The functionality provided by \emph{write_dataset} is to save the data set according to the function arguments. As this makes modifications to the database the user calling this function requires higher privileges than needed for other \emph{econdatar} functions - the user requires \emph{membership} with the relevant data provider.
}
\value{
No return value, called for side-effects - writes data to server.
}
\seealso{
\code{\link{read_dataset}}
\code{\link{write_release}}
}
\examples{
\dontrun{
x <- read_dataset("MINING")

tmp <- tempdir()

write_dataset(x, file =  file.path(tmp, "mining.json"))
}
}
\keyword{ save }
\keyword{ upload }
