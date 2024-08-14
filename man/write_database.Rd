\name{write_database}
\alias{write_database}
\title{
write_database
}
\description{
Create or update a data set (metadata including series and their metadata).
}
\usage{
write_database(x, method = "update", \dots)
}
\arguments{
  \item{x}{Data set to upload.}
  \item{method}{Desired method. One of \code{create} or \code{update}. \code{create} will create a new data set with the provided metadata. \code{update} (default) will update an existing data set in the database.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as JSON data to disk. \cr
}
}
}
\details{
An EconData account (http://econdata.co.za) is required to use this function. The user must provide an API token that can be found on the \emph{Account} page of the online portal, a GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.

The functionality provided by \emph{write_database} is to save the data set according to the function arguments. As this makes modifications to the database the user calling this function requires higher privileges than needed for other \emph{econdatar} functions - the user requires \emph{membership} with the relevant data provider.
}
\value{
No return value, called for side-effects - writes data to server.
}
\seealso{
\code{\link{read_database}}
}
\examples{
\dontrun{
x <- read_database("MINING")

write_database(x)
}
}
\keyword{ save }
\keyword{ upload }
