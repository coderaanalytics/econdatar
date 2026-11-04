\name{write_database}
\alias{write_database}
\title{
write_database
}
\description{
Create, update or delete a data set (metadata including series and their metadata).
}
\usage{
write_database(x, method = "update", \dots)
}
\arguments{
  \item{x}{Data set to upload. May be \code{NULL} when \code{method = "delete"}, in which case the data set is identified by \code{agencyid}, \code{id} and \code{version} in \dots.}
  \item{method}{Desired method. One of \code{create}, \code{update} or \code{delete}. \code{create} will create a new data set with the provided metadata. \code{update} (default) will update an existing data set in the database. \code{delete} will delete the data set, or only the series given by \code{series_key}.}

\item{\dots}{Further \emph{optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as JSON data to disk. \cr
    \code{series_key} \tab\tab character vector. Series keys to delete when \code{method = "delete"}; if omitted the whole data set is deleted. \cr
    \code{agencyid} \tab\tab character. Agency ID of the data set to delete when \code{x} is \code{NULL}. \cr
    \code{id} \tab\tab character. ID of the data set to delete when \code{x} is \code{NULL}. \cr
    \code{version} \tab\tab character. Version of the data set to delete when \code{x} is \code{NULL}. \cr
}
}
}
\details{
An EconData account is required to use this function. The user must provide an API key, which can be found on the landing page of the online portal. A GUI dialog will prompt the user for their API key. The API key can also be supplied by setting the ECONDATA_APIKEY environment variable.

The functionality provided by \emph{write_database} is to save the data set (structure) onto EconData's server. It is like putting out a bucket, before filling it with water. Very large datasets may need to be stripped of data and updated in chunks, as the API does not accept messages that are too long. Please \code{Sys.sleep(0.7)} in-between each loop.
}
\value{
No return value, called for side-effects---writes data to server.
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
