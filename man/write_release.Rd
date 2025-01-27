\name{write_release}
\alias{write_release}
\title{
write_release
}
\description{
Release data set with identifier --- ECONDATA:id(version). Moves the staged dataset to a vintage.
}
\usage{
write_release(id, version, description, method = "release", ...)
}
\arguments{
  \item{id}{character. Data identifier.}
  \item{version}{character. Version of the data definition.}
  \item{description}{character. Description of the vintage. This focuses on the reference period, and may additionally have the publication date (not time) in brackets.}
  \item{method}{One of \code{release}, \code{reset}, or \code{rollback}. \code{release} releases staged data, \code{reset} wipes staged data, and \code{rollback} wipes the previous release.}
  \item{\dots}{Further \emph{optional} arguments:
    \tabular{llll}{
      \code{agencyid} \tab\tab character. Defaults to \code{"ECONDATA"}. Agency responsible for the metadata creation/maintenance. \cr
      \code{release} \tab\tab timestamp. Date & time of publication by the data provider. For historical releases, with format \code{\%Y-\%m-\%dT\%H:\%M:\%S}. Defaults to \code{format(Sys.time(), "\%Y-\%m-\%dT\%H:\%M:\%S", tz = "Africa/Johannesburg")}.\cr
    }
  }
}
\details{
An \href{https://econdata.co.za}{EconData} account is required to use this function. The user must provide an API token, which can be found on the \emph{Account} page of the online portal. A GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.

The functionality provided by \emph{write_release} is to release, reset, or rollback a release/vintage of the data set according to the function arguments. As this makes modifications to the database, the user calling this function requires higher privileges than needed for other \emph{econdatar} functions---the user requires membership with the relevant data provider.
}
\value{
No return value, called for side-effects---writes data to server.
}
\seealso{
\code{\link{read_release}}
\code{\link{write_econdata}}
}
\examples{
\dontrun{
write_release(id = "BA900",
              version = "1.0.0",
              description = paste0("Vintage - ", Sys.Date()))

write_release(id = "RETAIL",
              version = "1.1.0",
              description = "Aug 2024 (2024-10-16)",
              release = "2024-10-16T14:30:00")
}
}
\keyword{ release }
\keyword{ vintage }
