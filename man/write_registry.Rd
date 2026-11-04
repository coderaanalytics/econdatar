\name{write_registry}
\alias{write_registry}
\title{
write_registry
}
\description{
Saves the data of the provided registry structure.
}
\usage{
write_registry(structure, x, method = "update", \dots)
}
\arguments{
  \item{structure}{character. (Metadata structure type.) One of \code{"category-scheme"}, \code{"codelist"}, \code{"concept-scheme"}, \code{"dataflow"}, \code{"data-structure"}, \code{"memberlist"}, \code{"consumption-agreement"}, or \code{"provision-agreement"}.}
  \item{x}{list. Registry structure to upload.}
  \item{method}{Desired method. One of \code{create}, \code{update} or \code{delete}. \code{create} will create a new registry structure with the provided metadata. \code{update} (default) will update an existing structure in the registry. \code{delete} will delete the structure identified by the \code{agencyid}, \code{id} and \code{version} of \code{x}; if \code{x} is \code{NULL} these are taken from \dots instead.}

\item{\dots}{Further \emph{optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as ODS spreadsheet to disk. \cr
    \code{agencyid} \tab\tab character. Agency ID of the structure to delete when \code{x} is \code{NULL}. \cr
    \code{id} \tab\tab character. ID of the structure to delete when \code{x} is \code{NULL}. \cr
    \code{version} \tab\tab character. Version of the structure to delete when \code{x} is \code{NULL}. \cr
}
}
}
\details{
An EconData account is required to use this function. The user must provide an API key, which can be found on the landing page of the online portal. A GUI dialog will prompt the user for their API key. The API key can also be supplied by setting the ECONDATA_APIKEY environment variable.

The functionality provided by \emph{write_registry} is to save the registry structure according to the function arguments. As this makes modifications to the registry, the user calling this function requires \strong{admin} privileges.

Memberlists are read as one row per membership; a member without memberships is a single row whose \code{membership_*} columns are \code{NA}. \code{membership_type} is one of \code{"agency"}, \code{"data consumer"} or \code{"data provider"}. Updating a memberlist replaces it in full, and the server updates members' annotations as they use the service, so a read-modify-write may overwrite concurrent changes---keep the interval between \code{read_registry} and \code{write_registry} short.
}
\value{
No return value, called for side-effects---writes data to server.
}
\seealso{
\code{\link{read_registry}}
}
\examples{
\dontrun{
x <- read_registry("codelist", id = "CL_FREQ")

tmp <- tempdir()

write_registry(x, file = file.path(tmp, "frequency.ods"))
}
}
\keyword{ save }
\keyword{ upload }
