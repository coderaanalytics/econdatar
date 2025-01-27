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
  \item{method}{Desired method. One of \code{create} or \code{update}. \code{create} will create a new registry structure with the provided metadata. \code{update} (default) will update an existing structure in the registry.}

\item{\dots}{Further \emph{optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as ODS spreadsheet to disk. \cr
}
}
}
\details{
An \href{https://econdata.co.za/}{EconData} account is required to use this function. The user must provide an API token, which can be found on the \emph{Account} page of the online portal. A GUI dialog will prompt the user for their API token.

The functionality provided by \emph{write_registry} is to save the registry structure according to the function arguments. As this makes modifications to the registry, the user calling this function requires \strong{admin} privileges.
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
