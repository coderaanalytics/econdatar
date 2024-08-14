\name{read_registry}
\alias{read_registry}
\alias{tidy_data.eds_registry}
\title{
read_registry
}
\description{
Returns the data for the given registry structure - agencyid:id(version).
}
\usage{
read_registry(structure, tidy = FALSE, \dots)

\method{tidy_data}{eds_registry}(x, \dots)
}
\arguments{
  \item{structure}{structure type. One of \code{category-scheme}, \code{codelist}, \code{concept-scheme}, \code{dataflow}, \code{data-structure}, \code{memberlist}, \code{consumption-agreement}, or \code{provision-agreement}}
  \item{x}{A raw API return object to be tidied. Can also be done directly in \code{read_registry()} by setting \code{tidy = TRUE}. See \code{tidy} below for tidying options.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab character. Defaults to \code{ECONDATA}. Agency responsible for the metadata creation/maintenance. \cr
    \code{version} \tab\tab character. Version(s) of the data (different versions will have different metadata), or 'all' to return all available versions. \cr
    \code{file} \tab\tab character. File name for retrieving structures stored as JSON data from disk (output of \code{read_registry()}). \cr
  }
}

\item{tidy}{logical. Return data and metadata in tidy \emph{data.table}'s (see Value), by passing the result through \code{tidy_data}. Currently not used.
}
}
\details{
An EconData account (http://econdata.co.za) is required to use this function. The user must provide an API token that can be found on the \emph{Account} page of the online portal, a GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.
}
\value{
If \code{tidy = FALSE}, a list detailing at a minimum the structure's agencyid, id, version, name, and description, as well as a data.frame with rows containing the data of any child structures.
}
\seealso{
\code{\link{write_registry}}
}
\examples{
\donttest{
# library(econdatar)

# Frequency codelist
CL_FREQ <- read_registry("codelist", id = "CL_FREQ")
}
}
\keyword{ load }
\keyword{ download }
