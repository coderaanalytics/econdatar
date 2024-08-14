\name{read_database}
\alias{read_database}
\alias{tidy_data.eds_database}
\title{
read_database
}
\description{
Returns the data sets for the given reference - agencyid:id(version), as a list, or as a tidy \emph{data.table}. A collection of data sets in \emph{econdatar} is referred to as a database. Tidying can be done directly within \code{read_database()}, or ex-post using \code{tidy_data()}.
}
\usage{
read_database(id, include_series = FALSE, tidy = FALSE, \dots)

\method{tidy_data}{eds_database}(x, \dots)
}
\arguments{
  \item{id}{Data set identifier. May be \code{"all"}}
  \item{include_series}{If \code{TRUE} series (and their metadata) with obervations omitted are included in the return (series are discarded when \emph{tidy} is \code{TRUE}).}
  \item{x}{A raw API return object to be tidied. Can also be done directly in \code{read_database()} by setting \code{tidy = TRUE}}.

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab character. Defaults to \code{ECONDATA}. Agency responsible for the metadata creation/maintenance. \cr
    \code{version} \tab\tab character. Version(s) of the data (different versions will have different metadata), or \code{"all"} to return all available versions. \cr
  }
}

\item{tidy}{logical. Return data and metadata in tidy \emph{data.table}'s (see Value), by passing the result through \code{tidy_data}.}
}
\details{
An EconData account (http://econdata.co.za) is required to use this function. The user must provide an API token that can be found on the \emph{Account} page of the online portal, a GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.
}
\value{
If \code{tidy = FALSE}, an unnamed list of data frames is returned. Each data frame further has a \code{"metadata"} attribute providing information about the data set.

If \code{tidy = TRUE} a single \emph{data.table} is returned with columns for \code{"agencyid"}, \code{"id"}, \code{"version"}, \code{"name"}, and \code{"description"}.
}
\seealso{
\code{\link{write_database}}
}
\examples{
\donttest{
# library(econdatar)

# Mining production and sales
MINING <- read_database(id = "MINING")

# Include previous versions
MINING <- read_database(id = "MINING", versions = "all")

# Return series without obervations (empty data set)
MINING <- read_database(id = "MINING", include_series = TRUE)

# Return all data sets (useful for browsing available data)
CATALOGUE <- read_database(id = "all")
tidy_data(CATALOGUE)
}
}
\keyword{ load }
\keyword{ download }
