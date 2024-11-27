\name{read_registry}
\alias{read_registry}
\alias{tidy_data.eds_registry}
\title{
read_registry
}
\description{
Returns the data for the given registry structure - agencyid:id(version). This function gets used to query high-level metadata concepts on the EconData Registry. This is more of a back-end solution to accessing this information; a GUI Registry is being developed as well.
}
\usage{
read_registry(structure, tidy = FALSE, \dots)

\method{tidy_data}{eds_registry}(x, \dots)
}
\arguments{
  \item{structure}{character. (Metadata structure type.) One of \code{"category-scheme"}, \code{"codelist"}, \code{"concept-scheme"}, \code{"dataflow"}, \code{"data-structure"}, \code{"memberlist"}, \code{"consumption-agreement"}, or \code{"provision-agreement"}.}
  \item{x}{A raw API return object to be tidied. Can also be done directly in \code{read_registry()} by setting \code{tidy = TRUE}. See \code{tidy} below for tidying options.}

\item{\dots}{Further \emph{optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab character. Defaults to \code{ECONDATA}. Agency responsible for the metadata creation/maintenance. \cr
    \code{version} \tab\tab character. Version(s) of the data (different versions will have different metadata), or 'all' to return all available versions. \cr
    \code{file} \tab\tab character. File name for retrieving structures stored as JSON data from disk (output of \code{write_registry()}). \cr
  }
}

\item{tidy}{logical. Return data and metadata in tidy \emph{data.table}'s (see Value), by passing the result through \code{tidy_data}. Currently not used.
}
}
\details{
An \href{http://econdata.co.za}{EconData} account is required to use this function. The user must provide an API token, which can be found on the \emph{Account} page of the online portal. A GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.

"Data structures" can be used across multiple dataflows, stemming from the same data source. This occurs when the data source is large, and split up into smaller dataflows. Apart from the ID, name, description, version, time dimension and value dimension, data structures comprise of an attributes table and a dimensions table. The dimensions make up the series key, which necessarily have to be encoded. The attributes provide extra concepts for the metadata table, and those concepts can either be encoded or in string format. We have copied much of the metadata from this endpoint to our public \href{https://econdata.co.za/user-guide/}{User Guide}, but the Registry still remains the single source of truth.

Encoded concepts use a codelist (another structure type that can be queried with this function). Codelists can be re-used across various dataflows. Codelists live in an agency.

Concept schemes group concepts together into categories, within an agency. Concept schemes (\code{concept_parentid}) are detailed in attribute and dimension tables, in data structures.

Category schemes organize our dataflows into categories, or groups, of dataflows. Multiple categories are grouped into category schemes. Our category schemes include ASISA, Prudential Regulatory Reporting, Public Finance and Accounts, SARB, and StatsSA, and further categories lie below these category schemes in the hierarchy.

The "dataflow" structure returns the latest data on the version, name, and data structure metadata, for a dataflow on our Registry.
}
\value{
If \code{tidy = FALSE}, a list detailing at a minimum the structure's agencyid, id, version, name, and description, as well as a data.frame with rows containing the data of any child structures.
}
\seealso{
\code{\link{write_registry}}
}
\examples{
\dontrun{
# library(econdatar)

# Frequency codelist
CL_FREQ <- read_registry("codelist", id = "CL_FREQ")

# Data structure
read_registry("data-structure", id = "QB")

# Concept scheme
read_registry("concept-scheme", id = "CROSS_DOMAIN_CONCEPTS", agencyid = "SDMX")
}
}
\keyword{ load }
\keyword{ download }
