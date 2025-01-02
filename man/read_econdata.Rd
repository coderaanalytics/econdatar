\name{read_econdata}
\alias{read_econdata}
\alias{tidy_data}
\title{
read_econdata
}
\description{
This function is deprecated, in favour of \code{\link{read_dataset}}. During development, please replace this function with the newer function name.

Returns the data for the given data set - ECONDATA:id(version), as a list, or as tidy \emph{data.table}'s. Available data sets can be looked up from the \href{http://www.econdata.co.za/}{web platform}. Tidying can be done directly within \code{read_econdata()}, or ex-post using \code{tidy_data()}.
}
\usage{
read_econdata(id, tidy = FALSE, \dots)

tidy_data(x, \dots)
}
\arguments{
  \item{id}{Data identifier.}
  \item{x}{A raw API return object to be tidied. Can also be done directly in \code{read_econdata()} by setting \code{tidy = TRUE}. See \code{tidy} below for tidying options.}

\item{\dots}{Further \emph{optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab character. Defaults to \code{ECONDATA}. Agency responsible for the metadata creation/maintenance. \cr
    \code{version} \tab\tab character. Version(s) of the data (different versions will have different metadata), or 'all' to return all available versions. \cr
    \code{series_key} \tab\tab character. A character vector specifying a subset of time series (see the web platform (export function) for details). \cr
    \code{release} \tab\tab character (optionally with format \%Y-\%m-\%dT\%H:\%M:\%S, that will be coerced to a date/time). The release description, which will return the data associated with that release (if the given description matches an existsing release); or a date/time which will return the data as it was at that moment; or 'latest' which will return the latest release; or 'unreleased' which will return any unreleased data (useful for data that is updated more often than it is released, usually daily data). \cr
    \code{file} \tab\tab character. File name for retrieving JSON data from disk. \cr
  }
}

\item{tidy}{logical. Return data and metadata in tidy \emph{data.table}'s (see Value), by passing the result through \code{tidy_data}. If \code{TRUE}, \code{read_econdata()/tidy_data()} admit the following additional arguments:}
\item{\dots}{
  \tabular{llll}{
    \code{wide} \tab\tab logical, default: \code{TRUE}. Returns data in a column-based format, with \code{"label"} and \code{"source_identifier"} attributes to columns (when available) and an overall "metadata" attribute to the table, otherwise a long-format is returned. See Value. \cr

    \code{codelabel} \tab\tab logical, default: \code{FALSE}. If \code{wide = TRUE}, setting \code{codelabel = TRUE} the data key will be used to generate the \code{"label"}, when available. \cr

    \code{combine} \tab\tab logical, default: \code{FALSE}. If \code{wide = FALSE}, setting \code{combine = TRUE} will combine all data and metadata into a single long table, whereas the default \code{FALSE} will return data and metadata in separate tables, for more efficient storage. \cr

    \code{origmeta}  \tab\tab logical, default: \code{FALSE}. If \code{wide = FALSE}, setting \code{origmeta = TRUE} will combine all metadata fields attached to the series in the dataset as they are. The default is to construct a standardized set of metadata variables, and then drop those not observed. See also \code{allmeta}. \cr

    \code{allmeta} \tab\tab logical, default: \code{FALSE}. If \code{wide = FALSE}, setting \code{allmeta = TRUE} always returns the full set of metadata fields, regardless of whether they are recorded for the given dataset. It is also possible that there are series with zero observations in a dataset. Such series are dropped in tidy output, but if \code{combine = FALSE}, \code{allmeta = TRUE} retains their metadata in the metadata table. \cr

    \code{prettymeta} \tab\tab logical, default: \code{TRUE}. Attempts to make the returned metadata more human readable replacing each code category and enumeration with its name. It is advisable to leave this set to \code{TRUE}, in some cases, where speed is paramount you may want to set this flag to \code{FALSE}. If multiple datasets are being querioed this option is automatically set to \code{FALSE}. \cr

   \code{release} \tab\tab logical, default: \code{FALSE}. \code{TRUE} allows you to apply \code{tidy_data()} to objects returned by \code{read_release()}. All other flags to \code{tidy_data()} are ignored.
}
}
}
\details{
An \href{http://econdata.co.za/}{EconData} account is required to use this function. The user must provide an API token, which can be found on the \emph{Account} page of the online portal; a GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.
}
\value{
If \code{tidy = FALSE}, a list of data frames is returned, where the names of the list are the EconData series codes, and each data frame has a single column named 'OBS_VALUE' containing the data, with corresponding dates attached as rownames. Each data frame further has a \code{"metadata"} attribute providing information about the series. The entire list of data frames also has a \code{"metadata"} attribute, providing information about the dataset. If multiple datasets (or versions of a dataset if \code{version} is specified as 'all') are being queried, a list of such lists is returned.

If \code{tidy = TRUE} and \code{wide = TRUE} (the default), a single \emph{data.table} is returned where the first column is the date, and the remaining columns are series named by their EconData codes. Each series has two attributes: \code{"label"} provides a variable label combining important metadata from the \code{"metadata"} attribute in the non-tidy format, and \code{"source.code"} gives the series code assigned by the original data provider. The table has the same dataset-level \code{"metadata"} attribute as the list of data frames if \code{tidy = FALSE}. If multiple datasets (or versions of a dataset if \code{version} is specified as 'all') are being queried, a list of such \emph{data.table}'s is returned.

If \code{tidy = TRUE} and \code{wide = FALSE} and \code{compact = FALSE} (the default), a named list of two \emph{data.table}'s is returned. The first, \code{"data"}, has columns 'code', 'date' and 'value' providing the data in a long format. The second, \code{"metadata"}, provides dataset and series-level matadata, with one row for each series. If \code{compact = TRUE}, these two datasets are combined, where all repetitive content is converted to factors for more efficient storage. If multiple datasets (or versions of a dataset if \code{version} is specified as 'all') are being queried, \code{compact = FALSE} gives a nested list, whereas \code{compact = TRUE} binds everything together to a single long frame. In general, if \code{wide = FALSE}, no attributes are attached to the tables or columns in the tables.
}
\seealso{
\code{\line{read_dataset}}
\code{\link{write_econdata}}
\code{\link{read_release}}
}
\examples{
\dontrun{
# library(econdatar)

# Electricity Generated
ELECTRICITY <- read_econdata(id = "ELECTRICITY")
ELECTRICITY_WIDE <- tidy_data(ELECTRICITY) # Or: read_econdata("ELECTRICITY", tidy = TRUE)
ELECTRICITY_LONG <- tidy_data(ELECTRICITY, wide = FALSE)
# Same as tidy_data(ELECTRICITY, wide = FALSE, combine = TRUE):
with(ELECTRICITY_LONG, metadata[data, on = "data_key"])

# CPI Analytical Series: Different Revisions
CPI_ANL <- read_econdata(id = "CPI_ANL_SERIES", version = "all")
CPI_ANL_WIDE <- tidy_data(CPI_ANL)
CPI_ANL_LONG <- tidy_data(CPI_ANL, wide = FALSE, combine = TRUE)
CPI_ANL_ALLMETA <- tidy_data(CPI_ANL, wide = FALSE, allmeta = TRUE) # v2.0 has some 0-obs series

# Can query a specific version by adding e.g. version = "2.0.0" to the call

# Returns 5-10 years (daily average bond yields) not yet contained in the latest release
# (particularly useful for daily data that is released monthly)
MARKET_RATES <- read_econdata(id = "MARKET_RATES",
                              series_key = "CMJD003.B.A",
                              release = "unreleased")
}
}
\keyword{ load }
\keyword{ download }
