\name{read_dataset}
\alias{read_dataset}
\alias{tidy_data.eds_dataset}
\title{
read_dataset
}
\description{
Returns the data for the given data set - ECONDATA:id(version), as a list, or as tidy \emph{data.table}'s. Available data sets can be looked up using \code{read_database()} or from the \href{https://www.econdata.co.za/}{web platform}. Tidying can be done directly within \code{read_dataset()}, or ex-post using \code{tidy_data()}.
}
\usage{
read_dataset(id, tidy = TRUE, \dots)

\method{tidy_data}{eds_dataset}(x, wide = TRUE, \dots)
}
\arguments{
  \item{id}{Data set identifier.}
  \item{x}{A raw API return object to be tidied. Can also be done directly in \code{read_dataset()} by setting \code{tidy = TRUE}. See \code{tidy} below for tidying options.}
  \item{wide}{Specifies whether the tidied data be returned in \emph{wide} or \emph{long} format.}

\item{\dots}{Further \emph{optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab character. Defaults to \code{ECONDATA}. Agency responsible for the metadata creation/maintenance.\cr
    \code{version} \tab\tab character. Version(s) of the data (different versions will have different metadata), or 'all' to return all available versions.\cr
    \code{series_key} \tab\tab character. A character vector specifying a subset of time series (see the web platform (export function) for an example). Select groups of dimensions with a + character and seperate groups with a . character (leave group blank to return all). For example, \code{series_key = "MIN001+MIN002..S"}.\cr
    \code{start_date} \tab\tab any object that can be coerced with as.Date(). The first observation returned by the query.\cr
    \code{end_date} \tab\tab any object that can be coerced with as.Date(). The last observation returned by the query.\cr
    \code{release} \tab\tab character (optionally with format \%Y-\%m-\%dT\%H:\%M:\%S, to be coerced to a date/time). The release description, which will return the data associated with that release (if the given description matches an existsing release); or a date/time which will return the data as it was at the given time; or 'latest' which will return the latest release; or 'unreleased' which will return any unreleased data (useful for data that is updated more often than it is released, e.g. daily data).\cr
    \code{file} \tab\tab character. File name for retrieving data sets stored as JSON data from disk (output of \code{read_dataset()}.\cr
}}
\item{tidy}{logical. Return data and metadata in tidy \emph{data.table}'s (see Value), by passing the result through \code{tidy_data}. If \code{TRUE}, \code{read_dataset()/tidy_data()} admit the following additional arguments:}
\tabular{llll}{
    \code{wide} \tab\tab logical, default: \code{TRUE}. Returns data in a column-based format, with \code{"label"} and \code{"source_identifier"} attributes to columns (when available) and an overall "metadata" attribute to the table, otherwise a long-format is returned. See Value.\cr
    \code{prettify} \tab\tab logical, default: \code{TRUE}. Attempts to make the returned metadata more human readable replacing each code category and enumeration with its name. It is advisable to leave this set to \code{TRUE}, in some cases, where speed is paramount you may want to set this flag to \code{FALSE}. If multiple datasets are being queried this option is automatically set to \code{FALSE}.\cr
    \code{combine} \tab\tab logical, default: \code{FALSE}. If \code{wide = FALSE}, setting \code{combine = TRUE} will combine all data and metadata into a single long table, whereas the default \code{FALSE} will return data and metadata in separate tables, for more efficient storage.\cr
}
}
\details{
An \href{https://econdata.co.za/}{EconData} account is required to use this function. The user must provide an API token, which can be found on the \emph{Account} page of the online portal; a GUI dialog will prompt the user for their API token. Credentials can also be supplied by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "client_id;client_secret", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="client_id;client_secret")}, when available.
}
\value{
If \code{tidy = FALSE}, a list of data frames is returned, where the names of the list are the EconData series codes, and each data frame has a single column named 'OBS_VALUE' containing the data, with corresponding dates attached as rownames. Each data frame further has a \code{"metadata"} attribute providing information about the series. The entire list of data frames also has a \code{"metadata"} attribute, providing information about the dataset. If multiple datasets (or versions of a dataset if \code{version} is specified as 'all') are being queried, a list of such lists is returned.

If \code{tidy = TRUE} and \code{wide = TRUE} (the default), a single \emph{data.table} is returned where the first column is the date, and the remaining columns are series named by their EconData codes. Each series has two attributes: \code{"label"} provides a variable label combining important metadata from the \code{"metadata"} attribute in the non-tidy format, and \code{"source_identifier"} gives the series code assigned by the original data provider where available. The table has the same dataset-level \code{"metadata"} attribute as the list of data frames if \code{tidy = FALSE}. If multiple datasets are being queried, a list of such \emph{data.table}'s is returned.

If \code{tidy = TRUE} and \code{wide = FALSE} and \code{combine = FALSE} (the default), a named list of two \emph{data.table}'s is returned. The first, \code{"data"}, has columns 'series_key', 'time_period' and 'obs_value' providing the data in a long format. The second, \code{"metadata"}, provides dataset and series-level matadata, with one row for each series. If \code{combine = TRUE}, these two datasets are combined, where all repetitive content is converted to factors for more efficient storage. If multiple datasets are being queried, \code{combine = FALSE} gives a nested list, whereas \code{combine = TRUE} binds everything together to a single long frame. 
}
\seealso{
\code{\link{write_dataset}}
\code{\link{read_release}}
}
\examples{
\dontrun{
# library(econdatar)

# Mining production and sales
read_dataset(id = "MINING")

# Tidy options
(MINING <- read_dataset(id = "MINING", tidy = FALSE))
# Same as: read_dataset(id = "MINING", tidy = TRUE)
tidy_data(MINING, wide = TRUE)
tidy_data(MINING, wide = FALSE)
tidy_data(MINING, wide = FALSE, combine = TRUE, prettify = FALSE)

# Can query a specific version by adding e.g. version = "1.0.0" to the call
read_dataset(id = "MINING", version = "all")
read_dataset(id = "MINING", version = "1.0.0")

# Using the series key
read_dataset(id = "MINING", series_key = "MIN001+MIN002..S")
read_dataset(id = "MINING", series_key = c("MIN001+MIN002..S", "MIN009.I.N"))

# Using start and end dates
read_dataset(id = "MINING",
             series_key = "MIN001.I.S",
             start_date = "2010-01-01",
             end_date = Sys.Date()-365)

# Returns 5-10 years (daily average bond yields) not yet contained in the latest release
# (particularly useful for daily data that is released monthly)
read_dataset(id = "MARKET_RATES",
             series_key = "CMJD003.B.A",
             release = "unreleased")

# library(tibble)
POP <- read_dataset(id = "POPULATION_DATA_REG",
                    series_key = "POP...80",
                    tidy = TRUE,
                    wide = FALSE)
str(POP)
print(names(POP$data))
print(names(POP$metadata))
as_tibble(POP$data)
as_tibble(POP$metadata) |> view()
}
}
\keyword{ load }
\keyword{ download }
