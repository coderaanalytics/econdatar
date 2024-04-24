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
    \code{username} \tab\tab character. Web username. \cr
    \code{password} \tab\tab character. Web password. \cr
  }
}

\item{tidy}{logical. Return data and metadata in tidy \emph{data.table}'s (see Value), by passing the result through \code{tidy_data}.}
}
\details{
An EconData account (http://econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="username;password")}. If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.
}
\value{
%%  ~Describe the value returned
If \code{tidy = FALSE}, an unnamed list of data frames is returned. Each data frame further has a \code{"metadata"} attribute providing information about the data set.

If \code{tidy = TRUE} a single \emph{data.table} is returned with columns for \code{"agencyid"}, \code{"id"}, \code{"version"}, \code{"name"}, and \code{"description"}.

%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{write_database}}
}
\examples{
\dontrun{
# library(econdatar)
# Sys.setenv(ECONDATA_CREDENTIALS="username;password")

# Mining production and sales
MINING <- read_database(id = "MINING")

# Include previous versions
MINING <- read_database(id = "MINING", versions = "all")

# Return series without obervations (empty data set)
MINING <- read_database(id = "MINING", include_series = TRUE)

# Return all data sets (useful for browsing available data)
MINING <- read_database(id = "all")
tidy_data(MINING)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }% use one of  RShowDoc("KEYWORDS")
\keyword{ download }% __ONLY ONE__ keyword per line
