\name{read_release}
\alias{read_release}
\alias{tidy_data.eds_release}
\title{
read_release
}
\description{
Returns the data for the given data set - ECONDATA:id(version), as a list, or as tidy \emph{data.table}'s. Tidying can be done directly within \code{read_econdata()}, or ex-post using \code{econdata_tidy()}.
}
\usage{
read_release(id, tidy = FALSE, \dots)
\method{tidy_data}{eds_release}(x, \dots)
}
\arguments{
  \item{id}{Data identifier.}
  \item{x}{A raw API return object to be tidied. Can also be done directly in \code{read_release()} by setting \code{tidy = TRUE}. See \code{tidy} below for tidying options.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab character. Defaults to \code{ECONDATA}. Agency responsible for the metadata creation/maintenance. \cr
    \code{version} \tab\tab Version of the data definition. \cr
    \code{before} \tab\tab datetime. Returns all releases before a certain date. \cr
    \code{after} \tab\tab datetime. Returns all releases after a certain date. \cr
    \code{description} \tab\tab character. Returns the release whose description matches the given string. \cr
    \code{username} \tab\tab character. Web username. \cr
    \code{password} \tab\tab character. Web password. \cr
  }
}

\item{tidy}{logical. Return releases in a tidy \emph{data.table} (see Value) with \code{"metadata"} attached in an attribute.}

}
\details{
Specifying the full dataflow and data provider details (as opposed to only using the data id) allows more fine-grained control over the data set being queried. This is not necessary if there is only a single definition of the data and a single provider, which is typically the case.

An EconData account (http://www.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\value{
If \code{tidy = FALSE}, a nested lists containing metadata and release desciption and dates. If \code{tidy = TRUE}, a (list of) \emph{data.table}(s) providing release information, with metadata attached as an attribute.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{write_release}}
\code{\link{read_econdata}}
}
\examples{
\dontrun{
# library(econdatar)
# Sys.setenv(ECONDATA_CREDENTIALS="username;password")
# for ids/versions see: https://www.econdata.co.za/app

# Bank Balance Sheet (institutional)
MINING_RELEASE <- read_release(id = "MINING")
# Same as: read_release(id = "BA900", tidy = TRUE)
econdata_tidy(MINING_RELEASE, is_release = TRUE)      # No further options supported.

# Releases for all CPI Revisions
read_release(id = "CPI_ANL_SERIES", version = "all", tidy = TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ release }% use one of  RShowDoc("KEYWORDS")
\keyword{ vintage }% __ONLY ONE__ keyword per line
