\name{read_release}
\alias{read_release}
\title{
read_release
}
\description{
Returns the dataset releases for the given dataflow - agencyid:id(version) and data provider - agencyid:id, as a list. A number of utilities are provided to filter only the releases of interest, detailed below.
}
\usage{
read_release(id, ..., tidy = FALSE)
}
\arguments{
  \item{id}{Data identifier.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab Agency responsible for the data definition. \cr\cr
    \code{version} \tab\tab Version of the data definition. \cr\cr
    \code{provideragencyid} \tab\tab Agency responsible for making the data available. \cr\cr
    \code{providerid} \tab\tab Provider of the data. \cr\cr

    \code{newest} \tab\tab logical. Returns only the latest release in the results. \cr\cr
    \code{oldest} \tab\tab logical. Returns only the oldest release in the results. \cr\cr
    \code{beforedatetime} \tab\tab datetime. Returns all releases before a certain date (can be used with \code{newest}). \cr\cr
    \code{afterdatetime} \tab\tab datetime. Returns all releases after a certain date (can be used with \code{oldest}). \cr\cr
    \code{includestext} \tab\tab character. Returns all releases that contain the given string in their description (can be used with \code{oldest} or \code{newest}). \cr\cr
    \code{releasedescription} \tab\tab character. Returns the release whose description is most similar to the given string. \cr\cr
    \code{returnrange} \tab\tab boolean. Returns the data set range for each returned release. \cr\cr
    \code{username} \tab\tab character. EconData username. \cr\cr
    \code{password} \tab\tab character. EconData password. \cr\cr
  }
}

\item{tidy}{logical. Return releases in a tidy \emph{data.table} (see Value) with \code{"metadata"} attached in an attribute.}

}
\details{
Specifying the full dataflow and data provider details (as opposed to only using the data id) allows more fine-grained control over the data set being queried. This is not necessary if there is only a single definition of the data and a single provider, which is typically the case.

An EconData account (http://app.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\value{
If \code{tidy = FALSE}, a nested lists containing metadata and release desciption and dates. If \code{tidy = TRUE}, a (list of) \emph{data.table}(s) providing release information, with metadata attached as an attribute.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{write_release}}
}
\examples{
\dontrun{
# library(econdatar)
# Sys.setenv(ECONDATA_CREDENTIALS="username;password")
# Ids see: https://www.econdata.co.za/FusionRegistry/data/datastructure.html

# Bank Balance Sheet (institutional)
BA900_RELEASE <- read_release(id = "BA900")
# Same as: read_release(id = "BA900", tidy = TRUE)
econdata_tidy(BA900_RELEASE, release = TRUE)      # No further options supported.

# Releases for all CPI Revisions
read_econdata(id = "CPI_ANL_SERIES", tidy = TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ release }% use one of  RShowDoc("KEYWORDS")
\keyword{ vintage }% __ONLY ONE__ keyword per line
