\name{write_release}
\alias{write_release}
\title{
write_release
}
\description{
Release data set with identifier - ECONDATA:id(version).
}
\usage{
write_release(id, version, providerid, description, method = "release", ...)
}
\arguments{
  \item{id}{Data identifier.}
  \item{version}{Version of the data definition.}
  \item{providerid}{Data provider identifier.}
  \item{description}{Description of the new data set release/vintage.}
  \item{method}{One of \code{release}, \code{reset}, or \code{rollback}. \code{release} releases staged data, \code{reset} wipes staged data, and \code{rollback} wipes the previous release.}
  \item{\dots}{Further \emph{Optional} arguments:
    \tabular{llll}{
      \code{agencyid} \tab\tab character. Defaults to \code{"ECONDATA"}. Agency responsible for the metadata creation/maintenance. \cr
      \code{release} \tab\tab Date/time. For historical releases with format \%Y-\%m-\%dT\%H:\%M:\%S. \cr
      \code{username} \tab\tab character. EconData username. \cr
      \code{password} \tab\tab character. EconData password. \cr
    }
  }
}
\details{
An EconData account (http://www.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.

The functionality provided by \emph{write_release} is to release, reset, or rollback a release/vintage of the data set according to the function arguments. As this makes modifications to the database the user calling this function requires higher privileges than needed for other \emph{econdatar} functions - the user requires membership with the relevant data provider.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{read_release}}
\code{\link{write_econdata}}
}
\examples{
\dontrun{
write_release(id = "BA900",
              version = "1.0.0",
              description = paste0("Vintage - ", Sys.Date()))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ release }% use one of  RShowDoc("KEYWORDS")
\keyword{ vintage }% __ONLY ONE__ keyword per line
