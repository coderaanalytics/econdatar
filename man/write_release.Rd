\name{write_release}
\alias{write_release}
\title{
write_release
}
\description{
Release data set with identifier - ECONDATA:id(version).
}
\usage{
write_release(id, version, description, reset = FALSE, rollback = FALSE, \dots)
}
\arguments{
  \item{id}{
Data identifier.
}
  \item{version}{
Version of the data definition.
}
  \item{description}{
Description of the new data set release/vintage.
}
  \item{reset}{
Should the unreleased data be reset (TRUE) or released (FALSE).
}
  \item{rollback}{
Should the latest release be rolled back (deleted).
}
  \item{\dots}{Further \emph{Optional} arguments:
    \tabular{llll}{
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
write_release(agencyid = "ECONDATA",
              id = "BA900",
              version = "1.0.0",
              description = paste0("Vintage - ", Sys.Date()))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ release }% use one of  RShowDoc("KEYWORDS")
\keyword{ vintage }% __ONLY ONE__ keyword per line
