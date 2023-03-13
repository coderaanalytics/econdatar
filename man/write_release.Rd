\name{write_release}
\alias{write_release}
\title{
write_release
}
\description{
Release data set vintage for the given dataflow - agencyid:id(version) and data provider - agencyid:id, as a list.
}
\usage{
write_release(agencyid, id, version, provideragencyid,
              providerid, releasedescription, ...)
}
\arguments{
  \item{agencyid}{
Agency responsible for the data definition.
}
  \item{id}{
Data identifier.
}
  \item{version}{
Version of the data definition.
}
  \item{provideragencyid}{
Agency responsible for making the data available.
}
  \item{providerid}{
Provider of the data.
}
  \item{releasedescription}{
Description of the new data set vintage.
}
  \item{\dots}{
Further options include: \strong{\emph{username}} (character), EconData username; \strong{\emph{password}} (character), EconData password; \strong{\emph{releasedattime}} (character), date and time for historical releases in ISO format.
}
}
\details{
An EconData account (http://app.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.

The functionality provided by \emph{write_release} is to release a new vintage of the data set described by the function arguments. As this makes modifications to the database the user calling this function requires higher privileges than needed for other \emph{econdatar} functions - determined by the correct data set role (owner). You can look up user roles for a given data set on the EconData app.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{read_release}}
}
\examples{
\dontrun{
write_release("ECONDATA", "BA900", "1.0", "ECONDATA", "SARB",
              releasedescription = paste0("Vintage - ", Sys.Date()))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ release }% use one of  RShowDoc("KEYWORDS")
\keyword{ vintage }% __ONLY ONE__ keyword per line
