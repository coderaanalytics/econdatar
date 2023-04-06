\name{write_release}
\alias{write_release}
\title{
write_release
}
\description{
Release data set vintage for the given dataflow - ECONDATA:id(version) and data provider - ECONDATA:id, as a list.
}
\usage{
write_release(id, version, providerid, releasedescription, ...)
}
\arguments{
  \item{id}{
Data identifier.
}
  \item{version}{
Version of the data definition.
}
  \item{providerid}{
Provider of the data.
}
  \item{releasedescription}{
Description of the new data set vintage.
}
  \item{\dots}{Further \emph{Optional} arguments:
    \tabular{llll}{
      \code{username} \tab\tab character. EconData username. \cr\cr
      \code{password} \tab\tab character. EconData password. \cr\cr
      \code{releasedatetime} \tab\tab Date and time for historical releases in ISO format. \cr\cr
    }
  }
}
\details{
An EconData account (http://www.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.

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
