\name{write_econdata}
\alias{write_econdata}
\title{
write_econdata
}
\description{
Saves the data set for the given dataflow - agencyid:id(version) and data provider - agencyid:id, as a list. Available data sets can be looked up from the data registry (http://www.econdata.co.za/FusionRegistry).
}
\usage{
write_econdata(db, agencyid, id, version, provideragencyid, providerid, ...)
}
\arguments{
  \item{db}{
Data set to upload.
}
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
  \item{\dots}{
Further options include: \strong{\emph{file}} (character), file name for saving data set as JSON data to disk; \strong{\emph{username}} (character), EconData username; \strong{\emph{password}} (character), EconData password..
}
}
\details{
An EconData account (http://app.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.

The functionality provided by \emph{write_econdata} is to save the data set described by the function arguments. As this makes modifications to the database the user calling this function requires higher privileges than needed for other \emph{econdatar} functions - determined by the correct data set role (owner). You can look up user roles for a given data set on the EconData app.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{read_econdata}}
}
\examples{
db <- read_econdata("BA900", nameby="CODE")

write_econdata(db, "ECONDATA", "BA900", "1.0", "ECONDATA", "SARB", file = "ba900.json")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ save }% use one of  RShowDoc("KEYWORDS")
\keyword{ upload }% __ONLY ONE__ keyword per line
