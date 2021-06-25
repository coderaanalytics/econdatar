\name{read_econdata}
\alias{read_econdata}
\title{
read_econdata
}
\description{
Returns the data set for the given dataflow - agencyid:id(version) and data provider - agencyid:id, as a list. Available data sets can be looked up from the data registry (http://www.econdata.co.za/FusionRegistry).
}
\usage{
read_econdata(id, ...)
}
\arguments{
  \item{id}{
Data identifier.
}
  \item{agencyid}{
Agency responsible for the data definition. \emph{Optional.}
}
  \item{version}{
Version of the data definition. \emph{Optional.}
}
  \item{provideragencyid}{
Agency responsible for making the data available. \emph{Optional.}
}
  \item{providerid}{
Provider of the data. \emph{Optional.}
}
  \item{\dots}{
Optional arguments include: \strong{\emph{file}} (character), file name for retrieving JSON data from disk; \strong{\emph{username}} (character), EconData username; \strong{\emph{password}} (character), EconData password.
}
}
\details{
Specifying the full dataflow and data provider details (as opposed to only using the data id) allows more fine-grained control over the data set being queried. This is not necessary if there is only a single definition of the data and a single provider, which is typically the case.

An EconData account (http://app.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.
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
\code{\link{write_econdata}}
}
\examples{
db <- read_econdata("BA900")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }% use one of  RShowDoc("KEYWORDS")
\keyword{ download }% __ONLY ONE__ keyword per line
