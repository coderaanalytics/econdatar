\name{read.edata}
\alias{read.edata}
\title{
read.edata
}
\description{
Returns the dataset of time tables matching the input agency id, the data id, and data version from \href{https://www.econdata.co.za}{www.econdata.co.za} server. Available data sets can be looked up from the data registry.
}
\usage{
read.edata(id, ...)
}
\arguments{
  \item{id}{
Data identifier.
}
  \item{\dots}{
Optional arguments include:  \strong{\emph{agencyid}}, agency responsible for the data (defaults to \emph{ECONDATA}). \strong{\emph{version}}, version of the data structure definition (defaults to \emph{latest}); \strong{\emph{dataprovideragencyid}}, the parent agency for the SDMX data provider (defaults to \emph{ECONDATA}); \strong{\emph{dataproviderid}}, the SDMX data provider id (defaults to \emph{ECONDATA}); \strong{\emph{debug}}, boolean indicating whether errors should be suppressed; \strong{\emph{file}}, file name for retrieving JSON data from disk; \strong{\emph{metadata}}, boolean indicating whether the data set metadata should be included in the output; \strong{\emph{nameby}}, a string or vector listing the dimensions that should be used to name the returned series; \strong{\emph{releasedescription}}, a string describing the desired release.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
\code{\link{write.edata}}
}
\examples{
db <- read.edata("BA900", nameby="key")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }% use one of  RShowDoc("KEYWORDS")
\keyword{ download }% __ONLY ONE__ keyword per line
