\name{read.edata}
\alias{read.edata}
\title{
read.edata
}
\description{
Returns the dataset of time tables matching the input agency id, the data id, and data version from www.econdata.co.za server. Available data sets can be looked up from the data registry.
}
\usage{
read.edata(agencyid, id, version, ...)
}
\arguments{
  \item{agencyid}{
Agency responsible for the data.
}
  \item{id}{
Data identifier.
}
  \item{version}{
Version of the data description.
}
  \item{\dots}{
Optional arguments include: \emph{dataprovider}, the SDMX data provider (defaults to 'INTERNAL'); \emph{dataprovideragencyid}, the parent agency for the SDMX data 
provider (defaults to 'SARB.ERD'); \emph{debug}, boolean indicating whether errors should be suppressed; \emph{file}, file name for retrieving to disk; \emph{key}; \emph{metadata}, boolean indicating whether the data set metadata should be included in the output; \emph{nameby}, a string or vector listing the dimensions that should be used to name the returned series; \emph{releasedescription}, a string describing the desired release.
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
}
\examples{
db <- read.edata("ECONDATA", "BA900", "1.0", nameby="CODE")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }% use one of  RShowDoc("KEYWORDS")
\keyword{ download }% __ONLY ONE__ keyword per line
