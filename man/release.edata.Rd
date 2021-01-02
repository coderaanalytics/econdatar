\name{release.edata}
\alias{release.edata}
\title{
release.edata
}
\description{
Returns the dataset releases for the given dataflow agencyid:id(version) as a table. A number of utilities are provided to filter only the releases of interest, detailed below.
}
\usage{
release.edata(agencyid, id, version, ...)
}
\arguments{
  \item{agencyid}{
Agency respobsible for the data.
}
  \item{id}{
Data identifier.
}
  \item{version}{
Version of the data description.
}
  \item{\dots}{
Optionally, you can also specify: \emph{newest} (boolean), returns only the latest release in the results; \emph{oldest} (boolean), returns only the oldest release in the results; \emph{beforedatetime} (datetime), returns all releases before a certain date (can be used with \emph{newest}); \emph{afterdatetime} (datetime), returns all releases after a certain date (can be used with \emph{oldest}); \emph{includestext} (character), returns all releases that contain the given string in their description (can be used with \emph{oldest} or \emph{newest}); \emph{releasedescription} (character), returns the release whose description is most similar to the given string, or releases data with this description; \emph{addrelease} (boolean), add release with given description; \emph{debug} (boolean), suppresses errors.
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
release.edata("ECONDATA", "BA900", "1.0")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ release }% use one of  RShowDoc("KEYWORDS")
\keyword{ vintage }% __ONLY ONE__ keyword per line
