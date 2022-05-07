\name{read_release}
\alias{read_release}
\title{
read_release
}
\description{
Returns the dataset releases for the given dataflow - agencyid:id(version) and data provider - agencyid:id, as a list. A number of utilities are provided to filter only the releases of interest, detailed below.
}
\usage{
read_release(id, ...)
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
Further options include: \strong{\emph{newest}} (boolean), returns only the latest release in the results; \strong{\emph{oldest}} (boolean), returns only the oldest release in the results; \strong{\emph{beforedatetime}} (datetime), returns all releases before a certain date (can be used with \emph{newest}); \strong{\emph{afterdatetime}} (datetime), returns all releases after a certain date (can be used with \emph{oldest}); \strong{\emph{includestext}} (character), returns all releases that contain the given string in their description (can be used with \emph{oldest} or \emph{newest}); \strong{\emph{releasedescription}} (character), returns the release whose description is most similar to the given string; \strong{\emph{returnrange}} (boolean), returns the data set range for each returned release; \strong{\emph{username}} (character), EconData username; \strong{\emph{password}} (character), EconData password.
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
\code{\link{write_release}}
}
\examples{
read_release("BA900")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ release }% use one of  RShowDoc("KEYWORDS")
\keyword{ vintage }% __ONLY ONE__ keyword per line
