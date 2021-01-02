\name{write.edata}
\alias{write.edata}
\title{
write.edata
}
\description{
Takes as input a database of timetables and posts the data to the www.econdata.co.za server. Alternatively the data can be saved to disk as an JSON file. It is required to specify the agency id, the data id, and data version; which can be looked up in the data registry.
}
\usage{
write.edata(db, agencyid, id, version, ...)
}
\arguments{
  \item{db}{
Dataset to upload.
}
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
Optionally, you can also specify: \emph{releasedescription}, if specified the data is released with the given description - use only if you are ready to release the uploaded data; \emph{file}, location and name of XML file to be saved.
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
\code{\link{spartaload}}
}
\examples{
db <- spartaload("ECONDATA", "BA900", "1.0", nameby="CODE")

write.edata(db, "ECONDATA", "BA900", "1.0", file="ba900.json")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ save }% use one of  RShowDoc("KEYWORDS")
\keyword{ upload }% __ONLY ONE__ keyword per line
