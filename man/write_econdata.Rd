\name{write_econdata}
\alias{write_econdata}
\title{
write_econdata
}
\description{
Saves the data set. Available data sets can be looked up from the web platform (http://www.econdata.co.za).
}
\usage{
write_econdata(x, create = FALSE, update = FALSE, stage = TRUE, \dots)
}
\arguments{
  \item{x}{Data set to upload.}
  \item{create}{Should a new data set be created. If true, ignores \code{update} and \code{stage} arguments.}
  \item{update}{Should the time series metadata be updated.}
  \item{stage}{Should the time series data be staged for release (added to unreleased data).}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as JSON data to disk. \cr
    \code{username} \tab\tab character. EconData username. \cr
    \code{password} \tab\tab character. EconData password. \cr
}
}
}
\details{
An EconData account (http://www.econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.

The functionality provided by \emph{write_econdata} is to save the data set according to the function arguments. As this makes modifications to the database the user calling this function requires higher privileges than needed for other \emph{econdatar} functions - the user requires membership with the relevant data provider.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{read_econdata}}
\code{\link{write_release}}
}
\examples{
\dontrun{
x <- read_econdata("MINING")

write_econdata(x, file = "mining.json")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ save }% use one of  RShowDoc("KEYWORDS")
\keyword{ upload }% __ONLY ONE__ keyword per line
