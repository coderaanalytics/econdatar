\name{write_database}
\alias{write_database}
\title{
write_database
}
\description{
Create or update a data set (metadata including series and their metadata).
}
\usage{
write_database(x, method = "update", \dots)
}
\arguments{
  \item{x}{Data set to upload.}
  \item{method}{Desired method. One of \code{create} or \code{update}. \code{create} will create a new data set with the provided metadata. \code{update} (default) will update an existing data set in the database.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as JSON data to disk. \cr
    \code{username} \tab\tab character. EconData username. \cr
    \code{password} \tab\tab character. EconData password. \cr
}
}
}
\details{
An EconData account (http://econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.

The functionality provided by \emph{write_database} is to save the data set according to the function arguments. As this makes modifications to the database the user calling this function requires higher privileges than needed for other \emph{econdatar} functions - the user requires \emph{membership} with the relevant data provider.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{read_database}}
}
\examples{
\dontrun{
x <- read_database("MINING")

write_database(x)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ save }% use one of  RShowDoc("KEYWORDS")
\keyword{ upload }% __ONLY ONE__ keyword per line
