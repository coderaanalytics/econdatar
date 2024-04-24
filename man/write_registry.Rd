\name{write_registry}
\alias{write_registry}
\title{
write_registry
}
\description{
Saves the data of the proided registry structure.
}
\usage{
write_registry(structure, x, method = "update", \dots)
}
\arguments{
  \item{structure}{structure type. One of \code{category-scheme}, \code{codelist}, \code{concept-scheme}, \code{dataflow}, \code{data-structure}, \code{memberlist}, \code{consumption-agreement}, or \code{provision-agreement}}
  \item{x}{Registry structure to upload.}
  \item{method}{Desired method. One of \code{create} or \code{update}. \code{create} will create a new registry structure with the provided metadata. \code{update} (default) will update an existing structure in the registry.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{file} \tab\tab character. File name for saving data set as ODS spreadsheet to disk. \cr
    \code{username} \tab\tab character. EconData username. \cr
    \code{password} \tab\tab character. EconData password. \cr
}
}
}
\details{
An EconData account (http://econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password". If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.

The functionality provided by \emph{write_registry} is to save the registry structure according to the function arguments. As this makes modifications to the registry the user calling this function requires admin privileges.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{read_registry}}
}
\examples{
\dontrun{
x <- read_registry("codelist", id = "CL_FREQ")

write_registry(x, file = "frequency.ods")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ save }% use one of  RShowDoc("KEYWORDS")
\keyword{ upload }% __ONLY ONE__ keyword per line
