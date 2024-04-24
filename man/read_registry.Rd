\name{read_registry}
\alias{read_registry}
\alias{tidy_data.eds_registry}
\title{
read_registry
}
\description{
Returns the data for the given registry structure - agencyid:id(version).
}
\usage{
read_registry(structure, tidy = FALSE, \dots)

\method{tidy_data}{eds_registry}(x, \dots)
}
\arguments{
  \item{structure}{structure type. One of \code{category-scheme}, \code{codelist}, \code{concept-scheme}, \code{dataflow}, \code{data-structure}, \code{memberlist}, \code{consumption-agreement}, or \code{provision-agreement}}
  \item{x}{A raw API return object to be tidied. Can also be done directly in \code{read_registry()} by setting \code{tidy = TRUE}. See \code{tidy} below for tidying options.}

\item{\dots}{Further \emph{Optional} arguments:
  \tabular{llll}{
    \code{agencyid} \tab\tab character. Defaults to \code{ECONDATA}. Agency responsible for the metadata creation/maintenance. \cr
    \code{version} \tab\tab character. Version(s) of the data (different versions will have different metadata), or 'all' to return all available versions. \cr
    \code{file} \tab\tab character. File name for retrieving structures stored as JSON data from disk (output of \code{read_registry()}). \cr
    \code{username} \tab\tab character. Web username. \cr
    \code{password} \tab\tab character. Web password. \cr
  }
}

\item{tidy}{logical. Return data and metadata in tidy \emph{data.table}'s (see Value), by passing the result through \code{tidy_data}. Currently not used.
}
}
\details{
An EconData account (http://econdata.co.za) is required to use this function. The user must provide their credentials either through the function arguments, or by setting the ECONDATA_CREDENTIALS environment variable using the syntax: "username;password", e.g. \code{Sys.setenv(ECONDATA_CREDENTIALS="username;password")}. If credentials are not supplied by the aforementioned methods a GUI dialog will prompt the user for credentials.
}
\value{
%%  ~Describe the value returned
If \code{tidy = FALSE}, a list detailing at a minimum the structure's agencyid, id, version, name, and description, as well as a data.frame with rows containing the data of any child structures.

%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{write_registry}}
}
\examples{
\dontrun{
# library(econdatar)
# Sys.setenv(ECONDATA_CREDENTIALS="username;password")

# Frequency codelist
CL_FREQ <- read_registry("codelist", id = "CL_FREQ")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }% use one of  RShowDoc("KEYWORDS")
\keyword{ download }% __ONLY ONE__ keyword per line
