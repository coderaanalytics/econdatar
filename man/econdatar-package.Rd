\name{econdatar-package}
\alias{econdatar-package}
\alias{econdatar}
\docType{package}
\title{
\packageTitle{econdatar}
}
\description{
\packageDescription{econdatar}
}
\details{

This package provides an interface to Codera Analytic's econometric data service (https://codera.co.za) and  the EconData database (https://www.econdata.co.za) in particular.

EconData enables automation of analytical workflows that depend on public domain or third-party data. It is also a leading-edge forecast management system, enabling data and model automation and best practice data and model governance. EconData supports data-sharing across databases and within institutions, codifies modelling process flows and provides user-level access control. EconData makes it easy to securely manage and share model scenarios and forecast vintages.

The EconData Registry provides a central data glossary of data concepts and the information necessary to access and interpret data and associated metadata.

Codera also uses EconData to automate models, do research, and create value-added products such as interactive scenario dashboards. These dashboards and forecasts are made available to our clients.
}
\author{

Maintainer: \packageMaintainer{econdatar}
}
\keyword{ package }
\seealso{
https://www.econdata.co.za
https://econdata.co.za
}
\examples{
\donttest{
# library(econdatar)

# Return all data sets (useful for browsing available data)
CATALOGUE <- read_database(id = "all", tidy = TRUE)

# Mining production and sales
MINING <- read_dataset(id = "MINING", tidy = TRUE)
}
}
