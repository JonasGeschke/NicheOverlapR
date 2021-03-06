% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cast_site.R
\name{cast_site}
\alias{cast_site}
\title{Transform data into cast format}
\usage{
cast_site(data, site = "missing")
}
\arguments{
\item{data}{Input dataset}

\item{site}{EBN (El Bosque Nuevo) or PP (Peru Panguana)}
}
\value{
Monitoring success per date
}
\description{
Takes the input dataset, wich is one row per individual, and transforms it into the cast format, which will be an overview of the monitoring success per date.
}

