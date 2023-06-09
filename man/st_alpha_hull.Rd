% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_alpha_hull.R
\name{st_alpha_hull}
\alias{st_alpha_hull}
\title{st_alpha_hull}
\usage{
st_alpha_hull(x_in_4326, y_in_4326, alpha_value = 0.01)
}
\arguments{
\item{x_in_4326}{x_in_4326}

\item{y_in_4326}{y_in_4326}

\item{alpha_value}{alpha_value to pass to alphahull package, Default: 0.01}
}
\value{
hull in 6349 (a compound crs with an explicit vertical datum)
}
\description{
gratefully pilfered and wrapper-d around alphahull
}
\details{
Code gratefully pilfered from alphahull package so that I could expose parameters, and renamed incorrectly so that I don't have to wonder what to start my hull auto completes with
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
[ahull][alphahull::ahull]
 [st_as_sf][sf::st_as_sf], [st_set_crs][sf::st_set_crs], [st_crs][sf::st_crs], [st_transform][sf::st_transform]
}
