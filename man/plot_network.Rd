% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_network.R
\name{plot_network}
\alias{plot_network}
\title{Description}
\usage{
plot_network(
  graph,
  number_of_names = 30,
  namevar = "lemma",
  colorvar = "name",
  sizevar = "nrefs",
  colors = c("red", "yellow", "blue")
)
}
\arguments{
\item{number_of_names}{the number of names to display,}

\item{colorvar}{the variable to take into account to define node color}

\item{sizevar}{the variable to take into account to define node size}

\item{colors}{the colors to interpolate to define palette}

\item{data}{a tibble produced with the cross() function}

\item{number_of_nodes}{the number of nodes to display}

\item{margin}{the number of standard deviations to keep nodes}
}
\value{
a tibble
}
\description{
Description
}
\examples{
data=extract_collection("BIOEENVIS", nmax=200)
data_ref_authors=tidy_ref_authors(data)

data_groups=tidy_groups(data_ref_authors,type="people")
data_crossed=HALtere::cross(data_groups)
graph=build_network(data_crossed,data_groups,number_of_nodes=200)

data_groups=tidy_groups(data_ref_authors,type="labs")
data_crossed=HALtere::cross(data_groups)
graph=build_network(data_crossed,data_groups,number_of_nodes=200)

plot_network(graph, colorvar="name")
}
