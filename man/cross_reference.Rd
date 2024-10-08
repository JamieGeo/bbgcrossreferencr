% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_reference.R
\name{cross_reference}
\alias{cross_reference}
\title{Cross-reference List of all IRIS Plants with Heritage Lists}
\usage{
cross_reference(heritage, allplants, themecode, themetype)
}
\arguments{
\item{heritage}{a dataframe containing the list of heritage plants of concern}

\item{allplants}{a dataframe containing the entire catalogue of plants on IRIS}

\item{themecode}{the numerical code associated with that heritage list}

\item{themetype}{description of the heritage list theme}
}
\value{
A saved table containing plant names that appears on the heritage
list as well as the all plant list on IRIS
}
\description{
Cross-reference List of all IRIS Plants with Heritage Lists
}
\examples{
themecode <- "O1893"
themetype <- e.g. "Catalogue of Orchids 1893"
}
