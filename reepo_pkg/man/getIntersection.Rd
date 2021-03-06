% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_functions.R
\name{getIntersection}
\alias{getIntersection}
\title{General function for updating the values of a field in a list of in clauses
  based on the intersection of two lists of possible values for that field}
\usage{
getIntersection(field, dbTable, queryIns, insList)
}
\arguments{
\item{field}{the field to update in the list of in clauses}

\item{dbTable}{the database table to query to get a list of possible values
for field}

\item{queryIns}{the list of in clauses to use in the sql query to get a list
of possible values for field}

\item{insList}{the list of in clauses containing the original list of
possible values for field}
}
\description{
General function for updating the values of a field in a list of in clauses
  based on the intersection of two lists of possible values for that field
}
