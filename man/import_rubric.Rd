% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_rubric.R
\name{import_rubric}
\alias{import_rubric}
\title{Reads in assignment rubric to produce rubric_list. Alternative to defining rubric as rubric_list manually}
\usage{
import_rubric(rubric_path)
}
\arguments{
\item{rubric_path}{string, path to assignment rubric. This rubric should be created using the function create_rubric_template, then filled in by the user. The rubric file name and column names must not be changed.}
}
\value{
list whose format corresponds to rubric_list, which is used by most functions in this package
}
\description{
Reads in assignment rubric to produce rubric_list. Alternative to defining rubric as rubric_list manually
}
\keyword{internal}
