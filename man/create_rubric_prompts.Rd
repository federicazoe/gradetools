% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_rubric_prompts.R
\name{create_rubric_prompts}
\alias{create_rubric_prompts}
\title{Creates assignment rubric prompts}
\usage{
create_rubric_prompts(rubric_list, github_issues)
}
\arguments{
\item{rubric_list}{list of lists. Each sub-list corresponds to a part of the assignment rubric and specifies the name of this part (e.g. "Question 1"), the total points that can be earned for this part, the rubric etc.}

\item{github_issues}{logical, whether the grader wants to be given the option to create an issue in students' repos or not (defaults to FALSE)}
}
\value{
list of prompts. One prompt for each question plus one for overall feedback.
}
\description{
Creates assignment rubric prompts
}
\keyword{internal}
