% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/push_to_github.R
\name{create_issues_github}
\alias{create_issues_github}
\title{Push issues to GitHub}
\usage{
create_issues_github(
  grading_progress_log_path,
  grading_progress_log,
  class_github_name,
  team_grading = FALSE
)
}
\arguments{
\item{grading_progress_log_path}{string; assist-grading() functions save a file which includes information for gradetools's internal use. 
This is that path for that file. Must be a .csv}

\item{grading_progress_log}{tibble created by assist_advanced_grading().}

\item{class_github_name}{string, GitHub name of the class. This input is needed to push feedback and issues to the correct class on GitHub.}

\item{team_grading}{logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)}
}
\description{
Push issues to GitHub
}
\keyword{internal}
