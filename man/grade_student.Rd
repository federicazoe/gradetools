% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grade_student.R
\name{grade_student}
\alias{grade_student}
\title{Leads grader through grading a student}
\usage{
grade_student(
  row,
  grading_progress_log,
  grading_progress_log_path,
  rubric_prompts,
  rubric_list,
  rubric_path,
  github_issues,
  questions_to_grade,
  write_grades_into_feedback
)
}
\arguments{
\item{row}{integer; This number indicates which row of the grading_progress_log is to be used / graded}

\item{grading_progress_log}{data frame; The grading_progress_log is a data frame containing information for gradetools internal use}

\item{grading_progress_log_path}{string; path to grading_progress_log}

\item{rubric_prompts}{list of prompts; One prompt for each question plus one for overall feedback. This is produced by create_rubric_prompts}

\item{rubric_list}{list whose format corresponds to rubric_list, which is used by most functions in this package. This is produced by import_rubric}

\item{rubric_path}{string, path to assignment rubric. This rubric should be created using the function create_rubric_template, then filled in by the user. The rubric file name and column names must not be changed.}

\item{github_issues}{Boolean; indicates if grader wants to be able to create GitHub issues}

\item{questions_to_grade}{vector of strings; names of assignment questions to grade, or "all" to specify all questions should be graded. All questions_to_grade must exactly match ones present in the rubric}

\item{write_grades_into_feedback}{logical, whether to write numeric grades into the feedback file, along with qualitative feedback (defaults to FALSE).}
}
\value{
grading progress log data frame
}
\description{
Leads grader through grading a student
}
\keyword{internal}
