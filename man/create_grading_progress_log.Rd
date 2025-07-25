% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_grading_progress_log.R
\name{create_grading_progress_log}
\alias{create_grading_progress_log}
\title{Prepares grading progress log for gradetools internal use}
\usage{
create_grading_progress_log(
  grading_progress_log_path,
  example_assignment_path,
  example_feedback_path,
  example_student_identifier,
  roster_path,
  github_issues,
  team_grading
)
}
\arguments{
\item{grading_progress_log_path}{string; assist-grading() functions save a file which includes information for gradetools's internal use. 
This is that path for that file. Must be a .csv}

\item{example_assignment_path}{string; file path(s) to the file(s) constituting one of the submissions to be graded. 
This file path structure will be used to determine where the other submissions to be graded are located. 
The student identifier has to be present somewhere in each file path(s).
If specified as "no_submissions", grading will proceed without automatic interaction with assignments (i.e opening and closing assignments).
If example_assignment_path is a vector of multiple file paths, gradetools will look for each of these files when grading a submission, and open all files it successfully finds for the given student or team.}

\item{example_feedback_path}{string; file path to one of the assignment feedback files that will be generated as the user grades. 
This file path structure will be used to determine where the other feedback files will be stored. 
The student identifier must be present somewhere in the file name and must be the only part of the file path unique to the student. 
The extension of the feedback file must be one of the following: "Rmd", "md", "docx", "html", "pdf". 
For extensions "md", "docx", "html", "pdf", a "qmd" file will be created first and then rendered to the desired format.}

\item{example_student_identifier}{string; a student identifier (e.g. name, id, id number, GitHub user name) that is used to identify the student on the roster. 
This needs to be present somewhere in the example_assignment_path. Currently, if team_grading is set to TRUE the team_identifier needs to be present in this path instead of the student_identifier.}

\item{roster_path}{string; file path to the class roster csv containing a column named student_identifier. 
If team_grading is set to TRUE then the class roster also needs to contain a column named team_identifier}

\item{github_issues}{logical, whether the grader wants to be given the option to create an issue in students' repos or not}

\item{team_grading}{logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)}
}
\value{
tibble; with the columns read in from roster_path, including student_identifier, and two new columns, feedback_path_qmd, feedback_path_to_be_rendered, and assignment_path containing the file paths to the student assignment files and student feedback files
}
\description{
Prepares grading progress log for gradetools internal use
}
\keyword{internal}
