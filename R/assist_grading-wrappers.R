#'
#' @param rubric_path string, path to assignment rubric. 
#'     This rubric should be created using the function create_rubric_template, then filled in by the user. 
#'     The rubric file name and column names must not be changed.
#' @param roster_path string; file path to the class roster csv containing a column named student_identifier. 
#'     If team_grading is set to TRUE then the class roster also needs to contain a column named team_identifier
#' @param grading_progress_log_path string; assist-grading() functions save a file which includes information for gradetools's internal use. 
#'     This is that path for that file. Must be a .csv
#' @param final_grade_sheet_path string; path to save final grade sheet to. Must be a .csv
#' @param example_assignment_path string; file path to one of the assignments to be graded. 
#'     This file path structure will be used to determine where the other assignments to be graded are located. 
#'     The student identifier has to be present somewhere in the file path.
#'     If specified as "no_submissions", grading will proceed without automatic interaction with assignments (i.e opening and closing assignments).
#' @param example_feedback_path string; file path to one of the assignment feedback files that will be generated as the user grades. 
#'     This file path structure will be used to determine where the other feedback files will be stored. 
#'     The student identifier must be present somewhere in the file name and must be the only part of the file path unique to the student. 
#'     The extension of the feedback file must be one of the following: "Rmd", "qmd", "md", "docx", "html", "pdf". 
#'     These file types (except the first two) will be knitted to: Markdown, Word, html, and pdf documents respectively
#' @param example_student_identifier string; a student identifier (e.g. name, id, id number, GitHub user name) that is used to identify the student on the roster. 
#'     This needs to be present somewhere in the example_assignment_path. The student_identifier needs to be the GitHub user name if the user wishes to push issues or feedback to GitHub later
#' @param example_team_identifier string; Used instead of example_student_identifier when grading team assignments with assist_team_grading(). 
#'     A team identifier (e.g. "team1", "team2", etc.) that is used to identify what team the student on the roster is in. 
#'     This needs to be present somewhere in the example_assignment_path.
#' @param missing_assignment_grade numeric; The grade to assign a student with no assignment submission
#' @param questions_to_grade vector of strings; names of assignment questions to grade, or "all" to specify all questions should be graded. 
#'     All questions_to_grade must exactly match ones present in the rubric
#' @param students_to_grade vector of strings; student_identifiers corresponding to students to grade, or "all" to specify all students should be graded. 
#'     All students_to_grade must be student_identifiers present in the roster
#' @param teams_to_grade vector of strings; team_identifiers corresponding to teams to grade, or "all" to specify all assignments should be graded. 
#'     Team grading is when the grade of an assignment is share among multiple students. 
#'     All teams_to_grade must be team_identifiers present in the roster
#' @param github_issues logical, whether the grader wants to be given the option to create an issue in students' repos or not (defaults to FALSE)
#'
#' @name assist_grading_functions
#' @description Functions to assist with grading and providing personalized feedback to students. \code{assist_grading()} requires minimal user input, while \code{assist_team_grading()} and \code{assist_advanced_grading()} offer more user control and allow for team grading and grading assignments from GitHub
NULL


#' @rdname assist_grading_functions 
#' @title Assisted grading
#' @export
assist_grading <- function(
    rubric_path,
    roster_path,
    grading_progress_log_path,
    final_grade_sheet_path,
    example_assignment_path,
    example_feedback_path,
    example_student_identifier,
    missing_assignment_grade = NA
) {
  
  core_assist_grading(
    rubric_path = rubric_path,
    roster_path = roster_path,
    grading_progress_log_path = grading_progress_log_path,
    final_grade_sheet_path = final_grade_sheet_path,
    example_assignment_path = example_assignment_path,
    example_feedback_path = example_feedback_path,
    example_student_identifier = example_student_identifier,
    missing_assignment_grade = missing_assignment_grade,
    questions_to_grade = "all",
    students_to_grade = "all",
    team_grading = FALSE,
    github_issues = FALSE
  )
  
}


#' @rdname assist_grading_functions
#' @title Team assisted grading
#' @export
assist_team_grading <- function(
  rubric_path,
  roster_path,
  grading_progress_log_path,
  final_grade_sheet_path,
  example_assignment_path,
  example_feedback_path,
  example_team_identifier,
  missing_assignment_grade = NA,
  questions_to_grade = "all",
  teams_to_grade = "all",
  github_issues = FALSE
) {
  
  core_assist_grading(
    rubric_path = rubric_path,
    roster_path = roster_path,
    grading_progress_log_path = grading_progress_log_path,
    final_grade_sheet_path = final_grade_sheet_path,
    example_assignment_path = example_assignment_path,
    example_feedback_path = example_feedback_path,
    example_student_identifier = example_team_identifier,
    missing_assignment_grade = missing_assignment_grade,
    questions_to_grade = questions_to_grade,
    students_to_grade = teams_to_grade,
    team_grading = TRUE,
    github_issues = github_issues
  )
}


#' @rdname assist_grading_functions
#' @title Advanced assisted grading
#' @export
assist_advanced_grading <- function(
  rubric_path,
  roster_path,
  grading_progress_log_path,
  final_grade_sheet_path,
  example_assignment_path,
  example_feedback_path,
  example_student_identifier,
  missing_assignment_grade = NA,
  questions_to_grade = "all",
  students_to_grade = "all",
  github_issues = FALSE
) {
  
  core_assist_grading(
    rubric_path = rubric_path,
    roster_path = roster_path,
    grading_progress_log_path = grading_progress_log_path,
    final_grade_sheet_path = final_grade_sheet_path,
    example_assignment_path = example_assignment_path,
    example_feedback_path = example_feedback_path,
    example_student_identifier = example_student_identifier,
    missing_assignment_grade = missing_assignment_grade,
    questions_to_grade = questions_to_grade,
    students_to_grade = students_to_grade,
    team_grading = FALSE,
    github_issues = github_issues
  )
  
}
