#' Prepares temporary grade sheet for gradetools internal use
#'
#' @param temp_grade_sheet_path string; assist-grading() functions save a file which includes information for gradetools's internal use. This is that path for that file. Must be a .csv
#' @param example_assignment_path string or a vector of strings; file paths to all of the assignments to be graded for one student. This file path structure will be used to determine where the other assignments to be graded are located. The student identifier has to be present somewhere in each of the file paths. The student identifier is assumed to be the only part of the example_assignment_path unique to the student
#' @param example_feedback_path string; file path to one of the assignment feedback files that will be generated as the user grades. This file path structure will be used to determine where the other feedback files will be stored. The student identifier must be present somewhere in the file name and must be the only part of the file path unique to the student. The extension of the feedback file must be one of the following: ".Rmd", ".docx", ".html", ".pdf"
#' @param example_student_identifier string; a student identifier (e.g. name, id, id number, GitHub user name) that is used to identify the student on the roster. This needs to be present somewhere in the example_assignment_path. Currently, if team_grading is set to TRUE the team_identifier needs to be present in this path instead of the student_identifier.
#' @param roster_path string; file path to the class roster csv containing a column named student_identifier. If team_grading is set to TRUE then the class roster also needs to contain a column named team_identifier
#' @param github_issues logical, whether the grader wants to be given the option to create an issue in students' repos or not
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#'
#' @return tibble; with the columns read in from roster_path, including student_identifier, and two new columns, feedback_path_Rmd, feedback_path_to_be_knitted, and assignment_path containing the file paths to the student assignment files and student feedback files
#' @import stringr
#' @import dplyr
#' @import readr
#' @importFrom svDialogs dlg_message
#' @importFrom fs path_ext_set
create_temp_grade_sheet <- function(
    temp_grade_sheet_path,
    example_assignment_path,
    example_feedback_path,
    example_student_identifier,
    roster_path,
    github_issues,
    team_grading
  ) {
  
  # Read in class roster
  temp_grade_sheet_new <- readr::read_csv(roster_path, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character)) %>% 
    mutate(student_identifier = as.character(student_identifier))
  
  # Save team to student identifier; save team members
  if (team_grading) {
    temp_grade_sheet_new <- temp_grade_sheet_new %>% 
      mutate(team_identifier = as.character(team_identifier)) %>% 
      group_by(team_identifier) %>% 
      summarize(students_in_team = str_c(
        student_identifier, 
        collapse = "&&&"
      )) %>% 
      rename(student_identifier = team_identifier)
    
  }
  
  # Make feedback file paths, saved as .Rmd and the one to be knitted to
  temp_grade_sheet_new <- temp_grade_sheet_new %>% 
    mutate(feedback_path_Rmd = str_replace_all(
      as.character(fs::path_ext_set(example_feedback_path, ext = "Rmd")), 
      pattern = example_student_identifier, 
      replacement = temp_grade_sheet_new$student_identifier
    )) %>% 
    mutate(feedback_path_to_be_knitted = str_replace_all(
      as.character(example_feedback_path), 
      pattern = example_student_identifier, 
      replacement = temp_grade_sheet_new$student_identifier
    )) 
  
  # Make assignment file paths
  temp_grade_sheet_new <- temp_grade_sheet_new %>% 
    mutate(assignment_path = NA) %>% 
    mutate(assignment_missing = FALSE)
  

  assignment_path <- rep(NA, length(example_assignment_path))
  
  # Check file exists at assignment file paths 
  no_assignment_file_paths_work <- TRUE
  
  for(i in 1:length(temp_grade_sheet_new$student_identifier)) {
    for (j in 1:length(example_assignment_path)) {
      assignment_path[j] <- str_replace_all(
          example_assignment_path[j], 
          pattern = example_student_identifier, 
          replacement = temp_grade_sheet_new$student_identifier[i]
        )
      
      if(file.exists(assignment_path[j])) {
        # We need to know if none of the assignment file paths work
        no_assignment_file_paths_work <- FALSE
        
      } else {
        # Note those without an assignment
        temp_grade_sheet_new$assignment_missing[i] <- TRUE
        
      }
      
    }
    
    # Save assignment paths as one string per student
    temp_grade_sheet_new$assignment_path[i] <- paste(
      assignment_path, 
      collapse = ", "
    )

  }
  
  if (no_assignment_file_paths_work) {
    stop(
      "No assignment file paths matched to assignment files.\nPlease check that the student identifier matched those in the file paths."
    )
  }
  
  # Merge old grade sheet if present or format new one with new columns
  if (file.exists(temp_grade_sheet_path)) {
    temp_grade_sheet_old <- readr::read_csv(
      temp_grade_sheet_path,
      show_col_types = FALSE,
      col_types = cols(
        .default = col_character(),
        assignment_missing = col_logical(),
        grade_student = col_logical(),
        last_time_graded = col_datetime()
      )
    )
    
    # New feedback_path, assignment_path and assignment_missing will 
    # always overwrite any old info in those columns
    cols_to_transfer <- c(
      "student_identifier",
      "grading_status", 
      "feedback_codes", 
      "graded_qs",
      "last_time_graded",
      "comments",
      "comment_qs"
    )
    
    github_cols_to_transfer <- c(
      "issue_titles", 
      "issue_bodies", 
      "issue_pushed",
      "feedback_pushed",
      "github_repo"
    )
    
    for (github_col in github_cols_to_transfer) {
      # Transfer github column over from old to new grade sheet
      # whenever the column exists
      if (github_col %in% colnames(temp_grade_sheet_old)) {
        cols_to_transfer <- c(
          cols_to_transfer,
          github_col
        )
      }
    }
    
    temp_grade_sheet <- temp_grade_sheet_old %>% 
      select(all_of(cols_to_transfer)) %>% 
      full_join(temp_grade_sheet_new, by = "student_identifier")
    
    # Add columns needed for issues if github_issues = TRUE but these
    # columns where missing from the previously saved temporary grade sheet
    if (github_issues & !("issue_titles" %in% colnames(temp_grade_sheet))) {
      temp_grade_sheet <- temp_grade_sheet %>% 
        mutate(issue_titles = NA) %>% 
        mutate(issue_bodies = NA) %>% 
        mutate(issue_pushed = NA)
    }
    
  } else {
    temp_grade_sheet <- temp_grade_sheet_new %>% 
      mutate(grading_status = "ungraded") %>% 
      mutate(feedback_codes = NA) %>% 
      mutate(graded_qs = NA) %>% 
      mutate(last_time_graded = as.POSIXlt(NA)) %>% 
      mutate(comments = NA) %>% 
      mutate(comment_qs = NA)

    if (github_issues) {
      temp_grade_sheet <- temp_grade_sheet %>% 
        mutate(issue_titles = NA) %>% 
        mutate(issue_bodies = NA) %>% 
        mutate(issue_pushed = NA)
      
    }
    
    temp_grade_sheet_message <- paste(
      "A temporary grade sheet is being created.",
      "This is for gradetools's internal use.",
      "Please do not delete this file.",
      "It must be retained if the grader does not finish grading and wants to resume,",
      "or if the grader wishes to change points associated with a rubric item.\n\n",
      sep = "\n"
    )
    
    dlg_message(temp_grade_sheet_message, type = "ok")
    
  } 
  
  temp_grade_sheet
}
