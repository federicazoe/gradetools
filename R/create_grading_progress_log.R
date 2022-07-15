#' Prepares grading progress log for gradetools internal use
#'
#' @param grading_progress_log_path string; assist-grading() functions save a file which includes information for gradetools's internal use. This is that path for that file. Must be a .csv
#' @param example_assignment_path string or a vector of strings; file paths to all of the assignments to be graded for one student. This file path structure will be used to determine where the other assignments to be graded are located. The student identifier has to be present somewhere in each of the file paths. The student identifier is assumed to be the only part of the example_assignment_path unique to the student
#' @param example_feedback_path string; file path to one of the assignment feedback files that will be generated as the user grades. This file path structure will be used to determine where the other feedback files will be stored. The student identifier must be present somewhere in the file name and must be the only part of the file path unique to the student. The extension of the feedback file must be one of the following: ".Rmd", ".docx", ".html", ".pdf"
#' @param example_student_identifier string; a student identifier (e.g. name, id, id number, GitHub user name) that is used to identify the student on the roster. This needs to be present somewhere in the example_assignment_path. Currently, if team_grading is set to TRUE the team_identifier needs to be present in this path instead of the student_identifier.
#' @param roster_path string; file path to the class roster csv containing a column named student_identifier. If team_grading is set to TRUE then the class roster also needs to contain a column named team_identifier
#' @param github_issues logical, whether the grader wants to be given the option to create an issue in students' repos or not
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#'
#' @return tibble; with the columns read in from roster_path, including student_identifier, and two new columns, feedback_path_Rmd, feedback_path_to_be_knitted, and assignment_path containing the file paths to the student assignment files and student feedback files
#' 
#' @import stringr
#' @import dplyr
#' @import readr
#' @importFrom svDialogs dlg_message
#' @importFrom fs path_ext_set
#' 
#' @keywords internal
#' 
create_grading_progress_log <- function(
    grading_progress_log_path,
    example_assignment_path,
    example_feedback_path,
    example_student_identifier,
    roster_path,
    github_issues,
    team_grading,
    assignment_folder,
    number_assignment 
) {
  
  # Read in class roster
  grading_progress_log_new <- readr::read_csv(roster_path, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character)) %>% 
    mutate(student_identifier = as.character(student_identifier))
  
  # Save team to student identifier; save team members
  if (team_grading) {
    grading_progress_log_new <- grading_progress_log_new %>% 
      mutate(team_identifier = as.character(team_identifier)) %>% 
      group_by(team_identifier) %>% 
      summarize(students_in_team = str_c(
        student_identifier, 
        collapse = "&&&"
      )) %>% 
      rename(student_identifier = team_identifier)
    
  }
  
  # Make feedback file paths, saved as .Rmd and the one to be knitted to
  grading_progress_log_new <- grading_progress_log_new %>% 
    mutate(feedback_path_Rmd = str_replace_all(
      as.character(fs::path_ext_set(example_feedback_path, ext = "Rmd")), 
      pattern = example_student_identifier, 
      replacement = grading_progress_log_new$student_identifier
    )) %>% 
    mutate(feedback_path_to_be_knitted = str_replace_all(
      as.character(example_feedback_path), 
      pattern = example_student_identifier, 
      replacement = grading_progress_log_new$student_identifier
    )) 
  
  # Make assignment file paths
  grading_progress_log_new <- grading_progress_log_new %>% 
    mutate(assignment_path = NA) %>% 
    mutate(assignment_missing = FALSE)
  
  
  assignment_path <- rep(NA, number_assignment)
  
  # Check file exists at assignment file paths 
  no_assignment_file_paths_work <- TRUE
  
  if(!is.na(assignment_folder)){ # if assignment_folder is defined, use it, and use the code to get all names
    assignment_list <- list.files(assignment_folder)
    
    # match assignments with student identifiers
    for(i in 1:length(grading_progress_log_new$student_identifier)) {
      # get assignment name(s), incl enclosing folder
      assignment_name <- paste(assignment_folder, assignment_list[str_which(string = assignment_list,
                                                                            pattern = grading_progress_log_new$student_identifier[i])],
                               sep = "/")
      # if there is no assignment, assignment_name will become e.g. FOR4934/ which causes errors
      # with opening files during grading. Replace with a word so it can be identified further on as missing 
      # for multiple files, just take the first entry (if there is something there, the assignment is not missing anyway)
      if(assignment_name[1] == paste(assignment_folder, "/", sep = "")){
        assignment_name <- "missing"
      }
      
      # check if there are enough assignments (if multiple are required)
      if(length(assignment_name) < length(assignment_path)){ # if not, fill up with NAs
        assignment_path <- c(assignment_name, rep(NA, (length(assignment_path) - length(assignment_name))))
      } else {
        assignment_path <- assignment_name
      }
      ####### maybe also add a message if there are more than the required assignments? #####
      
      for (j in 1:length(assignment_path)){
        # double-check if the file exists
        if(file.exists(assignment_path[j])) {
          # We need to know if none of the assignment file paths work
          no_assignment_file_paths_work <- FALSE
          
        } else {
          # Note those without an assignment
          grading_progress_log_new$assignment_missing[i] <- TRUE
        }
      }
      
      # Save assignment paths as one string per student
      grading_progress_log_new$assignment_path[i] <- paste(
        assignment_path, 
        collapse = ", "
      )
      
    }
    
  } else { # if the assignment folder is not defined, use original code that replaces student_identifier
    
    for(i in 1:length(grading_progress_log_new$student_identifier)) {
      for (j in 1:length(example_assignment_path)) {
        assignment_path[j] <- str_replace_all(
          example_assignment_path[j], 
          pattern = example_student_identifier, 
          replacement = grading_progress_log_new$student_identifier[i]
        )
        
        if(file.exists(assignment_path[j])) {
          # We need to know if none of the assignment file paths work
          no_assignment_file_paths_work <- FALSE
          
        } else {
          # Note those without an assignment
          grading_progress_log_new$assignment_missing[i] <- TRUE
          
        }
        
      }
      
      # Save assignment paths as one string per student
      grading_progress_log_new$assignment_path[i] <- paste(
        assignment_path, 
        collapse = ", "
      )
      
    }
    
  }

  if (no_assignment_file_paths_work) {
    stop(
      "No assignment file paths matched to assignment files.\nPlease check that the student identifier matched those in the file paths."
    )
  }
  
  # Merge old grade sheet if present or format new one with new columns
  if (file.exists(grading_progress_log_path)) {
    grading_progress_log_old <- readr::read_csv(
      grading_progress_log_path,
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
      "issue_qs",
      "issue_titles", 
      "issue_bodies", 
      "issue_pushed",
      "feedback_pushed",
      "github_repo"
    )
    
    for (github_col in github_cols_to_transfer) {
      # Transfer github column over from old to new grade sheet
      # whenever the column exists
      if (github_col %in% colnames(grading_progress_log_old)) {
        cols_to_transfer <- c(
          cols_to_transfer,
          github_col
        )
      }
    }
    
    grading_progress_log <- grading_progress_log_old %>% 
      select(all_of(cols_to_transfer)) %>% 
      full_join(grading_progress_log_new, by = "student_identifier")
    
    # Add columns needed for issues if github_issues = TRUE but these
    # columns where missing from the previously saved grading progress log
    if (github_issues & !("issue_titles" %in% colnames(grading_progress_log))) {
      grading_progress_log <- grading_progress_log %>% 
        mutate(issue_qs = NA) %>% 
        mutate(issue_titles = NA) %>% 
        mutate(issue_bodies = NA) %>% 
        mutate(issue_pushed = NA)
    }
    
  } else {
    grading_progress_log <- grading_progress_log_new %>% 
      mutate(grading_status = "ungraded") %>% 
      mutate(feedback_codes = NA) %>% 
      mutate(graded_qs = NA) %>% 
      mutate(last_time_graded = as.POSIXlt(NA)) %>% 
      mutate(comments = NA) %>% 
      mutate(comment_qs = NA)
    
    if (github_issues) {
      grading_progress_log <- grading_progress_log %>% 
        mutate(issue_qs = NA) %>% 
        mutate(issue_titles = NA) %>% 
        mutate(issue_bodies = NA) %>% 
        mutate(issue_pushed = NA)
      
    }
    
    grading_progress_log_message <- paste(
      "A grading progress log is being created.",
      "This is for gradetools's internal use.",
      "Please do not delete this file.",
      "It must be retained to save grading progress.\n\n",
      sep = "\n"
    )
    
    dlg_message(grading_progress_log_message, type = "ok")
    
  } 
  
  grading_progress_log
}
