#' Delete feedback and grades for specified students and questions
#'
#' @param grading_progress_log_path string; path to grading progress log produced by assign_grade
#' @param rubric_path string; path to assignment rubric.
#' @param identifier string; single student_identifier (or team_identifier) to have grading progress deleted.
#' @param questions_to_delete string; vector of assignment question 'names' to have grade and feedback deleted. 
#'     This can be specified as "all" to apply to all questions.
#' @param github_issues logical, whether the grader wants to be given the option to create an issue in students' repos or not (defaults to FALSE)
#'
#' @import readr
#' @import stringr
#' 
#' @keywords internal
#'
delete_student_grading_progress <- function(
    grading_progress_log_path, 
    rubric_path,
    identifier,
    questions_to_delete,
    github_issues
  ) {
  
  grading_progress_log <- readr::read_csv(
    grading_progress_log_path,
    show_col_types = FALSE,
    col_types = cols(
      .default = readr::col_character(),
      assignment_missing = readr::col_logical(),
      grade_student = readr::col_logical(),
      last_time_graded = readr::col_datetime()
    )
  )

  # extract row
  row_to_change <- grading_progress_log$student_identifier == identifier
  curr_row <- grading_progress_log[row_to_change, ]
  
  # Delete old feedback file
  unlink(curr_row$feedback_path_qmd)
  # Delete knitted feedback if present
  unlink(curr_row$feedback_path_to_be_rendered)
  
  # Set feedback_pushed = FALSE if column is present
  if ("feedback_pushed" %in% colnames(curr_row)){
    curr_row$feedback_pushed <- "FALSE"
  } 
  
  curr_row <- remove_associated_info(
    curr_row = curr_row, 
    questions_to_delete = questions_to_delete, 
    question_col = "graded_qs", 
    info_cols = "feedback_codes"
  )
    
  if (!is.na(curr_row$comment_qs)) {
    curr_row <- remove_associated_info(
      curr_row = curr_row, 
      questions_to_delete = questions_to_delete, 
      question_col = "comment_qs", 
      info_cols = "comments"
    )
  }
    
  if ("issue_qs" %in% colnames(curr_row)) {
    curr_row <- remove_associated_info(
      curr_row = curr_row, 
      questions_to_delete = questions_to_delete, 
      question_col = "issue_qs", 
      info_cols = c("issue_titles", "issue_bodies")
    )
  }
    
  # Update feedback and grading progress log
  rubric_list <- import_rubric(rubric_path)
  rubric_prompts <- create_rubric_prompts(
    rubric_list, 
    github_issues = github_issues
  )
  
  if (!is.na(curr_row$graded_qs)) {
  grade_info <- assign_grade_write_feedback(
    grading_progress_log_row = curr_row, 
    rubric_list = rubric_list,
    rubric_prompts = rubric_prompts
  )
  
  curr_row$grading_status <- grade_info$grading_status
  curr_row$last_time_graded <- Sys.time()
  
  } else {
    curr_row$grading_status <- "ungraded"
    curr_row$last_time_graded <- NA
    
  }
  
  grading_progress_log[row_to_change, ] <- curr_row
  
  grading_progress_log
  
}


#' Helper function for deleting info
#' @param curr_row string; row of grading progress log to change
#' @param questions_to_delete string; vector of assignment question 'names' to have grade and feedback deleted. 
#' @param question_col string; name of column containing question names for associated info
#' @param info_cols vector of strings; names of columns containing info to be deleted
#' 
#' @import stringr
#' 
#' @keywords internal
#' 
remove_associated_info <- function(
    curr_row, 
    questions_to_delete, 
    question_col, 
    info_cols
  ){
  
  info_qs <- unlist(stringr::str_split(curr_row[, question_col], pattern = "&&&"))
  
  issue_pushing_present <- question_col == "issue_qs" && 
    "issue_pushed" %in% colnames(curr_row)
  
  if (issue_pushing_present) {
    if (is.na(curr_row$issue_pushed)) {
      issue_pushed_vec <- FALSE
    } else {
      issue_pushed_vec <-as.logical(unlist(stringr::str_split(
        curr_row$issue_pushed, 
        pattern = "&&&"
      )))
    }
  }
  
  for (col in info_cols) {
    info <- unlist(stringr::str_split(curr_row[, col], pattern = "&&&"))
    
    keep_info <- !(info_qs %in% questions_to_delete)
    
    if (issue_pushing_present) {
      keep_info <- keep_info | issue_pushed_vec
    }
    
    curr_row[, col] <- ifelse(
      any(keep_info),
      yes = stringr::str_c(info[keep_info], collapse = "&&&"),
      no = NA
    )
    
    curr_row[, question_col] <- ifelse(
      any(keep_info),
      stringr::str_c(info_qs[keep_info], collapse = "&&&"),
      no = NA
    )
    
  }
  
  if (issue_pushing_present) {
    curr_row$issue_pushed <- ifelse(
      any(keep_info),
      yes = stringr::str_c(issue_pushed_vec[keep_info], collapse = "&&&"),
      no = NA
    )
  }

  curr_row
  
}
