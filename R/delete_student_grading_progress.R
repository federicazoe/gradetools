#' Delete feedback and grades for specified students and questions
#'
#' @param temp_grade_sheet_path string; path to temporary grade sheet produced by assign_grade
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
    temp_grade_sheet_path, 
    rubric_path,
    identifier,
    questions_to_delete,
    github_issues
  ) {
  
  temp_grade_sheet <- readr::read_csv(
    temp_grade_sheet_path,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_character(),
      assignment_missing = col_logical(),
      grade_student = col_logical(),
      last_time_graded = col_datetime()
    )
  )

  # extract row
  row_to_change <- temp_grade_sheet$student_identifier == identifier
  curr_row <- temp_grade_sheet[row_to_change, ]
  
  # Delete old feedback file
  unlink(curr_row$feedback_path_Rmd)
  # Delete knitted feedback if present
  unlink(curr_row$feedback_path_to_be_knitted)
  
  # Set feedback_pushed = FALSE if column is present
  if ("feedback_pushed" %in% colnames(curr_row)){
    curr_row$feedback_pushed <- FALSE
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
    
  # Update feedback and temporary grade sheet
  rubric_list <- import_rubric(rubric_path)
  rubric_prompts <- create_rubric_prompts(
    rubric_list, 
    github_issues = github_issues
  )
  
  grade_info <- assign_grade_write_feedback(
    temp_grade_sheet_row = curr_row, 
    rubric_list = rubric_list,
    rubric_prompts = rubric_prompts
  )
  
  curr_row$grading_status <- grade_info$grading_status
  curr_row$last_time_graded <- Sys.time()
  
  temp_grade_sheet[row_to_change, ] <- curr_row
  
  temp_grade_sheet
  
}


#' Helper function for deleting info
#' @param curr_row string; row of temporary grade sheet to change
#' @param questions_to_delete string; vector of assignment question 'names' to have grade and feedback deleted. 
#' @param question_col string; name of column containing question names for associated info
#' @param info_cols vector of strings; names of columns containing info to be deleted
#' 
#' @keywords internal
#' 
remove_associated_info <- function(
    curr_row, 
    questions_to_delete, 
    question_col, 
    info_cols
  ){
  
  info_qs <- unlist(str_split(curr_row[, question_col], pattern = "&&&"))
  
  issue_pushing_present <- question_col == "issue_qs" && 
    "issue_pushed" %in% colnames(curr_row)
  
  if (issue_pushing_present) {
    issue_pushed_vec <- ifelse(
      is.na(curr_row$issue_pushed),
      yes = FALSE,
      no = as.logical(unlist(str_split(
        curr_row$issue_pushed, 
        pattern = "&&&"
      )))
    )
    
  }
  
  for (col in info_cols) {
    info <- unlist(str_split(curr_row[, col], pattern = "&&&"))
    
    keep_info <- !(info_qs %in% questions_to_delete)
    
    if (issue_pushing_present) {
      keep_info <- keep_info & !issue_pushed_vec
    }
    
    curr_row[, col] <- ifelse(
      any(keep_info),
      yes = str_c(info[keep_info], collapse = "&&&"),
      no = NA
    )
    
    curr_row[, question_col] <- ifelse(
      any(keep_info),
      str_c(info_qs[keep_info], collapse = "&&&"),
      no = NA
    )
    
  }
  
  if (issue_pushing_present) {
    curr_row$issue_pushed <- ifelse(
      any(keep_info),
      yes = str_c(issue_pushed_vec[keep_info], collapse = "&&&"),
      no = NA
    )
  }
  
  curr_row
  
}