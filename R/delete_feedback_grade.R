#' Delete feedback and grades for specified students and questions
#'
#' @param temp_grade_sheet_path string; path to temporary grade sheet produced by assign_grade
#' @param rubric_path string, path to assignment rubric. This rubric should be created using the function create_rubric_template, then filled in by the user. The rubric file name and column names must not be changed.
#' @param student_ids string; vector of student identifiers to have grade and feedback deleted. This can be specified as "all" to apply to all student's in the rubric
#' @param questions_to_delete string; vector of assignment question 'names' to have grade and feedback deleted. This can be specified as "all" to apply to all questions.
#'
#' @import readr
#' @import stringr
#'
delete_feedback_grade <- function(
    temp_grade_sheet_path, 
    rubric_path,
    student_ids,
    questions_to_delete
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
  
  og_student_identifiers <- temp_grade_sheet$student_identifier
  
  if (student_ids == "all") {
    student_ids <- temp_grade_sheet$student_identifier
  }
  
  unchanged_grade_sheet <- temp_grade_sheet %>% 
    filter(!(student_identifier %in% student_ids))
  
  changed_grade_sheet <- temp_grade_sheet %>% 
    filter(student_identifier %in% student_ids)
  
  # Delete old feedback file
  unlink(changed_grade_sheet$feedback_path_Rmd)
  # Delete knitted feedback if present
  unlink(changed_grade_sheet$feedback_path_to_be_knitted)
  # Set feedback_pushed = FALSE if column is present
  if ("feedback_pushed" %in% colnames(changed_grade_sheet)){
    changed_grade_sheet$feedback_pushed <- FALSE
  } 
  
  if ("issue_titles" %in% colnames(changed_grade_sheet)) {
    github_issues <- TRUE
  } else {
    github_issues <- FALSE
  }
    
  if (questions_to_delete == "all") {
    changed_grade_sheet <- changed_grade_sheet %>% 
      mutate(grading_status = "ungraded") %>% 
      mutate(feedback_codes = NA) %>% 
      mutate(graded_qs = NA) %>% 
      mutate(last_time_graded = NA)
    
    if ("issue_titles" %in% colnames(changed_grade_sheet)) {
      issues_cols_to_delete <- c("issue_titles", "issue_bodies", "issue_pushed")
      changed_grade_sheet <- changed_grade_sheet %>% 
        mutate(issue_titles = NA, issue_bodies = NA, issue_pushed = NA)
    }
    
  } else {
    for (i in 1:length(student_ids)) {
      if (!is.na(changed_grade_sheet$comments[i])) {
        # Extract comments
        comments <- unlist(str_split(
          changed_grade_sheet$comments[i], 
          pattern = " // "
        ))
        
        comment_qs <- unlist(str_split(
          changed_grade_sheet$comment_qs[i], 
          pattern = " // "
        ))
        
        not_q_comment_position <- which(!(comment_qs %in% questions_to_delete))
        
        if (length(not_q_comment_position) == 0) {
          changed_grade_sheet$comments[i] <- NA
          
          changed_grade_sheet$comment_qs[i] <- NA
          
        } else {
        changed_grade_sheet$comments[i] <- str_c(
          comments[not_q_comment_position], 
          collapse = " // "
        )
        
        changed_grade_sheet$comment_qs[i] <- str_c(
          comment_qs[not_q_comment_position], 
          collapse = " // "
        )
        }
        
      }
      
      # if (github_issues) {
      #   if (!is.na(changed_grade_sheet$issue_titles[i])) {
      #     # Extract github issues
      #     comments <- unlist(str_split(
      #       changed_grade_sheet$comments[i], 
      #       pattern = " // "
      #     ))
      #     
      #     comment_qs <- unlist(str_split(
      #       changed_grade_sheet$comment_qs[i], 
      #       pattern = " // "
      #     ))          
      #   }
      #   
      # }
      
      # Extract saved grading progress
      fdbk_codes <- unlist(str_split(
        changed_grade_sheet$feedback_codes[i], 
        pattern = "&&&"
      ))
      
      qs_graded <- unlist(str_split(
        changed_grade_sheet$graded_qs[i], 
        pattern = "&&&"
      ))
      
      q_position <- which(qs_graded %in% questions_to_delete)
      not_q_position <- which(!(qs_graded %in% questions_to_delete))
      
      if (all(qs_graded %in% questions_to_delete)) {
        changed_grade_sheet$grading_status[i] <- "ungraded"
        changed_grade_sheet$feedback_codes[i] <- NA
        changed_grade_sheet$graded_qs[i] <- NA
        changed_grade_sheet$last_time_graded[i] <- NA 
        
      } else {
        changed_grade_sheet$feedback_codes[i] <- str_c(
          fdbk_codes[not_q_position], 
          collapse = "&&&"
        )
          
        changed_grade_sheet$graded_qs[i] <- str_c(
          qs_graded[not_q_position], 
          collapse = "&&&"
        )
        
        changed_grade_sheet$grading_status[i] <- "feedback created"
        changed_grade_sheet$last_time_graded[i] <- Sys.time()
        
        rubric_list <- import_rubric(rubric_path)
        rubric_prompts <- create_rubric_prompts(rubric_list, github_issues)
        
        grade_info <- assign_grade_write_feedback(
          temp_grade_sheet_row = changed_grade_sheet[i, ], 
          rubric_list = rubric_list,
          rubric_prompts = rubric_prompts
        )
        
        changed_grade_sheet$grading_status[i] <- grade_info$grading_status
        
      }
      
    }
  
  }
  
  # Put temporary grade sheet back together
  temporary_grade_sheet <- bind_rows(
    unchanged_grade_sheet, 
    changed_grade_sheet
  ) %>% 
    arrange(student_identifier, og_student_identifiers)
  
  # Save temporary grade sheet
  write_csv(temporary_grade_sheet, file = temp_grade_sheet_path)
  
}
