#' Leads grader through grading a student
#'
#' @param row integer; This number indicates which row of the grading_progress_log is to be used / graded
#' @param grading_progress_log data frame; The grading_progress_log is a data frame containing information for gradetools internal use 
#' @param grading_progress_log_path string; path to grading_progress_log
#' @param rubric_prompts list of prompts; One prompt for each question plus one for overall feedback. This is produced by create_rubric_prompts
#' @param rubric_list list whose format corresponds to rubric_list, which is used by most functions in this package. This is produced by import_rubric
#' @param rubric_path string, path to assignment rubric. This rubric should be created using the function create_rubric_template, then filled in by the user. The rubric file name and column names must not be changed.
#' @param github_issues Boolean; indicates if grader wants to be able to create GitHub issues
#' @param questions_to_grade vector of strings; names of assignment questions to grade, or "all" to specify all questions should be graded. All questions_to_grade must exactly match ones present in the rubric
#' 
#' @import readr
#' @import dplyr
#' @import stringr
#' @import rstudioapi
#' @import svDialogs
#'
#' @return grading progress log data frame
#' 
#' @keywords internal
#' 
grade_student <- function(
  row, 
  grading_progress_log, 
  grading_progress_log_path, 
  rubric_prompts,
  rubric_list,
  rubric_path,
  github_issues,
  questions_to_grade
){
  
  curr_row <- grading_progress_log[row, ]
  
  invalid_fbk_message <- 'Please enter a valid prompt code, or enter "s" to stop.'
  
  qs_to_grade_wo_gf <- questions_to_grade[questions_to_grade != "general_feedback"]
  previously_graded_qs <- unlist(stringr::str_split(curr_row$graded_qs, pattern = "&&&"))
  
  # Start at the first ungraded question and go through all rubric prompts
  q <- 1
  
  while(q <= length(rubric_prompts)) {
    curr_q <- names(rubric_prompts)[q]
    
    grade_this_question <- FALSE
    store_feedback_codes <- FALSE
    display_prompt_again <- FALSE
    
    if (curr_q %in% questions_to_grade && !(curr_q %in% previously_graded_qs)) {
        grade_this_question <- TRUE
    }
    
    if (grade_this_question) {
      question_fbk <- readline(prompt = cat(rubric_prompts[[q]]))
      
      if (question_fbk == "s") { # Stop grading
        return()
        
      } else if (question_fbk == "i") {
        curr_row <- note_github_issues(curr_row, curr_q = curr_q)
        display_prompt_again <- TRUE
        
      } else if (question_fbk == "p") { 
        question_comment <- svDialogs::dlg_input(
          paste(
            "Type personalized feedback and press [ok].",
            "To cancel typing personalized feedback, press [cancel].",
            sep = "\n"
          )
        )$res
        
        if (length(question_comment) > 0 ) {
          if (is.na(curr_row$comments)) {
            curr_row$comments <- question_comment
            curr_row$comment_qs <- curr_q
            
          } else {
            curr_row$comments <- paste0(curr_row$comments, "&&&", question_comment)
            curr_row$comment_qs <- paste0(curr_row$comment_qs,"&&&", curr_q)
            
          }
          
          curr_row$feedback_info_updated
          
        } 
        
        display_prompt_again <- TRUE
        
        if (curr_q == "general_feedback") {
          move_to_next_q <- svDialogs::ok_cancel_box(paste(
            "Press [ok] to move on to grade the next submission.",
            "Press [cancel] to continue providing general feedback.",
            sep = "\n"
          ))
          
          if (move_to_next_q) {
            display_prompt_again <- FALSE
          }
           
        }
        
      } else if (question_fbk == "r") { # let user add a rubric item for this question
        rubric_list <- add_rubric_item(
          curr_q = curr_q, 
          rubric_path = rubric_path, 
          rubric_list = rubric_list
        )
        
        rubric_prompts <- create_rubric_prompts(
          rubric_list, 
          github_issues = github_issues
        )
        
        display_prompt_again <- TRUE 
        
      } else if (question_fbk == "d" && curr_q == "general_feedback") {
        question_fbk <- "NA"
        store_feedback_codes <- TRUE
        curr_row$updated_info <- TRUE
        
      } else { 
        question_fbk <- stringr::str_replace_all(question_fbk, c(" " = "", "--" = "---"))
        q_fbk_separated <- unlist(stringr::str_split(question_fbk, "---"))
        
        if (curr_q != "general_feedback") {
          valid_fbk_codes <- c(
            rubric_list[[q]]$prompt_code, 
            rubric_list$all_questions$prompt_code
          )
          
        } else if ("general_feedback" %in% names(rubric_list)) {
          valid_fbk_codes <- c(rubric_list$general_feedback$prompt_code)
          
        } else {
          valid_fbk_codes <- NA
          
        }
        
        if (all(q_fbk_separated %in% valid_fbk_codes)) {
          store_feedback_codes <- TRUE
          
        } else {
          svDialogs::dlg_message(invalid_fbk_message, type = "ok")
          display_prompt_again <- TRUE
          
        }
        
      }
        
    } # End sorting through input
    
    if (store_feedback_codes) {
      if (is.na(curr_row$feedback_codes)) {
        curr_row$feedback_codes <- question_fbk
        curr_row$graded_qs <- curr_q
        
      } else {
        curr_row$feedback_codes <- paste0(curr_row$feedback_codes, "&&&", question_fbk)
        curr_row$graded_qs <- paste0(curr_row$graded_qs, "&&&", curr_q)
        
      }
      
      # Update grading_status
      curr_row$feedback_info_updated <- TRUE
      
      grade_info <- assign_grade_write_feedback(
        grading_progress_log_row = curr_row, 
        rubric_list = rubric_list,
        rubric_prompts = rubric_prompts
      )
      
      # Update feedback_pushed, if it exists
      if ("feedback_pushed" %in% colnames(curr_row)) {
        curr_row$feedback_pushed <- "FALSE"
      }
      
      curr_row$grading_status <- grade_info$grading_status
      curr_row$last_time_graded <- Sys.time()
      
      # If this student has been fully graded
      if (curr_row$grading_status == "all questions graded") { 
        cat(paste("Total grade assigned:", grade_info$grade, "\n\n"))
      }
      
    } # End storing question feedback
    
    grading_progress_log[row, ] <- curr_row
    readr::write_csv(grading_progress_log, file = grading_progress_log_path)
    
    # Move onto next question if valid feedback codes were provided
    if (!display_prompt_again) {
      q <- q + 1
    }
    
  } # End while loop
  
  grading_progress_log
  
}
