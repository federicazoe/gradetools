#' Leads grader through grading a student
#'
#' @param row integer; This number indicates which row of the temp_grade_sheet is to be used / graded
#' @param temp_grade_sheet data frame; The temp_grade_sheet is a data frame containing information for gradetools internal use 
#' @param temp_grade_sheet_path string; path to temp_grade_sheet
#' @param rubric_prompts list of prompts; One prompt for each question plus one for overall feedback. This is produced by create_rubric_prompts
#' @param rubric_list list whose format corresponds to rubric_list, which is used by most functions in this package. This is produced by import_rubric
#' @param rubric_path string, path to assignment rubric. This rubric should be created using the function create_rubric_template, then filled in by the user. The rubric file name and column names must not be changed.
#' @param github_issues Boolean; indicates if grader wants to be able to create GitHub issues
#' @param issue_every_question logical, whether the possibility to create issues should be given at every question or only at the end of the assignment
#' @param questions_to_grade vector of strings; names of assignment questions to grade, or "all" to specify all questions should be graded. All questions_to_grade must exactly match ones present in the rubric
#' 
#' @import readr
#' @import dplyr
#' @import stringr
#' @import rstudioapi
#' @import svDialogs
#'
#' @return temporary grade sheet data frame
#' 
grade_student <- function(
    row, 
    temp_grade_sheet, 
    temp_grade_sheet_path, 
    rubric_prompts,
    rubric_list,
    rubric_path,
    github_issues,
    issue_every_question,
    questions_to_grade
  ){
  
  curr_row <- temp_grade_sheet[row, ]
    
  invalid_fbk_message <- "Please enter a valid prompt code, or enter 222 to stop."
  
  # Start at the first ungraded question and go through all rubric prompts
  q <- 1
  
  store_feedback <- FALSE
  
  qs_to_grade_wo_gf <- questions_to_grade[questions_to_grade != "general_feedback"]
  grade_gf <- "general_feedback" %in% questions_to_grade
  previously_graded_qs <- unlist(str_split(curr_row$graded_qs, pattern = "&&&"))

  while(q <= length(rubric_prompts)) {
    curr_q <- names(rubric_prompts)[q]
    
    if (curr_q %in% previously_graded_qs) {
      # Don't grade previously graded questions
      
    } else if (grade_gf && curr_q == "general_feedback") {
      single_use_question_fbk <- character(0)
      ask_gf_again <- FALSE
      question_fbk <- readline(prompt = cat(rubric_prompts[[q]]))
      
      if (question_fbk == "") {
        question_fbk <- "NA"
        store_feedback <- TRUE

      } else if (question_fbk == "r" | question_fbk == "'r'") { # let user add a new prespecified generalized feedback
        rubric_list <- add_rubric_item(
          curr_q = curr_q, 
          rubric_path = rubric_path, 
          rubric_list = rubric_list
        )
        
        rubric_prompts <- create_rubric_prompts(rubric_list)
        
        q <- q - 1
        ask_gf_again <- TRUE
        
      } else {
        q_fbk_separated <- str_replace_all(
          unlist(str_split(question_fbk, "--")),
          c(" " = "")
        )
        
        valid_fbk_codes <- c("p", "'p'")
        
        if ("general_feedback" %in% names(rubric_list)) {
          valid_fbk_codes <- c(
            valid_fbk_codes,
            rubric_list[["general_feedback"]]$prompt_code
          )
        }
      
        if (!all(q_fbk_separated %in% valid_fbk_codes)) {
          dlg_message(invalid_fbk_message, type = "ok")
          q <- q - 1
          ask_gf_again <- TRUE
          
        } else {
          if (any(c("p", "'p'") %in% q_fbk_separated)) { # let user enter a single-use feedback
            single_use_question_fbk <-  dlg_input(
              paste(
                "Type single-use general feedback and press [enter].",
                "To undo writing a general feedback, press [cancel].",
                sep = "\n"
              )
            )$res
            
            where_p <- which(q_fbk_separated %in% c("p", "'p'"))
            q_fbk_separated <- q_fbk_separated[-where_p]
            
            # If the grader typed "p" multiple times, leave just one "p"
            if (length(single_use_question_fbk) != 0) {
              q_fbk_separated <- append(
                q_fbk_separated,
                single_use_question_fbk,
                where_p[1] - 1
              )
              
            } else {
              ask_gf_again <- TRUE
              
              q <- q - 1
              
            }
            
          } 
          
          if (ask_gf_again == FALSE) {
            question_fbk <- str_c(q_fbk_separated, collapse = "---")
            store_feedback <- TRUE
          }
        }
      }
      
      if (ask_gf_again == FALSE) {
        if (github_issues == TRUE & issue_every_question == FALSE) {
          curr_row <- request_github_issues(curr_row, last_chance = TRUE)
        }
      }
      
    } else if (curr_q %in% qs_to_grade_wo_gf) {
      question_fbk <- readline(prompt = cat(rubric_prompts[[q]]))
      
      question_fbk <- str_replace_all(
        question_fbk,
        c(" " = "", "--" = "---")
      )
      
      if (question_fbk == "222") { # Stop grading
        return()
      
      } else if (question_fbk == "r" | question_fbk == "'r'") { # let user add a rubric item for this question
        rubric_list <- add_rubric_item(
          curr_q = curr_q, 
          rubric_path = rubric_path, 
          rubric_list = rubric_list
        )
        
        rubric_prompts <- create_rubric_prompts(rubric_list)
        
        q <- q - 1

      } else {
        q_fbk_separated <- unlist(str_split(question_fbk, pattern = "---"))
            
        valid_fbk_codes <- c(
          rubric_list[[q]]$prompt_code, 
          rubric_list$all_questions$prompt_code
        )
        
        if (!all(q_fbk_separated %in% valid_fbk_codes)) {
          dlg_message(invalid_fbk_message, type = "ok")
          
          q <- q - 1
          
        } else {
          store_feedback <- TRUE
          
          if (github_issues == TRUE & issue_every_question == TRUE) {
            curr_row <- request_github_issues(curr_row)
          }
          
        }
        
      } 
      
    }
    
    q <- q + 1
    
    if (store_feedback) {
      if (is.na(curr_row$feedback_codes)) {
        curr_row$feedback_codes <- question_fbk
        curr_row$graded_qs <- curr_q
        
      } else {
        curr_row$feedback_codes <- paste0(
          curr_row$feedback_codes, "&&&", question_fbk
        )
        
        curr_row$graded_qs <- paste0(
          curr_row$graded_qs, "&&&", curr_q
        )
        
      }
      
      grade_info <- assign_grade_write_feedback(
        curr_row, 
        rubric_list = rubric_list,
        rubric_prompts = rubric_prompts
      )
      
      # Update grading_status
      curr_row$grading_status <- grade_info$grading_status
      
      curr_row$last_time_graded <- Sys.time()
      
      temp_grade_sheet[row, ] <- curr_row
      
      write_csv(temp_grade_sheet, file = temp_grade_sheet_path)
      
      # If this student has been fully graded
      if (curr_row$grading_status == "all questions graded") { 
        cat(paste("Total grade assigned:", grade_info$grade, "\n\n"))
      }
      
      store_feedback <- FALSE
      
    }
    
  } 
  
  # Save temporary grade sheet update
  temp_grade_sheet[row, ] <- curr_row
  write_csv(temp_grade_sheet, file = temp_grade_sheet_path)
    
  temp_grade_sheet
}
