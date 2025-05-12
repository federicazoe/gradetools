#' Writes feedback, and returns grade, grade decomposition, and grading status
#'
#' @param grading_progress_log_row data frame with 1 row; The grading_progress_log is a data frame containing information for gradetools internal use. This row should correspond to the student that needs to be graded and have feedback written
#' @param rubric_list list of lists; each sub-list corresponds to a part of the assignment rubric and specifies the name of this part (e.g. "question_1"), the total points that can be earned for this part, the rubric etc. Rubric should be made using 'create_rubric_template()'.
#' @param rubric_prompts list of prompts; One prompt for each question plus one for overall feedback. This is produced by create_rubric_prompts
#'
#' @return list with\describe{
#'   \item{grade}{A number, the total grade scored for this assignment by the student being graded (e.g. 3)}
#'   \item{grade_decomposition}{A vector that shows how each part of the assignment contributed to the overall grade (e.g. c(1, 0, 2))}
#'   \item{grading_status}{A string indicating the grading_status for this student}
#' }
#' @import stringr
#' @importFrom fs file_create
#' @importFrom readr write_file
#' 
#' @keywords internal
#'
assign_grade_write_feedback <- function(
    grading_progress_log_row,
    rubric_list,
    rubric_prompts
  ) {
  
  # Get feedback and grade corresponding to entered code
  feedback_code <- unlist(stringr::str_split(
    grading_progress_log_row$feedback_codes, 
    pattern = "&&&"
  ))
  
  questions_graded <- unlist(stringr::str_split(
    grading_progress_log_row$graded_qs, 
    pattern = "&&&"
  ))
  
  # To make sure questions are in order
  question_matches <- order(match(questions_graded, names(rubric_prompts)))
  
  num_questions <- sum(
    stringr::str_count(names(rubric_list), "question_")
  )
  
  if (!is.null(rubric_list[[1]]$points_to_remove)) {
    grading_scheme <- "negative"
  } else if (!is.null(rubric_list[[1]]$points_to_add)) {
    grading_scheme <- "positive"
  } else {
    stop(
      "The 6th rubric column is either incorrectly named or missing. It can only be 'points_to_remove' or 'points_to_add'"
    )
  }
  
  feedback <- NULL
  overall_grade <- 0
  grade_decomposition <- NULL
  
  # Assign feedback and grade for each question
  for (q in questions_graded[questions_graded != "general_feedback"]) {
    # Extract info for this question
    q_info <- rubric_list[[q]]
    
    # Extract prompt codes for this question
    q_prompt_code <- q_info$prompt_code
    
    # Extract feedback messages for this question
    q_feedback <- q_info$feedback
    
    if (grading_scheme == "negative") {
      # Extract points to change (remove) for this question
      q_points_to_change <- -abs(as.numeric(q_info$points_to_remove))
      
      # Grade starts as the total number of points that this question is worth
      # Then points are removed according to the feedback given
      q_grade <- as.numeric(q_info$total_points)
      
    } else {
      # Extract points to change (add) for this question
      q_points_to_change <- abs(as.numeric(q_info$points_to_add))
      
      # Grade starts as the total number of points that this question is worth
      # Then points are removed according to the feedback given
      q_grade <- 0
      
    }
    
    # Extract all codes given to this question
    # E.g. "2---77" into "2" and "77"
    q_feedback_code <- stringr::str_split(
      feedback_code[questions_graded == q], 
      pattern = "---"
    )[[1]]
    
    # Start feedback message for this question
    q_fbk <- paste("\n##", q)
    
    # Write question comment if present
    if (!is.na(grading_progress_log_row$comments)) {
      comments <- unlist(stringr::str_split(
        grading_progress_log_row$comments, 
        pattern = "&&&"
      ))
      
      comment_qs <- unlist(stringr::str_split(
        grading_progress_log_row$comment_qs, 
        pattern = "&&&"
      ))
      
      if (q %in% comment_qs) {
        q_fbk <- paste0(
          q_fbk, "\n", 
          stringr::str_c(comments[which(comment_qs == q)], collapse = "\n"), "\n"
        )
      }
      
    }
    
    for (fbk_code in q_feedback_code){
      if (fbk_code %in% q_prompt_code) {
        prompt_number <- which(q_prompt_code == fbk_code)
        new_fbk <- q_feedback[prompt_number]
        new_change <- q_points_to_change[prompt_number]
        
      } else if (fbk_code %in% rubric_list$all_questions$prompt_code) {
        prompt_number <- which(rubric_list$all_questions$prompt_code == fbk_code)
        new_fbk <- rubric_list$all_questions$feedback[prompt_number]
        
        # In feedback common to all questions, points to change are
        # expressed as a percentage of the total points assigned to a question 
        new_change_percent <- as.numeric(
          rubric_list$all_questions$points_to_remove[prompt_number]
        )
        new_change <- (new_change_percent * q_info$total_points) / 100
        
        
      } else {
        error_message <- paste(
          "Error: feedback code assigned for ",
          q,
          "was not among prompt codes specified in rubric_list."
        )
        
        stop(error_message)
        
      }
      
      q_fbk <- paste(q_fbk, new_fbk, sep = "\n\n")
      
      q_grade <- q_grade + new_change

    }
    
    # Check the grade
    # (total score for the question can be at minimum 0 and max total_points)
    if (grading_scheme == "negative") {
      q_grade <- max(q_grade, 0)
      
    } else {
      q_grade <- min(q_grade, as.numeric(q_info$total_points))
      
    }
    
    # Store question feedback, grade, and grade decomposition
    feedback <- paste(feedback, q_fbk, sep = "\n\n")
    overall_grade <- overall_grade + q_grade
    grade_decomposition <- c(grade_decomposition, q_grade)
    
  }
  
  # Assign overall feedback if in rubric or provided
  gf_provided <- any(questions_graded == "general_feedback")
  
  if (gf_provided) {
    gf_comments <- NULL
    
    if (!is.na(grading_progress_log_row$comments)) {
      comments <- unlist(stringr::str_split(
        grading_progress_log_row$comments, 
        pattern = "&&&"
      ))
      
      comment_qs <- unlist(stringr::str_split(
        grading_progress_log_row$comment_qs, 
        pattern = "&&&"
      ))
      
      if ("general_feedback" %in% comment_qs) {
        gf_comments <- paste0(
          stringr::str_c(
            comments[which(comment_qs == "general_feedback")], 
            collapse = "\n"
          ), 
          "\n"
        )
        
      }
      
    }
    
    gf_separated <- stringr::str_split(
      feedback_code[questions_graded == "general_feedback"], 
      pattern = "---"
    )[[1]]
    
    if (!is.null(gf_comments)) {
      if (length(gf_separated) == 1 && gf_separated[1] == "NA")
        gf_separated <- gf_comments
      
      else {
        gf_separated <- c(gf_comments, gf_separated) 
      }
      
    }

    if (gf_separated[1] != "NA") {
      general_feedback <- "\n## General feedback:"
      
      for (gf in gf_separated) {
        if (gf %in% rubric_list$general_feedback$prompt_code) {
          prompt_number <- which(
            rubric_list$general_feedback$prompt_code == gf
          )
          
          general_feedback <- paste(
            general_feedback, 
            rubric_list$general_feedback$feedback[prompt_number],
            sep = "\n\n"
          )
          
        } else {
          
          general_feedback <- paste(
            general_feedback, 
            gf,
            sep = "\n\n"
          )
          
        }
    
      }
      
      feedback <- paste(feedback, general_feedback, sep = "\n\n")
  
    }
  }
  
  # Delete student's old feedback if it exists and write a new one
  # This is important because the rubric could have changed
  if(grading_progress_log_row$grading_status != "ungraded") {
    unlink(grading_progress_log_row$feedback_path_qmd)
    unlink(grading_progress_log_row$feedback_path_to_be_rendered)
    
  }
  
  fs::file_create(grading_progress_log_row$feedback_path_qmd)
  
  # Determining type of file to knit feedback to
  feedback_file_ext <- as.character(fs::path_ext(
    grading_progress_log_row$feedback_path_to_be_rendered)[1]
  )
  
  if(feedback_file_ext == "qmd") {
    feedback_file_ext <- "html"
  }
  
  if (feedback_file_ext == "Rmd") {
    grading_progress_log_row$feedback_path_qmd <- fs::path_ext_set(
      grading_progress_log_row$feedback_path_qmd, 
      "Rmd"
    )
    
    yaml <- paste(
      "---",
      "title: 'Feedback'",
      "output: html_document", 
      "---\n",
      sep = "\n"
    )
    
  } else {
    yaml <- paste(
      "---",
      "title: 'Feedback'",
      paste0("format: ", feedback_file_ext), 
      "---\n",
      sep = "\n"
    )
    
  }
  
  # Write feedback in feedback file
  readr::write_file(
    x = paste0(yaml, "\n", feedback , "\n\n"), 
    file = grading_progress_log_row$feedback_path_qmd, 
    append = TRUE
  )
  
  # Update grading_status
  if (length(questions_graded) == length(rubric_prompts)) {
    grading_status <- "all questions graded"
    
  } else if (grading_progress_log_row$grading_status == "ungraded") {
    grading_status <- "feedback created"
    
  } else {
    grading_status <- grading_progress_log_row$grading_status
  }
      
  list(
    grade = overall_grade,
    grade_decomposition = grade_decomposition,
    grading_status = grading_status
  )
  
}
