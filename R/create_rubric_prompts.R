#' Creates assignment rubric prompts
#'
#' @param rubric_list list of lists. Each sub-list corresponds to a part of the assignment rubric and specifies the name of this part (e.g. "Question 1"), the total points that can be earned for this part, the rubric etc.
#'
#' @return list of prompts. One prompt for each question plus one for overall feedback.
#' @importFrom stringr str_count
#' @importFrom stringr str_c
create_rubric_prompts <- function(rubric_list) {
  rubric_names <- names(rubric_list)
  
  negative_grading <- "points_to_remove" %in% names(rubric_list[[1]])
  
  prompt_names <- rubric_names[rubric_names != c("all_questions")]
  
  if (!("all_questions" %in% rubric_names)) {
    all_questions_prompts <- NULL
    
  } else {
    if (negative_grading) {
      all_questions_prompts <- paste0(
        rubric_list$all_questions$prompt_code, ": ",
        rubric_list$all_questions$prompt_message, " [-",
        abs(rubric_list$all_questions$points_to_remove), "%]"
      )
    } else {
      all_questions_prompts <- paste0(
        rubric_list$all_questions$prompt_code, ": ",
        rubric_list$all_questions$prompt_message, " [+",
        abs(rubric_list$all_questions$points_to_add), "%]"
      )
    }
    
  }
  
  # Prepare prompts for each question
  rubric_prompts <- vector(mode = "list", length = length(prompt_names))
  names(rubric_prompts) <- prompt_names

  for (q in prompt_names[prompt_names != "general_feedback"]) {
    
    q_info <- rubric_list[[q]]
    q_name <- q_info$name
    
    if (negative_grading) {
      q_prompts_code_and_messages <- paste0(
        q_info$prompt_code, ": ",
        q_info$prompt_message, " [-", 
        abs(q_info$points_to_remove), "]"
      ) 
      
    } else {
      q_prompts_code_and_messages <- paste0(
        q_info$prompt_code, ": ",
        q_info$prompt_message, " [+", 
        abs(q_info$points_to_add), "]"
      )
      
    }
    
    rubric_prompts[q] <- paste(
      q_name,
      "Enter one or more of the following options, use -- to separate multiple items",
      stringr::str_c(q_prompts_code_and_messages, collapse = "\n"),
      stringr::str_c(all_questions_prompts, collapse = "\n"),
      "\nOr enter one of the following",
      "r: Create a new rubric item",
      "c: Add a comment to the feedback file",
      "222: Stop grading",
      sep = "\n"
    )
    
  }
  
  if ("general_feedback" %in% prompt_names) {
    general_feedback_prompts_code_and_messages <- paste(
      rubric_list$general_feedback$prompt_code,
      rubric_list$general_feedback$prompt_message,
      sep = ": "
    )
    
    general_feedback <- paste(
      "General feedback",
      "Enter one or more of the following options, use -- to separate multiple items",
      stringr::str_c(general_feedback_prompts_code_and_messages, collapse = "\n"),
      "p: Provide a single-use general feedback",
      "\nOr enter one of the following",
      "r: Create a new pre-specified general feedback",
      "[enter]: Decline to provide general feedback",
      sep = "\n"
    )
    
  } else {
    general_feedback <- paste(
      "General feedback.",
      "Enter one of the following options",
      "p: Provide a single-use general feedback",
      "r: Create a new pre-specified general feedback",
      "[enter]: Decline to provide general feedback",
      sep = "\n"
    )
    
  }
  
  rubric_prompts["general_feedback"] <- general_feedback

  rubric_prompts
  
}
