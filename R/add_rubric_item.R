#' Prompts grader to add a rubric item for the current question
#' 
#' @param curr_q string, the name of the question that was being graded and for which the grader has asked to add a rubric item. Needs to be one of the names in the column
#' @param rubric_path string, path to assignment rubric. This rubric should be created using the function create_rubric_template, then filled in by the user. The rubric file name and column names must not be changed.
#' @param rubric_list list whose format corresponds to rubric_list, which is used by most functions in this package. This is produced by import_rubric
#' 
#' @import readr
#' @import dplyr
#' @import stringr
#' @import rstudioapi
#' @import svDialogs
#'
#' @return rubric list
#' 
#' @keywords internal
#' 
add_rubric_item <- function(curr_q, rubric_path, rubric_list) {
  
  rubric <- readr::read_csv(
    rubric_path, 
    show_col_types = FALSE, 
    col_types = readr::cols(
      name = readr::col_character(),
      prompt_code = readr::col_character(),
      prompt_message = readr::col_character(),
      feedback = readr::col_character(),
      .default = readr::col_double()
    ), 
    skip_empty_rows = TRUE
  )
  
  negative_grading <- "points_to_remove" %in% colnames(rubric)
  
  if (curr_q %in% rubric$name) {
    
    # Define where new entry should be added
    rows_curr_q <- which(rubric$name == curr_q)
    num_rows_curr_q <- length(rows_curr_q)
    add_row_after <- rows_curr_q[num_rows_curr_q]
    
    # Extract current prompt codes
    existing_prompt_codes <- rubric$prompt_code[rows_curr_q]

    # Define existing prompt code message
    existing_prompt_codes_message <- paste(
      "You are about to choose a prompt code.",
      "Prompt codes", 
      stringr::str_c(existing_prompt_codes, collapse = ", "),
      "already exist.",
      "Valid prompt codes must begin with a digit and cannot include spaces."
    )
    
  } else { # This would only occur if this is the first time that
           # they pre-specify  general feedback
 
    # Define where new entry should be added
    add_row_after <- nrow(rubric)
    
    existing_prompt_codes <- NA
    
    # Define prompt code message
    existing_prompt_codes_message <- paste(
      "You are about to choose a prompt code.",
      "No prompt codes have been specified so far.",
      "Valid prompt codes must begin with a digit and cannot include spaces."
    )    
  }

  # Obtain prompt_code from user
  # (verify it differs from current prompt codes)
  prompt_code_assigned <- FALSE
  
  svDialogs::dlg_message(existing_prompt_codes_message, type = "ok")
  
  while (prompt_code_assigned == FALSE) {
    prompt_code <- svDialogs::dlg_input(
      paste(
        "Type the new PROMPT CODE and press [ok].",
        "To undo adding a rubric item,  press [cancel].",
        sep = "\n"
      )
    )$res
      
    if (length(prompt_code) == 0) {
      return(rubric_list)
    }
    
    if (!(substr(prompt_code, 1, 1) %in% as.character(0:9))) {
      prompt_code_must_start_with_a_digit <- paste(
        "The prompt codes must begin with a digit.",
        "Please enter a new prompt code."
      )
      
      svDialogs::dlg_message(prompt_code_must_start_with_a_digit, type = "ok")
      
    } else if (prompt_code %in% as.character(existing_prompt_codes)) { 
      # Check for validity of prompt code
      type_different_prompt_code <- paste(
        "Prompt codes", 
        stringr::str_c(existing_prompt_codes, collapse = ", "),
        "already exist and cannot be reset.",
        "To reset an existing prompt code, please interrupt grading and modify your rubric."
      )
      
      svDialogs::dlg_message(type_different_prompt_code, type = "ok")
      
    } else if (stringr::str_detect(prompt_code, " ")) {
      no_spaces_in_prompt_code <- paste(
        "Prompt codes cannot include spaces.",
        "Please enter a new prompt code."
      )
      
      svDialogs::dlg_message(no_spaces_in_prompt_code, type = "ok")      
      
    } else {
      prompt_code_assigned <- TRUE
    }
  }
  
  # Obtain prompt_message from user
  prompt_message <- svDialogs::dlg_input(
    paste(
      "Type the new PROMPT MESSAGE and press [ok].",
      "To undo adding a rubric item,  press [cancel].",
      sep = "\n"
    )
  )$res
  
  if (length(prompt_message) == 0) {
    return(rubric_list)
  }
  
  # Obtain feedback from user
  feedback <- svDialogs::dlg_input(
    paste(
      "Type the new FEEDBACK and press [ok].",
      "To undo adding a rubric item,  press [cancel].",
      sep = "\n"
    )
  )$res
  
  if (length(feedback) == 0) {
    return(rubric_list)
  } 
  
  if (curr_q != "general_feedback") {
    
    # Obtain points to remove or to add
    if (negative_grading) {
      message_points <- "You are about to enter the number of points that should be removed from this question's total grade when the new rubric item is selected."
      points_type <- "POINTS TO REMOVE"
    } else {
      message_points <- "You are about to enter the number of points that should be added to a student's grade for this question when the new rubric item is selected."
      points_type <- "POINTS TO ADD"
    }
    
    total_points <- rubric_list[[curr_q]][["total_points"]]
    points_assigned <- FALSE
    message_points <-  paste(
      message_points,
      paste0("The total points for this question are: ", total_points)
    )
    
    svDialogs::dlg_message(message_points, type = "ok")
    
    while (points_assigned == FALSE) {
      
      points_for_item <- svDialogs::dlg_input(
        paste(
          paste("Type", points_type ,"and press [okay]."),
          "To undo adding a rubric item,  press [cancel].",
          sep = "\n"
        )
      )$res
      
      if (length(points_for_item) == 0) {
        return(rubric_list)
      } 
      
      if (anyNA(suppressWarnings(as.numeric(points_for_item)))) {
        need_numeric_points <- paste(
          "Points need to be a number, e.g. 1 or 10.",
          "Please re-enter the points associated to the new rubric item.",
          paste0("Recall that the total points for this question are: ", total_points)
        )
        
        svDialogs::dlg_message(need_numeric_points, type = "ok")      
        
      } else {
        points_assigned <- TRUE
      }
      
      
    }
    
    new_item_summary <- paste(
      "Summary of your new rubric item: \n",
      paste0("Prompt code: ", prompt_code),
      paste0("Prompt message: ", prompt_message),
      paste0("Feedback: ", feedback),
      paste0(
        "Points ", 
        ifelse(negative_grading, "to remove: ", "to add: "),
        points_for_item
      ),
      sep = "\n"
    )
    
  } else {
    
    total_points <- NA
    points_for_item <- NA
    
    new_item_summary <- paste(
      "Summary of your new general feedback: \n",
      paste0("Prompt code: ", prompt_code),
      paste0("Prompt message: ", prompt_message),
      paste0("Feedback: ", feedback),
      sep = "\n"
    )
    
  }

  proceed_message <-  paste(
    new_item_summary,
    "\nTo add the rubric item, press [ok]",
    "To interrupt adding a rubric item, press [cancel].",
    sep = "\n"
  )
  
  proceed <- svDialogs::ok_cancel_box(proceed_message)
  
  if (proceed == TRUE) {
    if (negative_grading == TRUE) {
      rubric <- rubric %>% 
        add_row(
          name = curr_q,
          prompt_code = prompt_code,
          prompt_message = prompt_message,
          feedback = feedback,
          total_points = as.numeric(total_points),
          points_to_remove = as.numeric(points_for_item),
          .after = add_row_after
        )
      
    } else {
      rubric <- rubric %>% 
        add_row(
          name = curr_q,
          prompt_code = prompt_code,
          prompt_message = prompt_message,
          feedback = feedback,
          total_points = as.numeric(total_points),
          points_to_add = as.numeric(points_for_item),
          .after = add_row_after
        )     
    }
    
    readr::write_csv(rubric, rubric_path)

    rubric_list <- import_rubric(rubric_path = rubric_path)
    
  }
  
  rubric_list
  
}
