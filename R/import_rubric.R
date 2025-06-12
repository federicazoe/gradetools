#' Reads in assignment rubric to produce rubric_list. Alternative to defining rubric as rubric_list manually
#'
#' @param rubric_path string, path to assignment rubric. This rubric should be created using the function create_rubric_template, then filled in by the user. The rubric file name and column names must not be changed.
#'
#' @return list whose format corresponds to rubric_list, which is used by most functions in this package
#' 
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom stringr str_detect
#' 
#' @keywords internal
#' 
import_rubric <- function(rubric_path) {
  
  if (!file.exists(rubric_path)) {
    stop(paste0(
      "No file exists at ", rubric_path, ". ",
      "Are you sure that this path is correct?"
    ))  
    
  }
  
  col_names <- c(
    "name", "prompt_code", "prompt_message", 
    "feedback", "total_points"
  )
  
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
  
  invalid_number_of_cols <- ncol(rubric) != 6
  invalid_char_cols <- all(colnames(rubric)[1:5] == col_names)
  invalid_points_col <- colnames(rubric)[6] %in% c("points_to_remove", "points_to_add")
  
  
  # Need the columns to be present
  if (invalid_number_of_cols && invalid_char_cols && invalid_points_col) {
    stop(
      "The rubric's columns are incorrect. 
      Please use the create_rubric_template function to create a rubric. 
      The rubric's file name and column names cannot be changed."
    )
  }
  
  # Check that points_to_remove or add is valid
  gf_rows <- rubric$name == "general_feedback"
  points_to_change_rows <- pull(rubric, 6)[!gf_rows]
  
  if (anyNA(points_to_change_rows) || !is.numeric(points_to_change_rows)) {
    stop(paste0(
      "\nAll rubric items must have ", names(rubric)[6], " provided, except for general_feedback.",
      "\nThis column must be numeric."
    ))
  }
  
  # Check that total_points is valid for the first rubric item per question
  rubric_items_for_qs <- !(rubric$name %in% c("all_questions", "general_feedback"))
  total_pts_rows <- rubric$total_points[!duplicated(rubric$name) & rubric_items_for_qs]
  
  if (anyNA(total_pts_rows) || !is.numeric(total_pts_rows)) {
    stop(paste0(
      "\nThe first rubric item for each question needs to have total_points provided.",
      "\nThis column must be numeric."
    ))
  }
  
  negative_grading <- "points_to_remove" %in% colnames(rubric)
  
  question_names <- unique(rubric$name)
  
  expected_to_be_full_columns <- c(
    rubric$name,
    rubric$prompt_code, 
    rubric$prompt_message, 
    rubric$feedback
  )
  
  if (any(is.na(expected_to_be_full_columns))) {
    stop(paste0(
      "\nThe following columns of the rubric must have entries for every row: name, prompt_code, prompt_message, feedback."
    ))
  }
  
  # Check if any prompt codes have spaces
  if (any(stringr::str_detect(rubric$prompt_code, pattern = " "))) {
    stop(paste0(
      "\nPrompt codes in the rubric cannot contain spaces."
    ))
  }
  
  # Check if all_questions prompt_codes are unique in the rubric
  if ("all_questions" %in% question_names) {
    all_qs_prompt_codes <- rubric$prompt_code[rubric$name == "all_questions"]
    if (any(all_qs_prompt_codes %in% rubric$prompt_code[rubric_items_for_qs])) {
      stop(paste0(
        "\nNo prompt_code for 'all_questions' can be repeated in the rubric, except for in 'general_feedback'."
      ))
    }
  }
  
  # Check if other prompt codes are unique
  for(curr_question in question_names[!(question_names == "all_questions")]) {
    if (any(duplicated(rubric$prompt_code[rubric$name == curr_question]))) {
      stop(paste0(
        "\nThe rubric cannot have a prompt_code used multiple times for a question.",
        "\n", curr_question, " has a repeated prompt_code."
      ))
    }
  }
  
  # Check that the all prompt_code's begin with a number
  valid_start_ch <- as.character(0:9)
  if (!all(substr(rubric$prompt_code, 1, 1) %in% valid_start_ch)) {
    stop(("All rubric prompt_code's must begin with a digit."))
  }
  
  # All names not specified as all_questions or general_feedback, are considered to be for a specific assignment question
  rubric_list <- list()
  
  for (q in 1:length(question_names)) {
    q_list <- list()
    
    q_rubric <- rubric[rubric$name %in% question_names[q], ]
    
    q_list$name <- q_rubric$name[1]
    q_list$prompt_code <- stringr::str_replace_all(
      as.character(q_rubric$prompt_code),
      pattern = " ", 
      replacement = ""
    )
    
    if (any(duplicated(q_list$prompt_code))) {
      stop(paste(
        "\nAll prompt codes within a question need to be unique. 
        Fix the prompt codes for ", q_list$name
      ), sep = "\n")
      
    }
    
    q_list$prompt_message <- q_rubric$prompt_message
    q_list$feedback <- q_rubric$feedback
    q_list$total_points <- q_rubric$total_points[1]
    
    if (negative_grading) {
      q_list$points_to_remove <- -abs(q_rubric$points_to_remove)
      
    } else {
      q_list$points_to_add <- abs(q_rubric$points_to_add)
      
    }
    
    rubric_list <- append(rubric_list, list(q_list))
    
  }
  
  names(rubric_list) <- question_names
  
  rubric_list
  
}
