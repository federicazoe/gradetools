#' Render feedback files for graded students
#'
#' @param grading_progress_log The grading progress log used internally by gradetools
#' 
#' @importFrom fs path_ext_set
#' @importFrom quarto quarto_render
#' 
#' @keywords internal
#'

render_feedback <- function(grading_progress_log){
  
  feedback_file_ext <- as.character(fs::path_ext(
    grading_progress_log$feedback_path_to_be_rendered[1]
  ))
  
  ind_feedback_to_render <- grading_progress_log$grading_status != "ungraded"
  
  if (any(ind_feedback_to_render)) {
    
    # Let the user know that feedback is being knitted
    cat(paste0(
      "\nTrying to render feedback files to ", 
      feedback_file_ext, " format...\n"
    ))
    
    # Try to render feedback files
    tryCatch(               
      # Specifying expression
      expr = {              
        paths_returned <- mapply(
          quarto::quarto_render,
          grading_progress_log$feedback_path_qmd[ind_feedback_to_render],
          MoreArgs = list(quiet = TRUE)
        )
        
        cat(paste("\n...Succeeded!\n\n"))
        
        unlink(grading_progress_log$feedback_path_qmd)
        
        if (feedback_file_ext == "md") {
          unlink(fs::path_ext_set(
            path = grading_progress_log$feedback_path_qmd,
            ext = "html"
          ))
          
        }
      },
      
      # Specifying error message
      error = function(e){         
        cat(paste(
          "There was an error when trying to render the feedback file in the specified format.",
          "\nCompiling pdf's in R requires additional software.",
          "Feedback is rendered from a Quarto document (.qmd).",
          "We suggest you rerun the assist grading function with a different feedback_file_format.",
          "All of your progressed will be saved in the grading progress log.",
          sep = "\n"
        ))
      }
    )
    
  } 
  
}