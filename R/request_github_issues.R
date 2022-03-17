#' Allows the user to input their requested GitHub issues titles and bodies
#'
#' @param curr_row a row extracted from the temp_grade_sheet data table within assist_grading().
#' @param last_chance logical, whether it is the last chance to take note of an issue for the current student.
request_github_issues <- function(curr_row, last_chance = FALSE) {

  continue_creating_issues <- TRUE
  
  while (continue_creating_issues == TRUE){
    
    if (last_chance == FALSE) {
      create_issue <- readline(
        prompt = cat(
          paste(
            "Issues.",
            "Press 000 to take note of a new issue or [enter] to proceed to the next question.",
            sep = "\n"
          )
        )
      )
      
    } else {
      create_issue <- readline(
        prompt = cat(
          paste(
            "Issues.",
            "Press 000 to take note of a new issue or [enter] to proceed to the next student.",
            sep = "\n"
          )
        )
      )        
      
    }
    
    if (create_issue == "000") {
      issue_title <- dlg_input(
        paste(
          "Type title of the issue and press [ok].",
          "To undo taking note of an issue,  press [cancel].",
          sep = "\n"
        )
      )$res
      
      if (length(issue_title) == 0) {
        continue_creating_issues <- FALSE
        
      }  else {
        issue_body <- dlg_input(
          paste(
            "Type body of the issue and press [enter].",
            "To undo taking note an issue,  press [cancel].",
            sep = "\n"
          )
        )$res
        
        if (length(issue_body) == 0) {
          continue_creating_issues <- FALSE
          break()
          
        } else {
          # If this is the first issue recorded for this student/team
          if (is.na(curr_row$issue_titles)) {
            curr_row$issue_titles <- issue_title
            curr_row$issue_bodies <- issue_body
            curr_row$issue_pushed <- "FALSE"
            
          } else {
            curr_row$issue_titles <- paste0(
              curr_row$issue_titles, "&&&", issue_title
            )
            curr_row$issue_bodies <- paste0(
              curr_row$issue_bodies, "&&&", issue_body
            )
            curr_row$issue_pushed <- paste0(
              curr_row$issue_pushed, "&&&", "FALSE"
            )
            
          }
          
        }
        
      }
      
    } else {
      continue_creating_issues <- FALSE
      
    }
    
  }
  
  return(curr_row)
}
