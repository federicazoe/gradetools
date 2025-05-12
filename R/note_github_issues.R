#' Collects GitHub issue title and body
#'
#' @param curr_row a row extracted from the grading_progress_log data table within assist_grading().
#' @param curr_q name of current question
#' 
#' @importFrom svDialogs dlg_input
#' 
#' @return corresponding row of grading progress log
#' 
#' @keywords internal
#' 
note_github_issues <- function(curr_row, curr_q) {

  issue_title <- svDialogs::dlg_input(
    paste(
      "Type title of the issue and press [ok].",
      "To cancel taking note of an issue,  press [cancel].",
      sep = "\n"
    )
  )$res
  
  if (length(issue_title) > 0) {
    issue_body <- svDialogs::dlg_input(
      paste(
        "Type body of the issue and press [enter].",
        "To undo taking note an issue,  press [cancel].",
        sep = "\n"
      )
    )$res
    
    if (length(issue_body) > 0) {
      # If this is the first issue recorded for this student/team
      if (is.na(curr_row$issue_titles) || curr_row$issue_titles == "NA") {
        curr_row$issue_qs <- curr_q
        curr_row$issue_titles <- issue_title
        curr_row$issue_bodies <- issue_body
        curr_row$issue_pushed <- "FALSE"
        
      } else {
        curr_row$issue_qs <- paste0(
          curr_row$issue_qs, "&&&", curr_q
        )
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
      
      cat(paste(
        "The issue has been noted in the grading progress log.",
        "After grading use push_to_github() to push issues.",
        sep = "\n"
      ))
    
    }
    
  }
  
  curr_row
  
}
