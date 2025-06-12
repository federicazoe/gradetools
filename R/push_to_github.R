
#' Push feedback and issues to GitHub
#'
#' @inheritParams assist_grading_functions
#' @param class_github_name string, GitHub name of the class. This input is only needed if \code{github_issues} is set to TRUE.
#' @param example_github_repo string; example of the name of a student's GitHub repo for this assignment (or team's repo, if \code{team_grading} = TRUE). The \code{example_identifier} must feature in the \code{example_github_repo} and must be the same as the roster identifier for the student (or for the team if \code{team_grading} = TRUE).
#' @param example_identifier string; the GitHub username of the student or the name of the team, if \code{team_grading} = TRUE. This needs to be present somewhere in \code{example_github_repo}.
#' @param push_feedback logical, whether to push feedback files
#' @param create_issues logical, whether to create issues
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#' 
#' @export
#' 
#' @import readr
#' @import dplyr
#' @import stringr
#' @import ghclass
#'
push_to_github <- function(
    grading_progress_log_path,
    class_github_name,
    example_github_repo,
    example_identifier,
    push_feedback = FALSE,
    create_issues = FALSE,
    team_grading = FALSE
  ) {
  
  if (push_feedback == FALSE & create_issues == FALSE) {
    stop("At least one between push_feedback and create_issues must be set to TRUE.")
  }
  if (!file.exists(grading_progress_log_path)) {
    stop("No file found at the specified grading_progress_log_path. Please provide a valid path.")
  }
  
  if (!ghclass::org_exists(class_github_name)) {
    stop("The specified class_github_name does not correspond to the name of an existing organization on GitHub.")
  }
  
  if (!ghclass::repo_exists(paste(class_github_name, example_github_repo, sep = "/"))) {
    stop(paste(example_github_repo, "does not correspond to a repository in the GitHub organization", class_github_name))
  }
  
  if (!stringr::str_detect(example_github_repo, example_identifier)) {
    stop("Input example_github_repo does not contain input example_identifier. Make sure repos are distinguished by their identifiers.")
  }
  
  grading_progress_log <- readr::read_csv(
    grading_progress_log_path,
    show_col_types = FALSE,
    col_types = cols(
      .default = readr::col_character(),
      assignment_missing = readr::col_logical(),
      grade_student = readr::col_logical(),
      last_time_graded = readr::col_datetime()
    )
  )
  
  if (!("feedback_pushed" %in% colnames(grading_progress_log))) {
    # This means it is the first time that the grader tries to push feedback 
    # from this grading progress log
    
    # Initialize column that keeps track of which feedback files have been pushed
    grading_progress_log$feedback_pushed <- "FALSE"
    
    # Use the example GitHub identifier and GitHub repos provided to guess
    # the name of all student
    grading_progress_log$github_repo <- stringr::str_replace_all(
      example_github_repo, 
      pattern = example_identifier, 
      replacement = grading_progress_log$student_identifier
    )
    
    readr::write_csv(grading_progress_log, file = grading_progress_log_path) 
    
  }

  if (push_feedback) {
    push_feedback_github(
      grading_progress_log_path = grading_progress_log_path,
      grading_progress_log = grading_progress_log,
      class_github_name = class_github_name
    )
  }
  
  if (create_issues) {
    create_issues_github(
      grading_progress_log_path = grading_progress_log_path,
      grading_progress_log = grading_progress_log,
      class_github_name = class_github_name,
      team_grading = team_grading
    )
  }
    
}


  
#' Push feedback to GitHub
#'
#' @param grading_progress_log_path string; assist-grading() functions save a file which includes information for gradetools's internal use. 
#'     This is that path for that file. Must be a .csv
#' @param grading_progress_log tibble created by assist_advanced_grading().
#' @param class_github_name string, GitHub name of the class. This input is needed to push feedback and issues to the correct class on GitHub.
#'
#' @import readr
#' @import dplyr
#' @import stringr
#' @import ghclass
#' @importFrom svDialogs dlg_message
#' 
#' @keywords internal
#'
push_feedback_github <- function(
  grading_progress_log_path,
  grading_progress_log,
  class_github_name
) {
  
  all_push_statuses <- grading_progress_log$feedback_pushed
  
  # Interrupt pushing feedback if there is no feedback to push
  if (all(all_push_statuses == "TRUE")) {
    return(message(
      paste(
        "All feedback files have been pushed in this grading progress log.",
        "Are you sure that you have provided the right grading_progress_log_path?"
      )))
  }
  
  partially_graded <- svDialogs::dlg_message(
    c("Would you like to push feedback also for assignments that have been partially graded?",
      "If you select 'no', then feedback will only be pushed for fully graded assignments"),
    type = "yesno"
  )$res 
  
  if (partially_graded == "yes") {
    relevant_rows <- grading_progress_log$grading_status %in% 
                        c("all questions graded", "feedback created")
    if (all(relevant_rows == FALSE)) {
      stop("No feedback file has been created for this assignment yet.")
    }
    no_feedback_to_push_msg <- 
      "All feedback files that were created for this assignment have already been pushed."
    
  } else {
    relevant_rows <- (grading_progress_log$grading_status == "all questions graded")
    no_feedback_to_push_msg <- 
      "All feedback files for fully graded students have already been pushed."
    if (all(relevant_rows == FALSE)) {
      return(
        stop("No student has been fully graded for this assignment yet.")
      )
    }
  }
  
  relevant_push_statuses <- grading_progress_log$feedback_pushed[relevant_rows]
  
  # Interrupt pushing feedback if there is no feedback to push
  if (all(relevant_push_statuses == 'TRUE')) {
    return(message(no_feedback_to_push_msg))
  }
  
  for (i in 1:nrow(grading_progress_log)) {
    
    feedback_to_push <- (grading_progress_log$feedback_pushed[i] == "FALSE")
    
    if (partially_graded == "yes") {
      feedback_to_push <- (grading_progress_log$feedback_pushed[i] == "FALSE") &
        (grading_progress_log$grading_status[i] 
         %in% c("all questions graded", "feedback created"))
      
    } else {
      feedback_to_push <- (grading_progress_log$feedback_pushed[i] == "FALSE") &
        (grading_progress_log$grading_status[i] == "all questions graded")
      
    }
    
    if (feedback_to_push) {
      
      github_repo <- grading_progress_log$github_repo[i]
      feedback_path <- grading_progress_log$feedback_path_to_be_knitted[i]
      
      if (repo_exists(paste(class_github_name, github_repo, sep = "/"))) {
        ghclass::repo_add_file(
          repo = ghclass::org_repos(
            class_github_name, 
            paste0("\\b", github_repo, "$")
          ),
          message = "Feedback",
          file = feedback_path,
          overwrite = TRUE
        )
        
        grading_progress_log$feedback_pushed[i] <- "TRUE"
        
        # Update grading progress log (feedback_pushed for this row has changed)
        readr::write_csv(grading_progress_log, file = grading_progress_log_path) 
    
      } else {
        
        warning(paste(
          github_repo,
          "is not a repo in",
          class_github_name,
          "so the corresponding feedback file could not be pushed."
        ))
        
      }
      
    } 
  
  }
  
}
  


#' Push issues to GitHub
#' 
#' @param grading_progress_log_path string; assist-grading() functions save a file which includes information for gradetools's internal use. 
#'     This is that path for that file. Must be a .csv
#' @param grading_progress_log tibble created by assist_advanced_grading().
#' @param class_github_name string, GitHub name of the class. This input is needed to push feedback and issues to the correct class on GitHub.
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#' 
#' @import readr
#' @import dplyr
#' @import stringr
#' @import ghclass
#' @import svDialogs 
#' 
#' @keywords internal
#' 
create_issues_github <- function(
    grading_progress_log_path,
    grading_progress_log,
    class_github_name, 
    team_grading = FALSE
  ) {
  
  
  if (!("issue_titles" %in% colnames(grading_progress_log))) {
    stop("No column `issue_titles` was found in this grading progress log. Are you sure that you have used assist grading with `github_issues = TRUE` and that you have specified the right `grading_progress_log_path`?")
  }
  
  if (team_grading & !("students_in_team" %in% colnames(grading_progress_log))) {
    stop(
        paste(
          "You set team_grading = TRUE",
          "but there is no students_in_team column in the grading progress log",
          "loaded from the path you specified. Are you sure that the path",
          "is correct and that you graded using assist_team_grading()?"
        )
    )
  }
  
  all_push_statuses <- stringr::str_split(
    string = grading_progress_log$issue_pushed, 
    pattern = "&&&"
  ) %>% 
    unlist()
  
  # Interrupt creating issues if there are no issues to create
  if (all(is.na(all_push_statuses))) {
    return(message(
      paste(
        "No issues have been annotated in this grading progress log.",
        "Are you sure that you have provided the right grading_progress_log_path?"
      )))
  }
  
  partially_graded <- svDialogs::dlg_message(
    c("Would you like to create issues also for assignments that have been partially graded?",
      "If you select 'no', then issues will only be created for fully graded assignments"),
    type = "yesno"
  )$res 
  
  if (partially_graded == "yes") {
    relevant_rows <- c(1:nrow(grading_progress_log))
  } else {
    relevant_rows <- grading_progress_log$grading_status == "all questions graded"
  }
  
  relevant_push_statuses <- stringr::str_split(
    string = grading_progress_log$issue_pushed[relevant_rows], 
    pattern = "&&&"
  ) %>% 
    unlist()
  
  # Interrupt creating issues if there are no issues to create
  if (all(is.na(relevant_push_statuses))) {
    return(message(
      paste(
        "No issues have been noted in this grading progress log.",
        "Are you sure that you have provided the right grading_progress_log_path?"
      )))
  }
  
  # Interrupt creating issues if all annotated issues have already been pushed
  if (!any(relevant_push_statuses == "FALSE")) {
    return(message(
      paste(
        "All issues that were noted in this grading progress log have already been pushed"
      )))
  }
  
  show_issue_summary <- svDialogs::dlg_message(
    c("Would you like to see and confirm each issue before creating it?",
    "Creating issues on GitHub cannot be undone with gradetools."),
    type = "yesno"
  )$res
  
  for (i in 1:nrow(grading_progress_log)) {
    
    are_there_issues <- !is.na(grading_progress_log$issue_titles[i])
    issues_to_be_pushed <- 
      (grading_progress_log$grading_status[i] == "all questions graded") |
      (partially_graded == "yes")
    
    if (are_there_issues & issues_to_be_pushed) {
      
      github_repo <- grading_progress_log$github_repo[i]
      
      if (ghclass::repo_exists(paste(class_github_name, github_repo, sep = "/"))) {
        
        issue_titles <- grading_progress_log$issue_titles[i] %>% 
          stringr::str_split(pattern = "&&&") %>%
          unlist()
        
        issue_bodies <- grading_progress_log$issue_bodies[i] %>% 
          stringr::str_split(pattern = "&&&") %>%
          unlist()
        
        num_issues <- length(issue_titles)
        
        current_push_status <-  stringr::str_split(
          string = grading_progress_log$issue_pushed[i], 
          pattern = "&&&"
        ) %>% 
          unlist()
        
        if (team_grading) {
          assignees <- stringr::str_split(
            string = grading_progress_log$students_in_team[i], 
            pattern = "&&&"
          ) %>% 
            unlist()
          
          assignees <- paste0("@", assignees) %>% 
            stringr::str_c(collapse = " ")
          
        }
        
        for (j in 1:num_issues) {
          
          # Verify is issue has been pushed already
          already_pushed <- current_push_status[j] == "TRUE"
          
          if (!already_pushed) {
            
            proceed_with_issue <- TRUE
            
            if (show_issue_summary == "yes") {
              new_issue <- paste(
                "You are about to create the following issue: \n",
                paste0("Issue title: ", issue_titles[j]),
                paste0("Issue body: ", issue_bodies[j]),
                paste0(
                  "Assignees: ", 
                  ifelse(
                    !team_grading,
                    grading_progress_log$student_identifier[i],
                    assignees
                  )
                ),
                sep = "\n"
              )
              
              proceed_message <-  paste(
                new_issue,
                "\nTo create this issue, press [ok]",
                "To skip this issue, press [cancel].",
                sep = "\n"
              )
              
              proceed_with_issue <- svDialogs::ok_cancel_box(proceed_message)
            }
            
            if (proceed_with_issue) {
              
              if (!team_grading) {
                
                if (proceed_with_issue) {
                  ghclass::issue_create(
                    repo = ghclass::org_repos(class_github_name, github_repo),
                    title = issue_titles[j],
                    body = issue_bodies[j],
                    assignees = grading_progress_log$student_identifier[i]
                  )
                }
                
                
                
              } else {
                # Wish we could do this, but multiple assignees aren't supported
                # by GitHub for private repos in organization owned by free-plan
                # users
                # assignees <- roster %>% 
                #   filter(team_identifier == 
                #            grading_progress_log$student_identifier[i]) %>% 
                #   pull(student_identifier)
                #
                # ghclass::issue_create(
                #   repo = ghclass::org_repos(class_github_name, repo_path),
                #   title = issue_titles[i],
                #   body = issue_bodies[i],
                #   assignees = assignees
                # )
                
                ghclass::issue_create(
                  repo = ghclass::org_repos(class_github_name, github_repo),
                  title = issue_titles[j],
                  body = paste(issue_bodies[j], assignees)
                )
                
              }
              
              current_push_status[j] <- "TRUE"
              
              grading_progress_log$issue_pushed[i] <- stringr::str_c(
                string = current_push_status, 
                collapse = "&&&"
              )
              
              readr::write_csv(grading_progress_log, file = grading_progress_log_path) 
              
            }
            
          }
          
        }
        
      } else {
        
        warning(paste(
          github_repo,
          "is not a repo in",
          class_github_name,
          "so the corresponding issues could not be created."
        ))
        
      }
      
    }
  }
  
}
  