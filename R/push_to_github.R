
#' Push feedback and issues to GitHub
#'
#' @inheritParams assist_grading_functions
#' @param class_github_name string, GitHub name of the class. This input is only needed if github_issues is set to TRUE.
#' @param example_github_repo string; example of the name of a student's GitHub repo for this assignment (or team's repo, if team_grading = TRUE). The example_identifier must feature in the example_github_repo and must be the same as the roster identifier for the student (or for the team if team_grading = TRUE).
#' @param example_identifier string; the GitHub username of the student or the name of the team, if team_grading = TRUE. This needs to be present somewhere in example_github_repo.
#' @param push_feedback logical, whether to push feedback files
#' @param create_issues logical, whether to create issues
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#' 
#' @export
#' @import readr
#' @import dplyr
#' @import stringr
#' @import ghclass
#'
push_to_github <- function(
    temp_grade_sheet_path,
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
  if (!file.exists(temp_grade_sheet_path)) {
    stop("No file found at the specified temp_grade_sheet_path. Please provide a valid path.")
  }
  
  if (!org_exists(class_github_name)) {
    stop("The specified class_github_name does not correspond to the name of an existing organization on GitHub.")
  }
  
  if (!repo_exists(paste(class_github_name, example_github_repo, sep = "/"))) {
    stop(paste(example_github_repo, "does not correspond to a repository in the GitHub organization", class_github_name))
  }
  
  if (!str_detect(example_github_repo, example_identifier)) {
    stop("Input example_github_repo does not contain input example_identifier. Make sure repos are distinguished by their identifiers.")
  }
  
  temp_grade_sheet <- readr::read_csv(
    temp_grade_sheet_path,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_character(),
      assignment_missing = col_logical(),
      grade_student = col_logical(),
      last_time_graded = col_datetime()
    )
  )
  
  if (!("feedback_pushed" %in% colnames(temp_grade_sheet))) {
    # This means it is the first time that the grader tries to push feedback 
    # from this temporary grade sheet
    
    # Initialize column that keeps track of which feedback files have been pushed
    temp_grade_sheet$feedback_pushed <- "FALSE"
    
    # Use the example github identifier and github repos provided to guess
    # the name of all student
    temp_grade_sheet$github_repo <- str_replace_all(
      example_github_repo, 
      pattern = example_identifier, 
      replacement = temp_grade_sheet$student_identifier
    )
    
    write_csv(temp_grade_sheet, file = temp_grade_sheet_path) 
    
  }

  if (push_feedback) {
    push_feedback_github(
      temp_grade_sheet_path = temp_grade_sheet_path,
      temp_grade_sheet = temp_grade_sheet,
      class_github_name = class_github_name
    )
  }
  
  if (create_issues) {
    create_issues_github(
      temp_grade_sheet_path = temp_grade_sheet_path,
      temp_grade_sheet = temp_grade_sheet,
      class_github_name = class_github_name,
      team_grading = team_grading
    )
  }
    
}


  
#' Push feedback to GitHub
#'
#' @param temp_grade_sheet_path string; assist-grading() functions save a file which includes information for gradetools's internal use. 
#'     This is that path for that file. Must be a .csv
#' @param temp_grade_sheet tibble created by assist_advanced_grading().
#' @param class_github_name string, GitHub name of the class. This input is needed to push feedback and issues to the correct class on GitHub.
#'
#' @import readr
#' @import dplyr
#' @import stringr
#' @import ghclass
#' 
#' @keywords internal
#'
push_feedback_github <- function(
  temp_grade_sheet_path,
  temp_grade_sheet,
  class_github_name
) {
  
  for (i in 1:nrow(temp_grade_sheet)) {
    
    if (temp_grade_sheet$grading_status[i] == "all questions graded" &
        temp_grade_sheet$feedback_pushed[i] == "FALSE") {
      
      github_repo <- temp_grade_sheet$github_repo[i]
      feedback_path <- temp_grade_sheet$feedback_path_to_be_knitted[i]
      
      if (repo_exists(paste(class_github_name, github_repo, sep = "/"))) {
        ghclass::repo_add_file(
          repo = ghclass::org_repos(class_github_name, github_repo),
          message = "Feedback",
          file = feedback_path,
          overwrite = TRUE
        )
        
        temp_grade_sheet$feedback_pushed[i] <- "TRUE"
        
        # Update temporary grade sheet (feedback_pushed for this row has changed)
        write_csv(temp_grade_sheet, file = temp_grade_sheet_path) 
    
      } else {
        
        warning(paste(github_repo,
                      "is not a repo in",
                      class_github_name,
                      "so the corresponding feedback file could not be pushed."
          )
        )
        
      }
      
    } 
  
  }
  
}
  


#' Push issues to GitHub
#' 
#' @param temp_grade_sheet_path string; assist-grading() functions save a file which includes information for gradetools's internal use. 
#'     This is that path for that file. Must be a .csv
#' @param temp_grade_sheet tibble created by assist_advanced_grading().
#' @param class_github_name string, GitHub name of the class. This input is needed to push feedback and issues to the correct class on GitHub.
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#' 
#' @import readr
#' @import dplyr
#' @import stringr
#' @import ghclass
#' 
#' @keywords internal
#' 
create_issues_github <- function(
    temp_grade_sheet_path,
    temp_grade_sheet,
    class_github_name, 
    team_grading = FALSE
  ) {
  
  
  if (!("issue_titles" %in% colnames(temp_grade_sheet))) {
    stop("No column `issue_titles` was found in this temporary grade sheet. Are you sure that you have used assist grading with `github_issues = TRUE` and that you have specified the right `temp_grade_sheet_path`?")
  }
  
  if (team_grading & !("students_in_team" %in% colnames(temp_grade_sheet))) {
    stop(
        paste(
          "You set team_grading = TRUE",
          "but there is no students_in_team column in the temporary grade sheet",
          "loaded from the path you specified. Are you sure that the path",
          "is correct and that you graded using assist_team_grading()?"
        )
    )
  }
  
  show_issue_summary <- dlg_message(
    "Would you like to confirm each issue before creating it?",
    type = "yesno"
  )$res
  
  
  for (i in 1:nrow(temp_grade_sheet)) {
    
    are_there_issues <- !is.na(temp_grade_sheet$issue_titles[i])
    
    if (are_there_issues == TRUE) {
      
      github_repo <- temp_grade_sheet$github_repo[i]
      
      if (repo_exists(paste(class_github_name, github_repo, sep = "/"))) {
        
        issue_titles <- temp_grade_sheet$issue_titles[i] %>% 
          str_split(pattern = "&&&") %>%
          unlist()
        
        issue_bodies <- temp_grade_sheet$issue_bodies[i] %>% 
          str_split(pattern = "&&&") %>%
          unlist()
        
        num_issues <- length(issue_titles)
        
        current_push_status <-  str_split(
          string = temp_grade_sheet$issue_pushed[i], 
          pattern = "&&&"
        ) %>% 
          unlist()
        
        if (team_grading) {
          assignees <- str_split(
            string = temp_grade_sheet$students_in_team[i], 
            pattern = " &&& "
          ) %>% 
            unlist()
          
          assignees <- paste0("@", assignees) %>% 
            str_c(collapse = " ")
          
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
                paste0("Assignees: ", 
                       ifelse(!team_grading,
                              temp_grade_sheet$student_identifier[i],
                              assignees)),
                       sep = "\n"
              )
              proceed_message <-  paste(
                new_issue,
                "\nTo create this issue, press [ok]",
                "To skip this issue, press [cancel].",
                sep = "\n"
              )
              proceed_with_issue <- ok_cancel_box(proceed_message)
            }
            
            if (proceed_with_issue) {
              
              if (!team_grading) {
                
                if (proceed_with_issue) {
                  ghclass::issue_create(
                    repo = ghclass::org_repos(class_github_name, github_repo),
                    title = issue_titles[j],
                    body = issue_bodies[j],
                    assignees = temp_grade_sheet$student_identifier[i]
                  )
                }
                
                
                
              } else {
                # Wish we could do this, but multiple assignees aren't supported
                # by GitHub for private repos in organization owned by free-plan
                # users
                # assignees <- roster %>% 
                #   filter(team_identifier == 
                #            temp_grade_sheet$student_identifier[i]) %>% 
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
              
              temp_grade_sheet$issue_pushed[i] <- str_c(
                string = current_push_status, 
                collapse = "&&&"
              )
              
              write_csv(temp_grade_sheet, file = temp_grade_sheet_path) 
              
            }
            
          }
          
        }
        
      } else {
        
        warning(paste(github_repo,
                      "is not a repo in",
                      class_github_name,
                      "so the corresponding issues could not be created.")
        )
      }
      
    }
  }
  
}
  