#' Assists with regrading
#'
#' @description Assists with regrading, given an assist grading function was previously used. Deletes previously specified grades, feedback, comments and issues for each regraded question.
#' @inheritParams assist_grading_functions
#' @param questions_to_regrade vector of strings; vector of assignment question 'names' to regrade. This can be specified as "all" to apply to all questions. questions_to_grade must exactly match ones present in the rubric
#' @param students_to_regrade vector of strings; vector of student identifiers to regrade. This can be specified as "all" to apply to all student's in the rubric. students_to_grade must be student_identifiers present in the roster. This should not be provided if the assignment involved team grading
#' @param teams_to_regrade vector of strings; This argument is for team grading (i.e. if one assignment is connected with multiple students). team_identifiers corresponding to teams to grade, or "all" to specify all assignments should be graded. All teams_to_regrade must be team_identifiers present in the roster
#' 
#' @import readr 
#' @import stringr
#' @import svDialogs
#' @importFrom fs path_ext
#'
#' @export
#'
assist_regrading <- function(
    rubric_path,
    grading_progress_log_path,
    final_grade_sheet_path,
    questions_to_regrade,
    students_to_regrade,
    teams_to_regrade = NULL,
    missing_assignment_grade = NA,
    github_issues = FALSE
  ) {
  
  if (is.null(students_to_regrade) && is.null(teams_to_regrade)) {
    stop("students_to_regrade must be specified unless this assignment involved team grading, then teams_to_regrade must be specified.")
  } else if (!is.null(students_to_regrade) && !is.null(teams_to_regrade)) {
    stop("students_to_regrade and teams_to_regrade cannot both be specified. One must be NULL.")
  } else if (!is.null(teams_to_regrade)) {
    students_to_regrade <- teams_to_regrade
  }
  
  # Check that paths are valid
  if (!file.exists(grading_progress_log_path)) {
    stop("Nothing was found at the grading_progress_log_path!")
    
  } else if (!file.exists(rubric_path)) {
    stop("Nothing was found at the rubric_path!")
    
  }
  
  # Check grading_progress_log and final grade sheets paths differ
  if (grading_progress_log_path == final_grade_sheet_path) {
    stop("Inputs grading_progress_log_path and final_grade_sheet_path need to be different (otherwise one file would overwrite the other).")
    
  } 
  
  grading_progress_log <- readr::read_csv(
    grading_progress_log_path,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_character(),
      assignment_missing = col_logical(),
      grade_student = col_logical(),
      last_time_graded = col_datetime(),
    )
  )

  # Determine if it is team grading
  team_grading <- "students_in_team" %in% colnames(grading_progress_log)
  
  if (is.null(teams_to_regrade) && team_grading) {
    stop(paste0(
      "\nThe grading progress log indicates that the assignment involved team grading but teams_to_regrade was not provided.",
      "\nPlease rerun and provide teams_to_regrade instead of students_to_regrade."
    ))
  } else if (!is.null(teams_to_regrade) && !team_grading) {
    stop(paste0(
      "\nThe grading progress log indicates that the assignment did not involve team grading but teams_to_regrade was provided.",
      "\nPlease rerun and provide students_to_regrade instead of teams_to_regrade."
    ))
  }
  
  identifier_valid <- students_to_regrade %in% grading_progress_log$student_identifier
  
  # Check if the students_to_regrade are valid
  if (all(students_to_regrade == "all")) {
    students_to_regrade <- grading_progress_log$student_identifier
    
  } else if (!all(identifier_valid) || length(students_to_regrade) == 0) {
    stop(
      "\nSome or all students_to_regrade are not in the 'student_identifier' column of the grading progress log.
      \nIf there is a change to the roster please rerun assist_grading() or assist_advanced_grading()."
    )
  }
  
  grading_progress_log$grade_student <- grading_progress_log$student_identifier %in% students_to_regrade
  
  id_ungraded <- grading_progress_log$grading_status == "ungraded"
  id_ungraded_to_be_graded <- grading_progress_log$grade_student & id_ungraded
  
  if (any(id_ungraded_to_be_graded)) {
    ids_to_be_graded_short <- grading_progress_log$student_identifier[id_ungraded_to_be_graded]
    
    if (length(ids_to_be_graded_short) > 10) {
      ids_to_be_graded_short <- c(ids_to_be_graded_short[1:10], "...")
    }
    
    ungraded_present_message <- paste0(
      "The following students_to_regrade were not previously graded: \n",
      str_c(ids_to_be_graded_short, collapse = ",\n"), 
      "\nWould you still like to grade the previously ungraded students?"
    )
    
    grade_ungraded <- dlg_message(ungraded_present_message, type = "yesno")$res
    
    if (grade_ungraded == "no") {
      grading_progress_log$grade_student[id_ungraded_to_be_graded] <- FALSE
      
      if (!any(grading_progress_log$grade_student)) {
        return(
          message(
            "There are no students to be regraded. Regrading has been cancelled."
        ))
      }
      
    }
    
  }
  
  rubric_list <- import_rubric(rubric_path)
  rubric_prompts <- create_rubric_prompts(
    rubric_list,
    github_issues = github_issues
  )
  gf_in_rubric <- "general_feedback" %in% names(rubric_prompts)
  qs_valid <- questions_to_regrade %in% names(rubric_prompts)
  
  # Check if the questions_to_regrade are valid
  if (all(questions_to_regrade == "all")) {
    questions_to_regrade <- names(rubric_prompts)
    
  } else if (!all(qs_valid) || length(questions_to_regrade) == 0) {
    stop(
      "\nSome or all questions_to_regrade are not in the 'names' column in the rubric."
    )
    
  }
  
  rows_to_be_graded <- which(
    grading_progress_log$grade_student & !grading_progress_log$assignment_missing
  )
  
  continue_grading <- TRUE
  
  for (i in rows_to_be_graded) {
    if (continue_grading) {
      curr_id <- grading_progress_log$student_identifier[i]
      
      begin_message <- paste(
        paste0(
          "You are going to grade ",
          ifelse(team_grading, yes = "team: ", no = "student: "),
          curr_id,
          "."
        ),
        "Press [enter] or [ok] to continue or [cancel] to stop.",
        sep = "\n"
      )
      
      continue_grading <- ok_cancel_box(begin_message)
      
      if (!continue_grading) {
        cat(paste0(
          "\nRegrading has been suspended. 
          \nThe remaining questions_to_regrade will maintain their previous grades."
        ))
        
      } else {
        student_not_ungraded <- grading_progress_log$grading_status[i] != "ungraded"
        
        graded_questions <- unlist(stringr::str_split(
          grading_progress_log$graded_qs[i], 
          pattern = "&&&"
        ))
        
        # Get assignment_path
        assignment_path <- unlist(
          str_split(grading_progress_log$assignment_path[i], ", ")
        )
        
        doc_id <- NULL
        
        for(j in 1:length(assignment_path)) {
          navigateToFile(assignment_path[j])
          # Need short pause so documentId grabs the correct document
          Sys.sleep(1)
          doc_id[j] <- documentId()
        }
        
        for (q in questions_to_regrade) {
          if (continue_grading) {
            curr_grading_progress_log <- grading_progress_log
            
            if (student_not_ungraded && q %in% graded_questions) {
              grading_progress_log <- delete_student_grading_progress(
                grading_progress_log_path = grading_progress_log_path, 
                rubric_path = rubric_path,
                identifier = curr_id,
                questions_to_delete = q,
                github_issues = github_issues
              )
              
            }
            
            temp_obj <- grade_student(
              row = which(grading_progress_log$student_identifier == curr_id),
              grading_progress_log = grading_progress_log, 
              grading_progress_log_path = grading_progress_log_path,
              rubric_prompts = rubric_prompts,
              rubric_list = rubric_list,
              rubric_path = rubric_path,
              questions_to_grade = q,
              github_issues = github_issues
            )
            
            # Recreate rubric list and prompts, in case they have been modified
            # while regrading this student
            rubric_list <- import_rubric(rubric_path)
            rubric_prompts <- create_rubric_prompts(
              rubric_list,
              github_issues = github_issues
            )
            
            if (is.null(temp_obj)) {
              grading_progress_log <- curr_grading_progress_log
              
              write_csv(grading_progress_log, file = grading_progress_log_path)
              
              cat(paste0(
                "\nGrading has been suspended.",
                "\nAny remaining questions_to_regrade will retain their previous grades."
              ))
              
              continue_grading <- FALSE
              
            } else {
              
              grading_progress_log <- temp_obj
              
            } 
            
          }
          
        }
        
        # Close assignment
        for(j in 1:length(assignment_path)) {
          invisible(documentClose(id = doc_id[j], save = FALSE))
        }
        
      }
    }
  }
  
  if (file.exists(grading_progress_log_path)) {
    grading_progress_log <- readr::read_csv(
      grading_progress_log_path,
      show_col_types = FALSE,
      col_types = cols(
        .default = col_character(),
        assignment_missing = col_logical(),
        grade_student = col_logical(),
        last_time_graded = col_datetime()
      )
    )
    
  }
  
  create_final_grade_sheet(
    grading_progress_log = grading_progress_log, 
    final_grade_sheet_path = final_grade_sheet_path, 
    missing_assignment_grade = missing_assignment_grade, 
    rubric_list = rubric_list, 
    rubric_prompts = rubric_prompts, 
    team_grading = team_grading
  )
  
  feedback_file_ext <- fs::path_ext(
    grading_progress_log$feedback_path_to_be_knitted[1]
  )
  
  # Let the user know that feedback is being knitted
  cat(paste(
    "\nTrying to knit feedback files to", 
    feedback_file_ext, "format...\n"
  ))
  
  # Knit feedback
  if (feedback_file_ext %in% c("docx", "html", "pdf", "md")) {
    #Try to render feedback files
    tryCatch(               
      # Specifying expression
      expr = {              
        paths_returned <- mapply(
          render,
          grading_progress_log$feedback_path_Rmd[grading_progress_log$grading_status != "ungraded"],
          MoreArgs = list(clean = TRUE, quiet = TRUE)  
        )
        
        cat(paste("\n...Succeeded!\n\n"))
        
        unlink(grading_progress_log$feedback_path_Rmd)
        
        if (feedback_file_ext == "md") {
          unlink(fs::path_ext_set(
            path = grading_progress_log$feedback_path_Rmd,
            ext = "html"
          ))
          
        }
        
      },
      
      # Specifying error message
      error = function(e){         
        cat(paste(
          "There was an error when trying to render the feedback file in the specified format.",
          "\nCompiling pdf's in R requires additional software.",
          "We suggest you rerun the assist grading function with a different feedback_file_format.",
          "All of your progressed will be saved in the grading progress log.",
          sep = "\n"
        ))
      }
    )
  }
  
}