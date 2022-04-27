#' Assists with regrading, given assist_grading() or assist_advanced_grading() was previously used
#'
#' @inheritParams assist_grading_functions
#' @param questions_to_regrade vector of strings; vector of assignment question 'names' to regrade. This can be specified as "all" to apply to all questions. questions_to_grade must exactly match ones present in the rubric
#' @param students_to_regrade vector of strings; vector of student identifiers to regrade. This can be specified as "all" to apply to all student's in the rubric. students_to_grade must be student_identifiers present in the roster. This should not be provided if the assignment involved team grading
#' @param teams_to_regrade vector of strings; This argument is for team grading (i.e. if one assignment is connected with multiple students). team_identifiers corresponding to teams to grade, or "all" to specify all assignments should be graded. All teams_to_regrade must be team_identifiers present in the roster
#' 
#' @import readr 
#' @importFrom stringr str_split
#' @importFrom fs path_ext
#'
#' @export
#'
assist_regrading <- function(
    rubric_path,
    temp_grade_sheet_path,
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
  if (!file.exists(temp_grade_sheet_path)) {
    stop("Nothing was found at the temp_grade_sheet_path!")
    
  } else if (!file.exists(rubric_path)) {
    stop("Nothing was found at the rubric_path!")
    
  }
  
  # Check temporary and final grade sheets paths differ
  if (temp_grade_sheet_path == final_grade_sheet_path) {
    stop("Inputs temp_grade_sheet_path and final_grade_sheet_path need to be different (otherwise one file would overwrite the other).")
    
  } 
  
  temp_grade_sheet <- readr::read_csv(
    temp_grade_sheet_path,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_character(),
      assignment_missing = col_logical(),
      grade_student = col_logical(),
      last_time_graded = col_datetime(),
    )
  )

  # Determine if it is team grading
  team_grading <- "students_in_team" %in% colnames(temp_grade_sheet)
  
  if (is.null(teams_to_regrade) && team_grading) {
    stop(paste0(
      "\nThe temporary grade sheet indicates that the assignment involved team grading but teams_to_regrade was not provided.",
      "\nPlease rerun and provide teams_to_regrade instead of students_to_regrade."
    ))
  } else if (!is.null(teams_to_regrade) && !team_grading) {
    stop(paste0(
      "\nThe temporary grade sheet indicates that the assignment did not involve team grading but teams_to_regrade was provided.",
      "\nPlease rerun and provide students_to_regrade instead of teams_to_regrade."
    ))
  }
  
  identifier_valid <- students_to_regrade %in% temp_grade_sheet$student_identifier
  
  # Check if the students_to_regrade are valid
  if (all(students_to_regrade == "all")) {
    students_to_regrade <- temp_grade_sheet$student_identifier
    
  } else if (!all(identifier_valid) || length(students_to_regrade) == 0) {
    stop(
      "\nSome or all students_to_regrade are not in the 'student_identifier' column of the temporary gradesheet.
      \nIf there is a change to the roster please rerun assist_grading() or assist_advanced_grading()."
    )
  }
  
  temp_grade_sheet$grade_student <- temp_grade_sheet$student_identifier %in% students_to_regrade
  
  id_ungraded <- temp_grade_sheet$grading_status == "ungraded"
  id_ungraded_to_be_graded <- temp_grade_sheet$grade_student & id_ungraded
  
  if (any(id_ungraded_to_be_graded)) {
    ungraded_present_message <- paste0(
      "The following students_to_regrade were not previously graded: ",
      paste(
        temp_grade_sheet$student_identifier[id_ungraded_to_be_graded], 
        sep = ", "
      ), 
      ".  Would you still like to grade these previously ungraded students?"
    )
    
    grade_ungraded <- dlg_message(ungraded_present_message, type = "yesno")$res
    
    if (grade_ungraded == "no") {
      handling_ungraded_message <- paste0(
        "Would you still like to continue with regrading the previously graded students?"
      )
      
      continue_grading <- dlg_message(
        handling_ungraded_message, 
        type = "yesno"
      )$res
      
      if (continue_grading == "yes") {
        temp_grade_sheet$grade_student[id_ungraded_to_be_graded] <- FALSE
        
        if (!any(temp_grade_sheet$grade_student)) {
          return(
            "There are no students to be regraded. Regrading has been cancelled."
          )
        }
        
      } else {
        return("Regrading has been cancelled.")
        
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
    
  } else if (!("general_feedback" %in% questions_to_regrade) && gf_in_rubric) {
    questions_to_regrade <- c(questions_to_regrade, "general_feedback")
    
  }
  
  rows_to_be_graded <- which(
    temp_grade_sheet$grade_student & !temp_grade_sheet$assignment_missing
  )
  
  continue_grading <- TRUE
  
  for (i in rows_to_be_graded) {
    if (continue_grading) {
      curr_id <- temp_grade_sheet$student_identifier[i]
      
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
        student_not_ungraded <- temp_grade_sheet$grading_status[i] != "ungraded"
        
        graded_questions <- unlist(stringr::str_split(
          temp_grade_sheet$graded_qs[i], 
          pattern = "&&&"
        ))
        
        # Get assignment_path
        assignment_path <- unlist(
          str_split(temp_grade_sheet$assignment_path[i], ", ")
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
            curr_temp_grade_sheet <- temp_grade_sheet
            
            if (student_not_ungraded && q %in% graded_questions) {
              temp_grade_sheet <- delete_student_grading_progress(
                temp_grade_sheet_path = temp_grade_sheet_path, 
                rubric_path = rubric_path,
                identifier = curr_id,
                questions_to_delete = q,
                github_issues = github_issues
              )
              
            }
            
            temp_obj <- grade_student(
              row = which(temp_grade_sheet$student_identifier == curr_id),
              temp_grade_sheet = temp_grade_sheet, 
              temp_grade_sheet_path = temp_grade_sheet_path,
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
              temp_grade_sheet <- curr_temp_grade_sheet
              
              write_csv(temp_grade_sheet, file = temp_grade_sheet_path)
              
              cat(paste0(
                "\nGrading has been suspended.",
                "\nAny remaining questions_to_regrade will retain their previous grades."
              ))
              
              continue_grading <- FALSE
              
            } else {
              
              temp_grade_sheet <- temp_obj
              
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
  
  create_final_grade_sheet(
    temp_grade_sheet = temp_grade_sheet, 
    final_grade_sheet_path = final_grade_sheet_path, 
    missing_assignment_grade = missing_assignment_grade, 
    rubric_list = rubric_list, 
    rubric_prompts = rubric_prompts, 
    team_grading = team_grading
  )
  
  feedback_file_ext <- fs::path_ext(
    temp_grade_sheet$feedback_path_to_be_knitted[1]
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
          temp_grade_sheet$feedback_path_Rmd[temp_grade_sheet$grading_status != "ungraded"],
          MoreArgs = list(clean = TRUE, quiet = TRUE)  
        )
        
        cat(paste("\n...Succeeded!\n\n"))
        
        unlink(temp_grade_sheet$feedback_path_Rmd)
        
        if (feedback_file_ext == "md") {
          unlink(fs::path_ext_set(
            path = temp_grade_sheet$feedback_path_Rmd,
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
          "All of your progressed will be saved in the temporary grade sheet.",
          sep = "\n"
        ))
      }
    )
  }
  
}