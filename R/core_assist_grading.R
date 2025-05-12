#' Core function that assists with grading, creates feedback and grade book files
#'
#' @inheritParams assist_grading_functions
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#' 
#' @import readr
#' @import dplyr
#' @import stringr
#' @import rstudioapi
#' @import svDialogs
#' @import fs
#' 
#' @keywords internal
#' 
core_assist_grading <- function(
    rubric_path,
    roster_path,
    grading_progress_log_path,
    final_grade_sheet_path,
    example_assignment_path,
    example_feedback_path,
    example_student_identifier,
    missing_assignment_grade = NA,
    questions_to_grade = "all",
    students_to_grade = "all",
    team_grading = FALSE,
    github_issues = FALSE
  ) {
  
  # Check example_assignment_path is valid input
  if (example_assignment_path != "no_submissions") {
    if (!is.vector(example_assignment_path)) {
      stop("example_assignment_path must be a single string or a vector of strings")
    } else if (!all(stringr::str_detect(example_assignment_path, example_student_identifier))) {
      stop("The example_student_identifier must be present in the example_assignment_path.")
    }
  } 

  # Check that feedback, grading progress log and final grade sheets paths 
  # include correct directories
  paths_to_write_to <- c(
    example_feedback_path,
    grading_progress_log_path,
    final_grade_sheet_path
  )
  
  for (p in paths_to_write_to) {
    if (stringr::str_detect(p, "/")) {
      if (!dir.exists(fs::path_dir(p))) {
       stop(paste("This directory seems to be incorrect:", p))
         
      }
    }
  }
  
  # Check that grading progress log and final grade sheet paths are to .csv files
  temp_file_ext <- fs::path_ext(grading_progress_log_path)
  final_file_ext <- fs::path_ext(final_grade_sheet_path)
  if (!all(c(temp_file_ext, final_file_ext) %in% c("csv"))) {
    stop("The extension of grading_progress_log_path and final_grade_sheet_path must be .csv")
    
  }
  
  # Check that grading progress log and final grade sheet paths differ
  if (grading_progress_log_path == final_grade_sheet_path) {
    stop("Inputs grading_progress_log_path and final_grade_sheet_path need to be different (otherwise one file would overwrite the other).")
    
  } 
    
  # Check example_feedback_path is valid
  feedback_file_ext <- fs::path_ext(example_feedback_path)
  
  if (!(feedback_file_ext %in% c("md", "Rmd", "qmd", "html", "pdf", "docx"))) {
    stop("The extension of the example_feedback_path must be one of the following: '.md', '.Rmd', '.qmd', '.html', '.pdf', '.docx'.")
  } else if (!stringr::str_detect(example_feedback_path, example_student_identifier)) {
    stop("The example_student_identifier must be present in the example_feedback_path file name.")
  } 
  
  # Import rubric and create rubric prompts
  rubric_list <- import_rubric(rubric_path)
  rubric_prompts <- create_rubric_prompts(
    rubric_list, 
    github_issues = github_issues
  )
  gf_in_rubric <- "general_feedback" %in% names(rubric_prompts)
  qs_valid <- questions_to_grade %in% names(rubric_prompts)
  original_rubric_list <- rubric_list # to keep track of addition to rubric while grading
  
  if (all(questions_to_grade == "all")) {
    questions_to_grade <- names(rubric_prompts)
    
  } else if (!all(qs_valid) || length(questions_to_grade) == 0) {
    stop(
      "\nSome or all questions specified in questions_to_grade are not in the 'names' column in the rubric."
    )
    
  } else if (!("general_feedback" %in% questions_to_grade) && gf_in_rubric) {
    questions_to_grade <- c(questions_to_grade, "general_feedback")
    
  }
  
  # Create /(load and merge) grading progress log
  grading_progress_log <- create_grading_progress_log(
    grading_progress_log_path = grading_progress_log_path,
    example_assignment_path = example_assignment_path, 
    example_feedback_path = example_feedback_path,
    example_student_identifier = example_student_identifier, 
    roster_path = roster_path,
    github_issues = github_issues,
    team_grading = team_grading
  )
  
  # Specify which students to grade
  if (students_to_grade == "all") {
    students_to_grade <- grading_progress_log$student_identifier
  }
  
  grading_progress_log <- grading_progress_log %>% 
    mutate(grade_student = student_identifier %in% students_to_grade)

  continue_grading <- TRUE
  
  # Loop through students, grading each
  for (i in 1:nrow(grading_progress_log)) {
    if (continue_grading) {
      # Currently, if a feedback file exists that student does not get graded because their status is changed to feedback created
      all_qs_graded <- grading_progress_log$grading_status[i] == "all questions graded"
      
      assignment_should_be_graded <- (
        # Assignment is not missing
        !grading_progress_log$assignment_missing[i] &&
        # This student is supposed to be graded this session
        grading_progress_log$grade_student[i] &&
        # All questions have not already been graded for this student
        !all_qs_graded
      )
        
      if (assignment_should_be_graded) {
        if (grading_progress_log$grading_status[i] == "feedback created") {
          # Print message
          begin_message <- paste(
            paste0(
              "You are going to grade ",
              ifelse(team_grading, yes = "team: ", no = "student: "),
              grading_progress_log$student_identifier[i],
              ".\n This assignment was previously partially graded, with the last edit performed at: ",
              grading_progress_log$last_time_graded[i],
              ".\n The following questions were already graded: ",
              stringr::str_replace_all(
                grading_progress_log$graded_qs[i], 
                pattern = "&&&", 
                replacement = ", "
              ),
              ".\n You will only be prompted to grade ungraded questions for this assignment.\n"
            ),
            "Press [enter] or [ok] to continue or [cancel] to stop.",
            sep = "\n"
          )
          
        } else {
          begin_message <- paste(
            paste0(
              "You are going to grade ",
              ifelse(team_grading, yes = "team: ", no = "student: "),
              grading_progress_log$student_identifier[i],
              "."
            ),
            "Press [enter] or [ok] to continue or [cancel] to stop.",
            sep = "\n"
          )
          
        }
        
        continue_grading <- svDialogs::ok_cancel_box(begin_message)
        
        cat("\n")
        
        if (continue_grading) {
          
          if (grading_progress_log$assignment_path[i] != "no_submissions") {
            assignment_path <- unlist(
              stringr::str_split(grading_progress_log$assignment_path[i], ", ")
            )
            
            doc_id <- NULL
            
            for(j in 1:length(assignment_path)) {
              # Open file
              rstudioapi::navigateToFile(assignment_path[j], moveCursor = FALSE)
              
              # Need short pause so documentId() grabs the correct document
              Sys.sleep(1)
              doc_id[j] <- rstudioapi::documentId()
            }
            
          }
          
          temp_obj <- grade_student(
            row = i, 
            grading_progress_log = grading_progress_log, 
            grading_progress_log_path = grading_progress_log_path,
            rubric_prompts = rubric_prompts,
            rubric_list = rubric_list,
            rubric_path = rubric_path,
            github_issues = github_issues,
            questions_to_grade = questions_to_grade
          )
          
          if (example_assignment_path != "no_submissions") {
            for(j in 1:length(assignment_path)) {
              # Close assignment
              invisible(rstudioapi::documentClose(id = doc_id[j], save = FALSE))
            }
          }
          
          # Check if grading has been suspended
          if (is.null(temp_obj)) {
            Sys.sleep(1)
            cat("Grading has been suspended.")
            cat("\nMake sure to keep the grading progress log,")
            cat("\nit is necessary to maintain grading progress.\n")
            
            continue_grading <- FALSE
            
            Sys.sleep(1)
            
          } else {
            grading_progress_log <- temp_obj
            
          } 
          
          # Recreate rubric list and prompts, in case they have been modified
          # while grading this student
          rubric_list <- import_rubric(rubric_path)
          rubric_prompts <- create_rubric_prompts(
            rubric_list,
            github_issues = github_issues
          )
          
          # To allow enough time between grading of two students,
          # so that Total grade assigned to last student can be printed and seen before
          # the grader is prompted to grade a new student
          Sys.sleep(1)
          
        } 
      }
    } 
  }
  
  if (file.exists(grading_progress_log_path)) {
    grading_progress_log <- readr::read_csv(
      grading_progress_log_path,
      show_col_types = FALSE,
      col_types = readr::cols(
        .default = readr::col_character(),
        assignment_missing = readr::col_logical(),
        grade_student = readr::col_logical(),
        last_time_graded = readr::col_datetime()
      )
    )
    
  }

  if (any(grading_progress_log$grading_status != "ungraded")) {
    create_final_grade_sheet(
      grading_progress_log = grading_progress_log, 
      final_grade_sheet_path = final_grade_sheet_path,
      missing_assignment_grade = missing_assignment_grade,
      rubric_list = rubric_list,
      rubric_prompts = rubric_prompts,
      team_grading = team_grading
    )
  }
  
  if(!(feedback_file_ext %in% c("Rmd", "qmd"))) {
    render_feedback_now <- svDialogs::dlg_message(
      "Would you like the feedback files to be rendered now?",
      type = "yesno"
    )$res
    
    if (render_feedback_now == "yes") {
      render_feedback(grading_progress_log = grading_progress_log) 
    }
  }
  
}
