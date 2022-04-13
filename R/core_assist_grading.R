#' Core function that assists with grading, creates feedback and grade book files
#'
#' @param rubric_path string, path to assignment rubric. This rubric should be created using the function create_rubric_template, then filled in by the user. The rubric file name and column names must not be changed.
#' @param roster_path string; file path to the class roster csv containing a column named student_identifier. If team_grading is set to TRUE then the class roster also needs to contain a column named team_identifier
#' @param temp_grade_sheet_path string; assist-grading() functions save a file which includes information for gradetools's internal use. This is that path for that file. Must be a .csv
#' @param final_grade_sheet_path string; path to save final grade sheet to. Must be a .csv
#' @param example_assignment_path string; file path to one of the assignments to be graded. This file path structure will be used to determine where the other assignments to be graded are located. The student identifier has to be present somewhere in the file path
#' @param example_feedback_path string; file path to one of the assignment feedback files that will be generated as the user grades. This file path structure will be used to determine where the other feedback files will be stored. The student identifier must be present somewhere in the file name and must be the only part of the file path unique to the student. The extension of the feedback file must be one of the following: "Rmd", "docx", "html", "pdf"
#' @param example_student_identifier string; a student identifier (e.g. name, id, id number, GitHub user name) that is used to identify the student on the roster. This needs to be present somewhere in the example_assignment_path. The student_identifier needs to be the GitHub user name if the user wishes to push issues or feedback to GitHub later
#' @param missing_assignment_grade numeric; The grade to assign a student with no assignment submission
#' @param questions_to_grade vector of strings; names of assignment questions to grade, or "all" to specify all questions should be graded. All questions_to_grade must exactly match ones present in the rubric
#' @param students_to_grade vector of strings; student_identifiers corresponding to students to grade, or "all" to specify all students should be graded. All students_to_grade must be student_identifiers present in the roster
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#' @param github_issues logical, whether the grader wants to be given the option to create an issue in students' repos or not (defaults to FALSE)
#' @param issue_every_question logical, whether the possibility to create issues should be given at every question or only at the end of the assignment
#' 
#' @import readr
#' @import dplyr
#' @import stringr
#' @import rstudioapi
#' @import svDialogs
#' @import rmarkdown
#' @import fs
#' 
core_assist_grading <- function(
    rubric_path,
    roster_path,
    temp_grade_sheet_path,
    final_grade_sheet_path,
    example_assignment_path,
    example_feedback_path,
    example_student_identifier,
    missing_assignment_grade = NA,
    questions_to_grade = "all",
    students_to_grade = "all",
    team_grading = FALSE,
    github_issues = FALSE,
    issue_every_question = FALSE
  ) {
  
  # Check example_assignment_path is valid
  if (!is.vector(example_assignment_path)) {
    stop("example_assignment_path must be a single string or a vector of strings")
  } else if (!all(str_detect(example_assignment_path, example_student_identifier))) {
    stop("The example_student_identifier must be present in the example_assignment_path.")
  }
  
  # Check that rubric_path, roster_path and example_assignment_path 
  # point to a file 
  paths_to_files <- c(rubric_path, roster_path, example_assignment_path)
  for (p in paths_to_files) {
    if (!file.exists(p)) {
      stop(paste0("No file exists at ", p, ". ",
                  "Are you sure that this path is correct?"))  
      
    }
  }
  
  # Check that feedback, temporary and final grade sheets paths 
  # include correct directories
  paths_to_write_to <- c(example_feedback_path, 
                         temp_grade_sheet_path,
                         final_grade_sheet_path)
  for (p in paths_to_write_to) {
    if (str_detect(p, "/")) {
      if (!dir.exists(path_dir(p))) {
       stop(paste("This directory seems to be incorrect:",
                  p))
         
      }
    }
  }
  
  # Check that temporary and final grade sheets paths are to .csv files
  temp_file_ext <- path_ext(temp_grade_sheet_path)
  final_file_ext <- path_ext(final_grade_sheet_path)
  if (!all(c(temp_file_ext, final_file_ext) %in% c("csv"))) {
    stop("The extension of temp_grade_sheet_path and final_grade_sheet_path must be .csv")
    
  }
  
  # Check that temporary and final grade sheets paths differ
  if (temp_grade_sheet_path == final_grade_sheet_path) {
    stop("Inputs temp_grade_sheet_path and final_grade_sheet_path need to be different (otherwise one file would overwrite the other).")
    
  } 
    
  # Check example_feedback_path is valid
  feedback_file_ext <- path_ext(example_feedback_path)
  
  if (!(feedback_file_ext %in% c("Rmd", "docx", "html", "pdf"))) {
    stop("The extension of the example_feedback_path must be one of the following: '.Rmd', '.docx', '.html', '.pdf'.")
  } else if (!str_detect(example_feedback_path, example_student_identifier)) {
    stop("The example_student_identifier must be present in the example_feedback_path file name.")
  } 
  
  
  roster <- read_csv(roster_path, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character))
  
  if (sum(colnames(roster) == "student_identifier") != 1) {
    stop("\nThe class roster must only have a column named student_identifier")
    
  } else if (any(duplicated(roster$student_identifier))) {
    stop(paste0(
      "\nThere is at least one student identifier repeated in the class roster.", 
      "\nPlease make sure that the student_identifier is unique to the student."
    ))
    
  } else if (any(is.na(roster$student_identifier))) {
    stop("\nA student_identifier must be provided for every row of the roster.")
    
  }
  
  if (team_grading){
    if(sum(colnames(roster) == "team_identifier") != 1) {
      stop(paste0(
        "\nteam_grading is set to TRUE so there must be a column in the class roster called team_identifier.",
        "\nThis specified which team each student belongs to.",
        "\nThe team_identifier must also be present in the example_assignment_path."
      ))
    } else if (any(is.na(roster$team_identifier))) {
      stop("\nA team_identifier must be provided for every row of the roster.")
      
    }
  }
  
  # Import rubric and create rubric prompts
  rubric_list <- import_rubric(rubric_path)
  rubric_prompts <- create_rubric_prompts(rubric_list)
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
  
  # Create /(load and merge) temporary grade sheet
  temp_grade_sheet <- create_temp_grade_sheet(
    temp_grade_sheet_path = temp_grade_sheet_path,
    example_assignment_path = example_assignment_path, 
    example_feedback_path = example_feedback_path,
    example_student_identifier = example_student_identifier, 
    roster_path = roster_path,
    github_issues = github_issues,
    team_grading = team_grading
  )
  
  # Specify which students to grade
  if (students_to_grade == "all") {
    students_to_grade <- temp_grade_sheet$student_identifier
  }
  
  temp_grade_sheet <- temp_grade_sheet %>% 
    mutate(grade_student = student_identifier %in% students_to_grade)

  continue_grading <- TRUE
  
  # Loop through students, grading each
  for (i in 1:nrow(temp_grade_sheet)) {
    if (continue_grading) {
      # Currently, if a feedback file exists that student does not get graded because their status is changed to feedback created
      all_qs_graded <- temp_grade_sheet$grading_status[i] == "all questions graded"
      
      assignment_should_be_graded <- (
        # Assignment is not missing
        !temp_grade_sheet$assignment_missing[i] &&
        # This student is supposed to be graded this session
        temp_grade_sheet$grade_student[i] &&
        # All questions have not already been graded for this student
        !all_qs_graded
      )
        
      if (assignment_should_be_graded) {
        if (temp_grade_sheet$grading_status[i] == "feedback created") {
          # Print message
          begin_message <- paste(
            paste0(
              "You are going to grade ",
              ifelse(team_grading, yes = "team: ", no = "student: "),
              temp_grade_sheet$student_identifier[i],
              ".\n This assignment was previously partially graded, with the last edit performed at: ",
              temp_grade_sheet$last_time_graded[i],
              ".\n The following questions were already graded: ",
              str_replace_all(
                temp_grade_sheet$graded_qs[i], 
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
              temp_grade_sheet$student_identifier[i],
              "."
            ),
            "Press [enter] or [ok] to continue or [cancel] to stop.",
            sep = "\n"
          )
          
        }
        
        continue_grading <- ok_cancel_box(begin_message)
        
        cat("\n")
        
        if (continue_grading) {
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
          
          temp_obj <- grade_student(
            row = i, 
            temp_grade_sheet = temp_grade_sheet, 
            temp_grade_sheet_path = temp_grade_sheet_path,
            rubric_prompts = rubric_prompts,
            rubric_list = rubric_list,
            rubric_path = rubric_path,
            github_issues = github_issues,
            issue_every_question = issue_every_question,
            questions_to_grade = questions_to_grade
          )
          
          # Close assignment
          for(j in 1:length(assignment_path)) {
            invisible(documentClose(id = doc_id[j], save = FALSE))
          }
          
          # Check if grading has been suspended
          if (is.null(temp_obj)) {

            # Stop grading
            cat(paste0(
              "\nGrading has been suspended.\nTo resume grading at any time keep your temporary grade sheet and rerun the assist grading function."
            ))
            continue_grading <- FALSE
            
          } else {
            temp_grade_sheet <- temp_obj
            
          } 
          
          # Recreate rubric list and prompts, in case they have been modified
          # while grading this student
          rubric_list <- import_rubric(rubric_path)
          rubric_prompts <- create_rubric_prompts(rubric_list)
          
          # To allow enough time between grading of two students,
          # so that Total grade assigned to last student can be printed and seen before
          # the grader is prompted to grade a new student
          Sys.sleep(1)
          
        } 
      }
    
    } 
    
  }

  if (any(temp_grade_sheet$grading_status != "ungraded")) {
    create_final_grade_sheet(
      temp_grade_sheet = temp_grade_sheet, 
      final_grade_sheet_path = final_grade_sheet_path,
      missing_assignment_grade = missing_assignment_grade,
      rubric_list = rubric_list,
      rubric_prompts = rubric_prompts,
      team_grading = team_grading
    )
  }
  
  # Remind that the rubric has changed
  rubric_list <- import_rubric(rubric_path)  
  updated_rubric <- !(identical(original_rubric_list, rubric_list))
  
  if (updated_rubric == TRUE) {
    rubric_updated_message <- paste(
      "The rubric has been updated.",
      "You will have to share the updated rubric with your teaching team,",
      "if you would like changes to be reflected on their end."
    )
    dlg_message(rubric_updated_message, type = "ok")
    
  }
  
  some_students_graded <- any(temp_grade_sheet$grading_status != "ungraded")
  
  # if (github_issues == TRUE  && some_students_graded) {
  #   push_feedback_issue_message <- 
  #     "To push feedback files and create issues in GitHub remember to run push_to_github()."
  #   dlg_message(push_feedback_issue_message, type = "ok")
  # } 
   
  if (feedback_file_ext %in% c("docx", "html", "pdf") && some_students_graded) {
    
    # Let the user know that feedback is being knitted
    cat(paste("Trying to knit feedback files to", feedback_file_ext, "format..."))
    
    # Try to render feedback files
    tryCatch(               
      # Specifying expression
      expr = {              
        paths_returned <- mapply(
          render,
          temp_grade_sheet$feedback_path_Rmd[temp_grade_sheet$grading_status != "ungraded"],
          MoreArgs = list(clean = TRUE, quiet = TRUE)  
        )
        
        cat("succeeded!")
        
        unlink(temp_grade_sheet$feedback_path_Rmd)
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
