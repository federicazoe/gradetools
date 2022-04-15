#' Writes final grade sheet csv
#'
#' @param temp_grade_sheet data frame; The temp_grade_sheet is a data frame containing information for gradetools internal use 
#' @param final_grade_sheet_path string; path to save final grade sheet to. Must be a .csv
#' @param missing_assignment_grade numeric; The grade to assign a student with no assignment submission
#' @param rubric_prompts list of prompts; One prompt for each question plus one for overall feedback. This is produced by create_rubric_prompts
#' @param rubric_list list whose format corresponds to rubric_list, which is used by most functions in this package. This is produced by import_rubric
#' @param team_grading logical, indicates if any assignment submission is associated with multiple students (e.g. team projects)
#' 
#' @importFrom svDialogs dlg_message
#' @importFrom readr write_file
#' @import dplyr
#'
create_final_grade_sheet <- function(
    temp_grade_sheet,
    final_grade_sheet_path,
    missing_assignment_grade,
    rubric_list,
    rubric_prompts,
    team_grading
  ) {
  
  final_grade_sheet <- temp_grade_sheet %>% 
    mutate(assignment_missing = as.logical(assignment_missing)) %>% 
    mutate(grade_student = as.logical(grade_student)) %>% 
    mutate(grade = NA) %>% 
    mutate(grade_decomposition = NA)
  
  # Final grade is only created for students without grade status "ungraded"
  no_assignment_missing <- all(temp_grade_sheet$assignment_missing == FALSE)
  all_assignments_fully_graded <- all(
    temp_grade_sheet$grading_status == "all questions graded"
  )
  
  for (j in 1:nrow(temp_grade_sheet)) {
    # Get final grade and grade_decomposition and write feedback
    if (temp_grade_sheet$grading_status[j] != "ungraded") {
      grade_info <- assign_grade_write_feedback(
        temp_grade_sheet[j, ],
        rubric_list = rubric_list,
        rubric_prompts = rubric_prompts
      )
    }
    
    if (temp_grade_sheet$grading_status[j] == "all questions graded") {
      final_grade_sheet$grade[j] <-  grade_info$grade
      
      final_grade_sheet$grade_decomposition[j] <- paste(
        grade_info$grade_decomposition,
        collapse = " & "
      )
      
    } else if (temp_grade_sheet$assignment_missing[j]) {
      final_grade_sheet$grade[j] <- missing_assignment_grade
      
    }
    
  }
  
  final_grade_sheet <- final_grade_sheet %>% 
    select(-c(
      feedback_path_Rmd, 
      feedback_path_to_be_knitted,
      assignment_path,
      assignment_missing,
      grading_status, 
      feedback_codes,
      graded_qs,
      last_time_graded,
      grade_student,
      comments,
      comment_qs
    ))
  
  if (team_grading) {
    brief_roster <- final_grade_sheet %>% 
      select(student_identifier, students_in_team)
    
    split_student_ids <- str_split(
      final_grade_sheet$students_in_team, 
      pattern = "&&&"
    )
    
    brief_roster <- data.frame(
      "team_identifier" = rep(
        brief_roster$student_identifier, 
        unlist(lapply(split_student_ids, FUN = length))
      ),
      "student_identifier" = unlist(split_student_ids)
    )
    
    final_grade_sheet <- final_grade_sheet %>% 
      select(-students_in_team) %>% 
      rename(team_identifier = student_identifier) %>% 
      full_join(brief_roster, by = "team_identifier")
    
  }
  
  if(file.exists(final_grade_sheet_path)) {
    unlink(final_grade_sheet_path)
  }
  
  write_csv(final_grade_sheet, final_grade_sheet_path)
  
  ungraded_message <- paste(
    "\nNot all questions have been graded, or some students have an assignment missing!",
    "To finish grading just rerun the assist grading function.\n",
    sep = "\n"
  )
  
  finished_message <- paste(
    "A final grade sheet has been created which contains the overall grade as well as the grade decomposed by points per question.",
    "The final gradesheet will assign students a score of NA if their assignment is not completely graded.",
    sep = "\n"
  )
  
  if (!no_assignment_missing || !all_assignments_fully_graded) {
    finished_message <- paste(ungraded_message, finished_message, sep = "\n\n")
  }
  
  dlg_message(finished_message, type = "ok")
}
