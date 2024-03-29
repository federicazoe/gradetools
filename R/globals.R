utils::globalVariables(c(
  "student_identifier",          # colname in grading_progress_log
  "students_in_team",            # colname in grading_progress_log
  "grading_status",              # colname in grading_progress_log
  "feedback_codes",              # colname in grading_progress_log
  "graded_qs",                   # colname in grading_progress_log
  "last_time_graded",            # colname in grading_progress_log
  "comments",                    # colname in grading_progress_log
  "comment_qs",                  # colname in grading_progress_log
  "issue_qs",                    # colname in grading_progress_log         
  "issue_titles",                # colname in grading_progress_log
  "issue_bodies",                # colname in grading_progress_log
  "issue_pushed",                # colname in grading_progress_log
  "feedback_pushed",             # colname in grading_progress_log
  "github_repo",                 # colname in grading_progress_log
  "name",                        # colname in grading_progress_log
  "team_identifier",             # colname in grading_progress_log
  "feedback_path_Rmd",           # colname in grading_progress_log
  "feedback_path_to_be_knitted", # colname in grading_progress_log
  "assignment_path",             # colname in grading_progress_log
  "assignment_missing",          # colname in grading_progress_log
  "grade_student",                # colname in grading_progress_log
  "questions_to_regrade",        # argument in assist_regrading()
  "grading_progress_log_path",   # argument in core_assist_grading()
  "points_to_remove"             # colname in rubric
))