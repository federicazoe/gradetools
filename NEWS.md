# gradetools 0.2.0 

## Minor changes  

- Added ability to leave a personalized comment in a student's feedback file without any grade associated
- Prompting is neater 
    - prompt codes starting with digits are user made
    - letter prompt options are options provided by gradetools (e.g. adding rubric items, noting issues, and providing personalized feedback)
- Feedback files can now have the following extensions: 
    - Rmd
    - md (for GitHub documents)
    - docx (for Word documents)
    - html
    - pdf
- Moved `questions_to_grade` argument before `students_to_grade` in `assist_team_grading()`, and `assist_advanced_grading()`.
- Moved `questions_to_regrade` argument before `students_to_regrade` in `assist_grading()`
- temporary grade sheet is now called the grading progress log