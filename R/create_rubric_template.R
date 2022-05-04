#' Creates rubric template csv file
#' 
#' @details The rubric must be specified in the following way: 
#'     The column names generated must be left the same. 
#'     Each row of the rubric will contain the details for one rubric item for one question. 
#'     Every question to be graded must have at least one rubric item associated.
#'     The entries in the 'name' column must be 'general_feedback', 'all_questions', or the name of the question that the rubric item is specified for.
#'     Rubric items with 'name' specified as 'all_questions' are rubric items available for all assignment questions. There should be no total_points value for these rubric items. The points_to_remove or points_to_add values will be taken as percentages (e.g. If points_to_remove for an all_questions rubric item is specified as 25, when the grader chooses this grade rubric option while grading 25% of the total_points for that question will be deducted.).
#'     Rubric items with 'name' specified as 'general_feedback' are rubric items which the user will be prompted for after they have completely graded an assignment. These rubric items do not have any points attached, their purpose is to leave a general message to the student after grading has been completed.
#'     All rubric prompt codes must begin with a digit, contain no spaces, and be unique within a question. 
#'     Rubric prompt messages are brief summaries to remind the user of the appropriate situation to apply that rubric item.
#'     When the user is grading they will see see all rubric items for that question and 'all_questions' rubric items. The corresponding prompt codes points to remove/add and prompt message will be displayed.
#'     The 'feedback' column contains the message that will be printed in the student's feedback file if that rubric item is selected.
#'
#' @param rubric_path string; path used to save the rubric template. This must be a csv file.
#' @param negative_grading Boolean; TRUE indicates the assignment will use negative grading instead of positive.
#' @param create_example_rubric Boolean; TRUE indicates an example rubric will also be created.
#' @param example_rubric_path string; path used to save the example rubric if create_example_rubric is TRUE. This must be a csv file.
#'
#' @export
#' @import dplyr
#' @importFrom utils write.csv

create_rubric_template <- function(
    rubric_path,
    negative_grading = TRUE, 
    create_example_rubric = FALSE,
    example_rubric_path = NULL  
  ) {
  
  if (create_example_rubric) {
    if (is.null(example_rubric_path)) {
      stop("The example_rubric_path must be provided to create the example rubric.")
    } else if (fs::path_ext(example_rubric_path) != "csv") {
      stop("The example_rubric_path must be a csv file.")
    }
  }
  
  if (fs::path_ext(rubric_path) != "csv") {
    stop("The rubric_path must be a csv file.")
  }
  
  rubric_template <- data.frame(
    name = "", 
    total_points = "", 
    prompt_code = "", 
    prompt_message = "", 
    feedback = "", 
    points_to_remove = ""
  )
  
  if (create_example_rubric) {
    example_rubric <- data.frame( 
      name = c(
        rep("question_1", 1),
        rep("question_2", 1),
        rep("question_3a", 3),
        rep("question_3b", 2),
        "all_questions",
        "general_feedback"
      ),
      prompt_code = c(
        "a",
        "a",
        "1", "1a", "1b",
        "1", "2",
        "0",
        "*"
      ),
      prompt_message = c(
        "Incorrect multiple choice option",
        "Incorrect multiple choice option",
        "Both hypotheses are incorrect",
        "The null hypothesis is incorrect",
        "The alternative hypothesis is incorrect",
        "Error in computing the p-value",
        "Incorrect interpretation of p-value",
        "Correct",
        "Great job on the assignment!"
      ),
      feedback = c(
        "The correct choice was option b. $\\beta_0$ is the regression intercept.",
        "The correct choice was option c. $\\beta_1$ is the regression slope.",
        "The hypotheses should be $H_0:\\beta_1 = 0$ and $H_A:\\beta_1\\ne0$.",
        "The null hypothesis should be $H_0:\\beta_1 = 0$. This hypothesis, that the slope of the simple regression is zero, means that there is no linear relationship between the predictor and the response.",
        "The alternative hypothesis should be $H_0:\\beta_1 \\ne 0$. This hypothesis, that the slope of the simple regression is not zero, means that there is a linear relationship between the predictor and the response.",
        "There appears to be an error in the computation of the p-value. Please refer to the homework solution.",
        "The p-value is the probability of observing a test statistic as or more extreme as that observed, given the null hypothesis is correct. Since the p-value was extremely low, we found strong evidence against the hypothesis of no linear relationship.",
        "Your solution is correct.",
        "Great job on the assignment!"
      ),
      total_points = c(
        "2", 
        "2",
        "3", "", "",
        "5", "",
        "",
        ""
      ),
      points_to_remove = c(
        "2", 
        "2",
        "3", "1.5", "1.5",
        "1", "2",
        "0",
        ""
      )
    )
  
    utils::write.csv(
      example_rubric, 
      file = example_rubric_path,
      row.names = FALSE
    )
    
  }
  
  if (!negative_grading) {
    rubric_template <- rename(rubric_template, points_to_add = points_to_remove)
  }
  
  utils::write.csv(
    rubric_template, 
    file = rubric_path,
    row.names = FALSE
  )
}
