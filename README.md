# gradetools <img src='man/figures/gradetools-logo.png' align="right" width="200" alt="a hex shaped logo with a cat in front of a laptop sending out paper planes and a notepad with A+ on it. The logo reads gradetools"/>

**gradetools** is created with data science instructors in mind.  Assessing students with open-ended assignments (e.g. projects, interpretation questions) can be impactful but can also be arduous when it comes to grading. 
gradetools supports grading assessments for which automated feedback is not possible.

gradetools aims to do two major things:

1. Automate the grading workflow not the feedback. 
2. Minimize switching between different interfaces (grade book, student's work, GitHub etc.)

The functions in gradetools allow the grader to:

- efficiently grade assignments in several formats (e.g. .R, .Rmd, .py, .txt);
- specify the rubric and modify it dynamically while grading;
- save a final grade sheet with all assigned grades;
- save a personalized feedback file for each student;
- grade both individual and team-based assignments;
- regrade specified students and questions;
- push feedback and create GitHub issues into students' GitHub repos. 

The package has only been tested with RStudio GUI.

## Vignettes

Throughout the vignettes we provide example grading scenarios that you can use to test the functions. There are currently four vignettes available. 

1. [How to grade with gradetools](https://federicazoe.github.io/gradetools/articles/a-grading-with-gradetools.html): For beginner users we recommend starting with this vignette.
2. [How to regrade assignments with gradetools](https://federicazoe.github.io/gradetools/articles/b-regrading-with-gradetools.html): This vignette shows how to change previously assigned grades.
3. [Extended gradetools Capability: Team Grading](https://federicazoe.github.io/gradetools/articles/c-extended-capability-teams.html): This vignette is only relevant for grading assignments that are team-based.
4. [Extended gradetools Capability: Assignments on GitHub](https://federicazoe.github.io/gradetools/articles/d-extended-capability-github.html): This vignette is only relevant for assignments that are on GitHub.
5. [Comprehensive example of grading with gradetools](https://federicazoe.github.io/gradetools/articles/e-comprehensive-example.html): This is a showcase of most of gradetools' capabilities.

## Workshop

You can watch <a href="https://www.youtube.com/watch?v=ECI4CzkUqak" target="_blank"> this tutorial </a> to learn more about what gradetools was created for and how to use it.

## Installation

You can install the development version from GitHub. You would also need to install the devtools package if you do not have it installed already.

``` r
#install.packages
devtools::install_github("federicazoe/gradetools")
```

## Support

<table>
  <tr style="text-align: left"> 
    <td> <img src="man/figures/nsf-logo.png" align="center" alt="HPI logo" width="120" /> </td>
    <td style="text-align: left"> <br> <br> Part of this work is supported by NSF HDR DSC award #2123366. </td>
  </tr> 
  <tr></tr>
  <tr style="text-align: left"> 
    <td> <img src="man/figures/hpi-logo.jpeg" align="center" alt="NSF logo" width="120" /> </td>
    <td style="text-align: left"> This work is also supported by the HPI Research Center in Machine Learning and Data Science at UC Irvine. </td>
  </tr>   
</table>

## Citation

Ricci, Federica Zoe, Catalina Mari Medina, and Mine Dogucu. "Automated grading workflows for providing personalized feedback to open-ended data science assignments." Technology Innovations in Statistics Education (2024).

``` latex
@article{,
  title = {Automated grading workflows for providing personalized feedback to open-ended data science assignments},
  author = {Ricci, Federica Zoe and Medina, Catalina Mari and Dogucu, Mine},
  journal = {Technology Innovations in Statistics Education},
  year = {2024}
}
```
