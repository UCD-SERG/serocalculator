## Resubmission

This is a third resubmission of the package `serocalculator` version 1.0.3.

### Changes in this version

#### Response to previous CRAN comments:

1. **Comment 1: Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar) Missing Rd-tags: fdev.Rd and llik.Rd**

  - **Response**: Those two functions (fdev and llik) are just wrappers providing legacy support for functions that have been renamed for  a more consistent API, and their documentation is therefore minimal. The input parameters and return values are documented in the manual pages for the new function names, which are now hyperlinked from the manual pages for the old function names

  - **Specific Changes**
      - hyperlinked deprecated fdev.Rd to the updated f_dev.Rd
      - hyperlinked deprecated llik.Rd to the updated log_likelihood.Rd
   
## Notes

* Auto-check will issue a warning about misspelled words in DESCRIPTION, but the  words flagged are all correct and are specific to our scientific field. 
* Examples using \donttest are not able to run in <5 sec. These examples are complex or load data from an online repository. 


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

