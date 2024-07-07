## Resubmission

This is a second resubmission of the package `serocalculator` version 1.0.2.

### Changes in this version

#### Response to previous CRAN comments:

1. **Comment 1: Please only write package names, software names and API (application programming interface) names in single quotes in title and description.**
  - **Response**: Removed quotes except for package names. Confirmed that flagged words are spelled correctly. 
  - **Specific Changes**
    - Removed single quotes
    - Replaced directed quotes around package names with single quotes
    
2. **Comment 2: If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file**
  - **Response**: Reviewed references to methods in DESCRIPTION file
  - **Specific Changes**: Updated references using the format authors (year) <doi:...> in DESCRIPTION
  
3. **Comment 3: It seems like you have too many spaces in your description field.**
  - **Response**: Reviewed spacing in DESCRIPTION
  - **Specific Changes**: Removed additional linebreaks from DESCRIPTION
  
4. **Comment 4: We see : Unexecutable code in man/autoplot.curve_params.Rd: `%>%:`**
  - **Response**: We thoroughly reviewed the code in man/autoplot.curve_params.Rd and couldn't reproduce the reported issue. We fixed what we though could be the issue and ensured that the code is correct and functional.
  - **Specific Changes**: Reformatted code to avoid pipes in naming
  
5. **Comment 5: Please add `\value` to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too**
  - **Response**: Reviewed functions without a return value
  - **Specific Changes**: Added \value and output description to the following files:
    - print.seroincidence.by.Rd
    - print.seroincidence.Rd
    - print.summary.seroincidence.by.Rd
    
6. **Comment 6: Using `foo:::f` instead of `foo::f` allows access to unexported objects. This is generally not recommended, as the semantics of unexported objects may be changed by the package author in routine maintenance. Please omit one colon.**
  - **Response**: Reviewed all instances of three colons
  - **Specific Changes**: Updated the following files by removing one colon:
    - df_to_array.Rd
    - df.to.array.Rd
    - plot_curve_params_one_ab.Rd
    
7. **Comment 7: You have examples for unexported functions. Please either omit these examples or export these functions.* -> Warning: Examples for unexported function**
  - **Response**: Removed examples from unexported functions
  - **Specific Changes**: Removed examples from 
    -  sim.cs() in warn.missing.strata.Rd

8. **Comment 8:  `\dontrun{}` should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in `\dontrun{}` adds the comment ("# Not run:") as a warning for the user. Does not seem necessary.**
  - **Response**: Reviewed use of `\dontrun` vs `\donttest` and clarified with CRAN reviewer
  - **Specific Changes**: Replaced `\dontrun` with `\donttest` and noted that this was used because examples could not run in <5 sec

9. **Comment 9: You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.**
  - **Response**: Reviewed affected files and made edits where we could. We could not find this issue in R/par.pred.R. We intentionally use print() in R/warn_missing_strata.R because we want to print a dataframe to the console to inform users about which pieces of data are broken. This forms part of an error message and is followed immediately by a stop()
  - **Specific Changes**: Deleted function get_additional_data.R.
  
10. **Comment 10: Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited.**
  - **Response**: Reviewed instances where user options are changed, or there is example code for updating these examples.
  - **Specific Changes**: Deleted function get_additional_data.R.


   

## Notes

* Auto-check will issue a warning about misspelled words in DESCRIPTION, but the  words flagged are all correct and are specific to our scientific field. 
* Examples using \donttest are not able to run in <5 sec. These examples are complex or load data from an online repository. 


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

