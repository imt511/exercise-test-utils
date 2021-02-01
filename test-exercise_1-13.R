# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 1-13")
library("lintr")
library("schoolmath")

#source("exercise-test-utils.R")

testEnv <- new.env()
file_name <- 'exercise_1-13.R'
file_path <- paste0("../../", file_name)

# has side-effect of running script; returns first error that occurs
loadSource <- function() {
  tryCatch({
    # execute the source file. Blockes printed output, variables scoped to testEnv
    utils::capture.output(source(file_path, testEnv))
    return(NULL)
  }, error = function(e) { # catch any errors from running source and return them as result
    #warning(paste("Exercise script failed to run: ", e$message))
    # stop(e$message) # crash -- or do I just give an error message?
    return(e$message) # global to check
    # print(e$message) # okay, we can check for errors. Good.
  })
}

my_linters <- with_defaults(commented_code_linter = NULL, # can comment out code
  #extraction_operator_linter # will this block $ usage?
  implicit_integer_linter = NULL, # don't need 1L
  infix_spaces_linter = NULL, # 1+1 is fine
  line_length_linter = line_length_linter(160), # put a higher, but existing limit
  object_length_linter = object_length_linter(50), # put a higher, but existing limit on var names
  seq_linter = NULL, # we use length() and nrow()
  single_quotes_linter = NULL, # double quotes are fine
  spaces_inside_linter = NULL, # can have spaces inside () and []
  spaces_left_parentheses_linter = NULL,
  todo_comment_linter = NULL,
  trailing_blank_lines_linter = NULL, # don't care
  trailing_whitespace_linter = NULL # don't care
  #undesirable_function_linter = NULL, # what are undesirable functions??
)


### GENERIC TESTS ###

test_that("Script file should execute without errors", {
  errors <- loadSource()
  #expect_null(errors, paste("Encountered errors when executing files: ", errors))
  expect(is.null(errors), paste("Encountered execution error in script: ", errors))
})

test_that("Code should be written in proper style", {
  # this only seems to be linting the text file, not the exercise file, no matter what I do...
  #expect_lint_free(linter = my_linters)
  #expect_lint_free(filename = "../../exercise.R", exclusions = list("test-exercise.R"))

  lint_results = lint(filename = file_path, linter = my_linters)
  if(length(lint_results) > 0) {
    print(lint_results) # makes show up in Markers panel
  }

  # need custom expectation here
  expect(length(lint_results) == 0, paste("Found", length(lint_results), "style mistakes. See the 'Markers' panel for details."))
})

### EXERCISE TESTS ###

expect_has_variable <- function(object, var_type=NULL, var_value=NULL, extra_msg="", envir=pos.to.env(-1L)){
  # capture object and label ?? This is actual expectation object?
  act <- quasi_label(rlang::enquo(object), arg = "object")
  
  message <- NULL
  var_value <- NULL
  
  # do the expect()
  act$exists <- exists(act$val, envir = envir) # do the work
  if(!act$exists){
    message <- sprintf("Variable `%s` is not declared. Did you name it correctly? Case matters!", act$val)
  } else {
    var_value = get(act$val, envir)
  }
  if(!is.null(var_type) & !is.null(var_value)){
    if(typeof(var_value) != var_type){
      message <- sprintf("Variable `%s` should be of type %s, not %s", act$val, var_type, typeof(var_value))
    }
  }
  if(is.null(message)) { # if haven't found an error
    succeed()
    return(invisible(act$val))
  } else { # finally
    fail(sprintf("%s %s", message, extra_msg))
  }
}

# runs capture.output and returns result, will fail() test on error
get_output_for_test <- function(source_file, envir, should.stop=FALSE){
  tryCatch({
    # execute the source file. Blocks printed output, variables scoped to testEnv
    output <- utils::capture.output(source(source_file, envir))
    return(output)
  }, error = function(e) { # catch any errors from running source and return them as result
    msg <- "Error when executing source file; unable to capture output."
    if(should.stop){ stop(msg) } else { fail(msg) }
    return(NA)
  })
}

# accesses the variable from the environment, will fail() test on error
get_var_for_test <- function(var_name, envir, should.stop=FALSE){
  tryCatch({
    actual <- get(var_name, testEnv)
    return(actual)
  }, error = function(e) {
    msg <- sprintf("Unable to read value `%s`. Make sure the variable has been defined!", var_name)
    if(should.stop){ stop(msg) } else { fail(msg) }
    return(NA)
  })
}

expect_printed <- function(varName){
  output <- get_output_for_test(file_path, testEnv)
  if(length(output) < 1) {
    return(fail(paste("No printed output. Need to print out the", varName, "variable.")))
  } else {
    varOfInterest <- get_var_for_test(varName, testEnv)
    if(is.na(varOfInterest)){
      return() # don't continue if we couldn't read
    } else if(!grepl(varOfInterest, output[1], fixed=TRUE)) {
      msg <- sprintf(paste('The value of your', varName, 'variable (%s) not found in printed output (%s)'), varOfInterest, substring(output[1], 5))
      return(fail(msg))
    } else {
      succeed()
    }
  }
}

expect_printed_at_loc <- function(toPrint, loc){
  output <- get_output_for_test(file_path, testEnv)
  if(length(output) < 1) {
    return(fail(paste("No printed output. Need to print ", toPrint, "variable.")))
  } else {
      if(!grepl(toPrint, output[loc], fixed=TRUE)) {
      msg <- sprintf(paste(toPrint, 'was not found in printed output (%s)'), substring(output[loc], 5))
      return(fail(msg))
    } else {
      succeed()
    }
  }
}

test_that("Define `add_three` function", {
  expect_has_variable("add_three", var_type="closure", envir=testEnv)
})


test_that("Function `add_three` works as expected", {
  add_three <- get_var_for_test("add_three", testEnv)
  check1 <- add_three(1)
  check2 <- add_three(-5)
  check3 <- add_three(0)
  if((check1 == 4) & (check2 == -2) & (check3 == 3)) {
    succeed()
  } else {
    msg <- 'The output from your `add_three` function is incorrect. Are you adding 3 to the argument?'
    return(fail(msg))
  }
})

test_that("Define `imperial_to_metric` function", {
  expect_has_variable("imperial_to_metric", var_type="closure", envir=testEnv)
})

test_that("Function `imperial_to_metric` works as expected", {
  imperial_to_metric <- get_var_for_test("imperial_to_metric", testEnv)
  imperial_to_metric_check <- function(feet, inches) {
    return(((feet * 12) + inches) * 0.0254)
  }
  bool1 <-  imperial_to_metric(0, 0) == imperial_to_metric_check(0, 0)
  bool2 <-  imperial_to_metric(10, 0) == imperial_to_metric_check(10, 0)
  bool3 <-  imperial_to_metric(0, 10) == imperial_to_metric_check(0, 10)
  bool4 <-  imperial_to_metric(5, 10) == imperial_to_metric_check(5, 10)
  
  if(bool1 & bool2 & bool3 & bool4) {
    succeed()
  } else {
    msg <- 'The output from your `imperial_to_metric` function is incorrect. Are you using the correct conversions?'
    return(fail(msg))
  }
})

test_that("Define `diff_str_length` function", {
  expect_has_variable("diff_str_length", var_type="closure", envir=testEnv)
})

test_that("Function `diff_str_length` works as expected", {
  diff_str_length <- get_var_for_test("diff_str_length", testEnv)
  compare_str_length_check <- function(string1, string2) {
    nchar_1 <- nchar(string1)
    nchar_2 <- nchar(string2)
    return(abs(nchar_1 - nchar_2))
  }
  check1 <- diff_str_length('thisStringLarger', 'smaller')
  if(is.numeric(check1)) {
    bool1 <- diff_str_length('thisStringLarger', 'smaller') == compare_str_length_check('thisStringLarger', 'smaller')
    bool2 <- diff_str_length('smaller', 'thisStringLarger') == compare_str_length_check('smaller', 'thisStringLarger')
    bool3 <- diff_str_length('equalStrings', 'equalStrings') == compare_str_length_check('equalStrings', 'equalStrings')
    
    if(bool1 & bool2 & bool3) {
      succeed()
    } else {
      msg <- 'The output from your `diff_str_length` function is incorrect. Are you returning the correct output?'
      return(fail(msg))
    }
  } else {
    msg <- 'Your `diff_str_length` function does not output a numeric data type.'
    return(fail(msg))
  }
})

test_that("Define `fraction_str` function", {
  expect_has_variable("fraction_str", var_type="closure", envir=testEnv)
})

test_that("Function `fraction_str` works as expected", {
  fraction_str <- get_var_for_test("fraction_str", testEnv)
  fraction_str_check <- function(numerator = 1, denominator = 1) {
    return(paste(numerator, '/', denominator))
  }
  bool1 <- fraction_str() == fraction_str_check()
  bool2 <- fraction_str(numerator = 2) == fraction_str_check(numerator = 2)
  bool3 <- fraction_str(denominator = 2) == fraction_str_check(denominator = 2)
  
  if(bool1 & bool2 & bool3) {
    succeed()
  } else {
    msg <- 'The output from your `fraction_str` function is incorrect. Did you set the correct default values?'
    return(fail(msg))
  }
})