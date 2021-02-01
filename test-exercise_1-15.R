# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 1-15")
library("lintr")
library("schoolmath")

#source("exercise-test-utils.R")

testEnv <- new.env()
file_name <- 'exercise_1-15.R'
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

test_that("Define `num` variable", {
  expect_has_variable("num", var_type="double", envir=testEnv)
})

test_that("Check values for `num`", {
  num <- get_var_for_test('num', testEnv)
  if ((num >= 1) & (num <= 20)) {
    succeed()
  } else {
    msg <- sprintf('The value of your `num` variable (%.4f) is not between 1 and 20', num)
    return(fail(msg))
  }
})

test_that("Define `greater_than_12` variable", {
  expect_has_variable("greater_than_12", var_type="logical", envir=testEnv)
})

test_that("Check values for `num`", {
  num <- get_var_for_test('num', testEnv)
  greater_than_12 <- get_var_for_test('greater_than_12', testEnv)
  check = num > 12 
  if (check == greater_than_12) {
    succeed()
  } else {
    msg <- sprintf('The value of your `greater_than_12` variable (%s) is incorrect.', greater_than_12)
    return(fail(msg))
  }
})

test_that("Define `between_5_and_15` variable", {
  expect_has_variable("between_5_and_15", var_type="logical", envir=testEnv)
})

test_that("Check values for `between_5_and_15`", {
  num <- get_var_for_test('num', testEnv)
  between_5_and_15 <- get_var_for_test('between_5_and_15', testEnv)
  check = (num > 5) & (num < 15)
  if (check == between_5_and_15) {
    succeed()
  } else {
    msg <- sprintf('The value of your `between_5_and_15` variable (%s) is incorrect.', between_5_and_15)
    return(fail(msg))
  }
})

test_that("Define `rounded_num` variable", {
  expect_has_variable("rounded_num", var_type="double", envir=testEnv)
})

test_that("Check values for `rounded_num`", {
  num <- get_var_for_test('num', testEnv)
  rounded_num <- get_var_for_test('rounded_num', testEnv)
  check <- round(num)
  if (check == rounded_num) {
    succeed()
  } else {
    msg <- sprintf('The value of your `rounded_num` variable (%.4f) is incorrect. Did you use the round() function?', rounded_num)
    return(fail(msg))
  }
})

test_that("Define `is_odd` function", {
  expect_has_variable("is_odd", var_type="closure", envir=testEnv)
})

test_that("Test `is_odd` function", {
  is_odd <- get_var_for_test('is_odd', testEnv)
  check1 <- is_odd(2)
  check2 <- is_odd(3)
  check3 <- is_odd(3.5)
  check4 <- is_odd(0)
  if (!check1 & check2 & !check3 & !check4) {
    succeed()
  } else {
    msg <- 'The output from your `is_odd` function in incorrect. Are you using the modulo operator (%%)?'
    return(fail(msg))
  }
})

test_that("Define `between_7_and_13` function", {
  expect_has_variable("between_7_and_13", var_type="closure", envir=testEnv)
})

test_that("Check `between_7_and_13` outputs", {
  between_7_and_13 <- get_var_for_test('between_7_and_13', testEnv)
  check1 <- between_7_and_13(10)
  check2 <- between_7_and_13(3)
  check3 <- between_7_and_13(7)
  check4 <- between_7_and_13(13)
  if (check1 & !check2) {
    if (check3 & check4) {
      succeed()
    } else {
      msg <- 'The output from your `between_7_and_13` function is incorrect. Remember - we are checking for between 7 and 13 (INCLUSIVE)'
      return(fail(msg))
    }
  } else {
    msg <- 'The output from your `between_7_and_13` function is incorrect. Are you using the correct operators (< or >)?'
    return(fail(msg))
  }
})

test_that("Define `odd_and_between_7_and_13` function", {
  expect_has_variable("odd_and_between_7_and_13", var_type="closure", envir=testEnv)
})

test_that("Check `odd_and_between_7_and_13` outputs", {
  odd_and_between_7_and_13 <- get_var_for_test('odd_and_between_7_and_13', testEnv)
  check1 <- odd_and_between_7_and_13(10) == "The number is between 7 and 13 but not odd"
  check2 <- odd_and_between_7_and_13(3) == "The number is odd but not between 7 and 13"
  check3 <- odd_and_between_7_and_13(0) == "The number is neither odd nor between 7 and 13"
  check4 <- odd_and_between_7_and_13(7) == "The number is both odd and between 7 and 13"
  check5 <- odd_and_between_7_and_13(13) == "The number is both odd and between 7 and 13"
  if (check1 & check2 & check3 & check4 & check5) {
    succeed()
  } else {
    msg <- 'The output from your `odd_and_between_7_and_13` function is incorrect. Are you using your previously defined functions?'
    return(fail(msg))
  }
})

test_that("Define `test_1` variable", {
  expect_has_variable("test_1", var_type="character", envir=testEnv)
})

test_that("Define `test_2` variable", {
  expect_has_variable("test_2", var_type="character", envir=testEnv)
})

test_that("Check `test_1` and `test_2` values", {
  test_1 <- get_var_for_test('test_1', testEnv)
  test_2 <- get_var_for_test('test_2', testEnv)
  odd_and_between_7_and_13 <- get_var_for_test('odd_and_between_7_and_13', testEnv)
  rounded_num <- get_var_for_test('rounded_num', testEnv)
  check1 <- test_1 == odd_and_between_7_and_13(rounded_num)
  check2 <- test_2 == odd_and_between_7_and_13(rounded_num + 1)
  if (check1) {
    if (check2) {
      succeed()
    } else {
      msg <- sprintf('The value of your `test_2` variable ("%s") is incorrect for `rounded_num` + 1 (%.4f) as an input. Did you use `rounded_num + 1` as the input?', test_2, rounded_num + 1)
      return(fail(msg))
    }
  } else {
    msg <- sprintf('The value of your `test_1` variable ("%s") is incorrect for `rounded_num` (%.4f) as an input. Did you use `rounded_num` as the input?', test_1, rounded_num + 1)
    return(fail(msg))
  }
})

test_that("Define `word` variable", {
  expect_has_variable("word", var_type="character", envir=testEnv)
})

test_that("Define `longer_than_8` variable", {
  expect_has_variable("longer_than_8", var_type="logical", envir=testEnv)
})

test_that("Check `longer_than_8` value", {
  longer_than_8 <- get_var_for_test('longer_than_8', testEnv)
  word <- get_var_for_test('word', testEnv)
  if ((nchar(word) > 8) == longer_than_8) {
    succeed()
  } else {
    msg <- sprintf('The value of your `longer_than_8` variable ("%s") is incorrect for the word you chose ("%s")', longer_than_8, word)
    return(fail(msg))
  }
})

test_that("Define `compare_str_length` function", {
  expect_has_variable("compare_str_length", var_type="closure", envir=testEnv)
})

test_that("Check `compare_str_length` value", {
  compare_str_length <- get_var_for_test('compare_str_length', testEnv)
  check1 <- compare_str_length('thisStringLarger', 'smaller') == 'The first string is longer than the second by 9'
  check2 <- compare_str_length('small', 'thisStringLarger') == 'The second string is longer than the first by 11'
  check3 <- compare_str_length('equalStrings', 'equalStrings') == "The strings are equal in length"
  if (check1 & check2 & check3) {
    succeed()
  } else {
    msg <- 'The output from your `compare_str_length` function is incorrect. Are you returning the correct string?'
    return(fail(msg))
  }
})