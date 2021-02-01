# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 2-4")
library("lintr")
library("schoolmath")

#source("exercise-test-utils.R")

testEnv <- new.env()
file_name <- 'exercise_2-4.R'
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

expect_printed_at_loc <- function(toPrint, loc=1){
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

test_that("Define `phone_number` variable", {
  expect_has_variable("phone_number", var_type="double", envir=testEnv)
})

test_that("Check value of phone number", {
  phone_number <- get_var_for_test("phone_number", testEnv)
  if(identical(c(8, 6, 7, 4, 3, 0, 9), phone_number)) {
    succeed()
  } else {
    msg <- sprintf("Your `phone_number` vector (%s) is not (8, 6, 7, 4, 3, 0, 9)", paste(phone_number, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Print the phone `number` variable in the correct form", {
  expect_printed_at_loc("867-4309")
})

test_that("Define `phone_number_2` variable", {
  expect_has_variable("phone_number_2", var_type="double", envir=testEnv)
})

test_that("Check value of phone_number_2", {
  phone_number_2 <- get_var_for_test("phone_number_2", testEnv)
  if(identical(c(8, 6, 7, 4, 3, 2, 9), phone_number_2)) {
    succeed()
  } else {
    msg <- sprintf("Your `phone_number_2` vector (%s) is not (8, 6, 7, 4, 3, 2, 9)", paste(phone_number_2, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `get_middle_value` function", {
  expect_has_variable("get_middle_value", var_type="closure", envir=testEnv)
})

test_that("Check `get_middle_value` outputs", {
  get_middle_value <- get_var_for_test("get_middle_value", testEnv)
  check1 <- get_middle_value(seq(1,9))
  check2 <- get_middle_value(seq(1,20))
  if(check1 == 5) {
    if (check2 == 10) {
      succeed()
    }
    else {
      msg <- 'Your `get_middle_value` function did not return the middle value for a vector with an even length. Remember to "round down"'
      return(fail(msg))
    }
  } else {
    msg <- "Your `get_middle_value` function did not return the middle value for a vector with an odd length"
    return(fail(msg))
  }
})

test_that("Define `middle_digit` function", {
  expect_has_variable("`middle_digit`", var_type="double", envir=testEnv)
})

test_that("Check `middle_digit` value", {
  middle_digit <- get_var_for_test("middle_digit", testEnv)
  if(middle_digit == 4) {
    succeed()
  } else {
    msg <- "Your `middle_digit` value is incorrect. Did you use the `phone_number` variable?"
    return(fail(msg))
  }
})

test_that("Define `greater_than_4` function", {
  expect_has_variable("greater_than_4", var_type="logical", envir=testEnv)
})

test_that("Define `filtered_digits` function", {
  expect_has_variable("filtered_digits", var_type="double", envir=testEnv)
})

test_that("Check `filtered_digits` value", {
  filtered_digits <- get_var_for_test("filtered_digits", testEnv)
  if(identical(filtered_digits, c(8,6,7,9))) {
    succeed()
  } else {
    msg <- "Your `filtered_digits` value is incorrect. Did you filter the `phone_number` variable?"
    return(fail(msg))
  }
})
