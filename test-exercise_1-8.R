# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 1-8")
library("lintr")
library("schoolmath")

#source("exercise-test-utils.R")

testEnv <- new.env()
file_name <- 'exercise_1-8.R'
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

test_that("Define the `my_name` variable", {
  expect_has_variable("my_name", var_type="character", envir=testEnv)
})

test_that("Define the `name_length` variable", {
  expect_has_variable("name_length", var_type="integer", envir=testEnv)
})

test_that("Correctly calculate the `name_length` variable", {
  my_name <- get_var_for_test("my_name", testEnv)
  name_length <- get_var_for_test("name_length", testEnv)
  if(name_length == nchar(my_name)) {
    succeed()
  } else {
    msg <- sprintf('The value of your `name_length` variable (%d) is incorrect. Did you use the `nchar()` function?', name_length)
    return(fail(msg))
  }
})

test_that("Print the `name_length` variable", {
  expect_printed('name_length')
})

test_that("Define the `num_of_times` variable", {
  expect_has_variable("num_of_times", var_type="double", envir=testEnv)
})

test_that("Correctly calculate the `num_of_times` variable", {
  num_of_times <- get_var_for_test("num_of_times", testEnv)
  name_length <- get_var_for_test("name_length", testEnv)
  expected <- floor(500 / name_length)
  if(expected == num_of_times) {
    succeed()
  } else {
    msg <- sprintf('The value of your `num_of_times` variable (%d) is incorrect. Did you use the `floor()` function?', num_of_times)
    return(fail(msg))
  }
})

test_that("Define the `lowercase_name` variable", {
  expect_has_variable("lowercase_name", var_type="character", envir=testEnv)
})

test_that("Have correct value for the `lowercase_name` variable", {
  lowercase_name <- get_var_for_test("lowercase_name", testEnv)
  my_name <- get_var_for_test("my_name", testEnv)
  if(lowercase_name == tolower(my_name)) {
    succeed()
  } else {
    msg <- sprintf('The value of your `lowercase_name` variable (%s) is incorrect. Did you use the `tolower()` function?', lowercase_name)
    return(fail(msg))
  }
})

test_that("Define the `fruits` variable", {
  expect_has_variable("fruits", var_type="character", envir=testEnv)
})

test_that("Have correct value for the the `fruits` variable", {
  fruits <- get_var_for_test("fruits", testEnv)
  if(fruits == 'apples and bananas') {
    succeed()
  } else {
    msg <- sprintf('The value of your `fruits` variable (%s) is incorrect. Be sure you have the correct capitalization and puncutation. Also check for typos.', fruits)
    return(fail(msg))
  }
})

test_that("Define the `fruits_e` variable", {
  expect_has_variable("fruits_e", var_type="character", envir=testEnv)
})

test_that("Have correct value for the the `fruits_e` variable", {
  fruits_e <- get_var_for_test("fruits_e", testEnv)
  if(fruits_e == "eepples eend beeneenees") {
    succeed()
  } else {
    msg <- sprintf('The value of your `fruits_e` variable (%s) is incorrect. Did you use the `grep()` function?', fruits_e)
    return(fail(msg))
  }
})

test_that("Define the `fruits_o` variable", {
  expect_has_variable("fruits_o", var_type="character", envir=testEnv)
})

test_that("Have correct value for the the `fruits_e` variable", {
  fruits_o <- get_var_for_test("fruits_o", testEnv)
  if(fruits_o == "opples ond bononos") {
    succeed()
  } else {
    msg <- sprintf('The value of your `fruits_o` variable (%s) is incorrect. Did you use the `grep()` function?', fruits_o)
    return(fail(msg))
  }
})

test_that("Check print apples and bananas", {
  expect_printed_at_loc("I like to eat apples and bananas", 2)
})

test_that("Check print eepples eend beeneenees", {
  expect_printed_at_loc("I like to eat eepples eend beeneenees", 3)
})

test_that("Check print opples ond bononos", {
  expect_printed_at_loc("I like to eat opples ond bononos", 4)
})