# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 1")
library("lintr")

#source("exercise-test-utils.R")

testEnv <- new.env()

# has side-effect of running script; returns first error that occurs
loadSource <- function() {
  tryCatch({
    # execute the source file. Blockes printed output, variables scoped to testEnv
    utils::capture.output(source("../../exercise_1-1.R", testEnv))
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

  lint_results = lint(filename = "../../exercise.R", linter = my_linters)
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

test_that("Define the `food` variable", {
  expect_has_variable("food", var_type="character", envir=testEnv)
})

# runs capture.output and returns result, will fail() test on error
get_output_for_test <- function(source_file, envir, should.stop=FALSE){
  tryCatch({
    # execute the source file. Blockes printed output, variables scoped to testEnv
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
    actual <- get("food", testEnv)
    return(actual)
  }, error = function(e) {
    msg <- sprintf("Unable to read value `%s`. Make sure the variable has been defined!", var_name)
    if(should.stop){ stop(msg) } else { fail(msg) }
    return(NA)
  })
}


test_that("Print the `food` variable", {

  # tryCatch({ # handle the multiple possible errors as single failure
  #   # get_output
  #   output <- get_output_for_test("../../exercise.R", testEnv, should.stop = TRUE)
  #
  #   food <- get_var_for_test("food", testEnv, should.stop = TRUE)
  # }, error = function(e){
  #   fail(e$message)
  # })

  #print(testEnv$food) # may not need other function? Or make to handle list of variables?

  output <- get_output_for_test("../../exercise.R", testEnv)
  if(is.na(output)){
    return() # don't continue if we couldn't read
  } else if(length(output) < 1) {
    return(fail("No printed output. Need to print out the `food` variable."))
  } else {
    food <- get_var_for_test("food", testEnv)
    if(is.na(food)){
      return() # don't continue if we couldn't read
    } else if(!grepl(food, output[1], fixed=TRUE)) {
      msg <- sprintf('The value of your `food` variable ("%s") not found in first printed line (%s)', food, substring(output[1], 5))
      return(fail(msg))
    } else {
      succeed()
    }
  }
})



### Questions:
# can I have this test file source tests from online, so can adjust them remotely? It looks like that might work!!

