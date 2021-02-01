# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 1-3")
library("lintr")
library("schoolmath")

#source("exercise-test-utils.R")

testEnv <- new.env()
file_name <- 'exercise_1-3.R'
file_path <- paste0("../../", file_name)

# has side-effect of running script; returns first error that occurs
loadSource <- function() {
  tryCatch({
    # execute the source file. Blocks printed output, variables scoped to testEnv
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

test_that("Define the `food` variable", {
  expect_has_variable("food", var_type="character", envir=testEnv)
})

test_that("Print the `food` variable", {
  expect_printed("food")
})

test_that("Define the `num_of_friends`", {
  expect_has_variable("num_of_friends", var_type="double", envir=testEnv)
})

test_that("Check that `num_of_friends` is a whole number", {
  num_of_friends <- get_var_for_test("num_of_friends", testEnv)
  if(is.numeric(num_of_friends) & is.whole(num_of_friends)) {
      succeed()
  } else {
    msg <- sprintf('The value of your `num_of_friends` variable (%.4f) must be a positive whole number', num_of_friends)
    return(fail(msg))
  }
})

test_that("Define the `meal_price`", {
  expect_has_variable("meal_price", var_type="double", envir=testEnv)
})

test_that("Check that `meal_price` is a decimal number", {
  meal_price <- get_var_for_test("meal_price", testEnv)
  if(is.numeric(meal_price) & (is.real.positive(meal_price) & (is.decimal(meal_price)))) {
    if(is.whole(meal_price * 100)){ #modulo was giving weird results for meal_price = 5.1
      succeed()
    } else {
      msg <- sprintf('Your `meal_price` variable (%.4f) must not contain values smaller than cents', meal_price)
      return(fail(msg))
    }
  } else {
    msg <- sprintf('The value of your `meal_price` variable (%d) must be a positive number that is not a whole number', meal_price)
    return(fail(msg))
  }
})

test_that("Define the `price_with_tip` variable", {
  expect_has_variable("price_with_tip", var_type="double", envir=testEnv)
})

test_that("Check value of `price_with_tip`", {
  price_with_tip <- get_var_for_test("price_with_tip", testEnv)
  meal_price <- get_var_for_test("meal_price", testEnv)
  expected_val <- meal_price * 1.15
  if(price_with_tip == expected_val) {
    succeed()
  } else {
    msg <- sprintf('The value of your `price_with_tip` variable (%.04f) is not 115%% of your `meal_price` variable (%.02f)', price_with_tip, meal_price)
    return(fail(msg))
  }
})

test_that("Define the `cost_for_party` variable", {
  expect_has_variable("cost_for_party", var_type="double", envir=testEnv)
})

test_that("Check that `cost_for_party` is correct multiple of price with tip", {
  price_with_tip <- get_var_for_test("price_with_tip", testEnv)
  cost_for_party <- get_var_for_test("cost_for_party", testEnv)
  num_of_friends <- get_var_for_test("num_of_friends", testEnv)
  if(cost_for_party == (price_with_tip * (num_of_friends + 1))) {
    succeed()
  } else {
    msg <- sprintf('The value of your `cost_for_party` variable (%.02f) is incorrect Did you remember to include yourself?', cost_for_party)
    return(fail(msg))
  }
})

test_that("Define `budget` variable", {
  expect_has_variable("budget", var_type="double", envir=testEnv)
})

test_that("Check value of `budget` variable", {
  budget <- get_var_for_test("budget", testEnv)
  if(is.real.positive(budget)) {
    succeed()
  } else {
    msg <- 'The value of your `budget` variable must be a positive number'
    return(fail(msg))
  }
})

test_that("Define `can_afford_party` variable", {
  expect_has_variable("can_afford_party", var_type="logical", envir=testEnv)
})

test_that("Check that `can_afford_party` is correctly calcualted", {
  budget <- get_var_for_test("budget", testEnv)
  cost_for_party <- get_var_for_test("cost_for_party", testEnv)
  can_afford_party <- get_var_for_test("can_afford_party", testEnv)
  if((cost_for_party <= budget) == can_afford_party) {
    succeed()
  } else {
    msg <- 'The value of your `can_afford_party` variable is incorrect'
    return(fail(msg))
  }
})
