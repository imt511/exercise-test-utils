# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 2-2")
library("lintr")
library("schoolmath")

#source("exercise-test-utils.R")

testEnv <- new.env()
file_name <- 'exercise_2-2.R'
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

test_that("Define `colors` variable", {
  expect_has_variable("colors", var_type="character", envir=testEnv)
})

test_that("Check value of colors", {
  colors <- get_var_for_test("colors", testEnv)
  if(identical(c('orange', 'violet', 'green'), colors)) {
    succeed()
  } else {
    msg <- sprintf("Your `colors` vector (%s) is not (orange, violet, green)", paste(colors, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `primary_colors` variable", {
  expect_has_variable("primary_colors", var_type="character", envir=testEnv)
})

test_that("Check value of primary_colors", {
  primary_colors <- get_var_for_test("primary_colors", testEnv)
  if(identical(c('red', 'blue', 'yellow'), primary_colors)) {
    succeed()
  } else {
    msg <- sprintf("Your `primary_colors` vector (%s) is not (red, blue, yellow)", paste(primary_colors, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `all_colors` variable", {
  expect_has_variable("all_colors", var_type="character", envir=testEnv)
})

test_that("Check value of `primary_colors`", {
  all_colors <- get_var_for_test("all_colors", testEnv)
  primary_colors <- get_var_for_test("primary_colors", testEnv)
  colors <- get_var_for_test("colors", testEnv)
  check <- append(primary_colors, colors)
  if(identical(check, all_colors)) {
    succeed()
  } else {
    msg <- sprintf("Your `all_colors` vector (%s) is not (%s)", paste(all_colors, collapse = ", "), paste(check, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `ones_and_twos` variable", {
  expect_has_variable("ones_and_twos", var_type="double", envir=testEnv)
})

test_that("Check value of `ones_and_twos`", {
  ones_and_twos <- get_var_for_test("ones_and_twos", testEnv)
  check <- c(1,2,1,2,1,2)
  if(identical(check, ones_and_twos)) {
    succeed()
  } else {
    msg <- sprintf("Your ones_and_twos vector (%s) is not (%s)", paste(ones_and_twos, collapse = ", "), paste(check, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `by_two` variable", {
  expect_has_variable("by_two", var_type="double", envir=testEnv)
})

test_that("Check value of `by_two`", {
  by_two <- get_var_for_test("by_two", testEnv)
  check <- seq(2,12,2)
  if(identical(check, by_two)) {
    succeed()
  } else {
    msg <- sprintf("Your `ones_and_twos` vector (%s) is not (%s)", paste(by_two, collapse = ", "), paste(check, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `sums` variable", {
  expect_has_variable("sums", var_type="double", envir=testEnv)
})

test_that("Check value of `sums`", {
  by_two <- get_var_for_test("by_two", testEnv)
  ones_and_twos <- get_var_for_test("ones_and_twos", testEnv)
  sums <- get_var_for_test("sums", testEnv)
  check <- ones_and_twos + by_two
  if(identical(check, sums)) {
    succeed()
  } else {
    msg <- sprintf("Your `sums` vector (%s) is not (%s)", paste(sums, collapse = ", "), paste(check, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `ones_twos_short` variable", {
  expect_has_variable("ones_twos_short", var_type="double", envir=testEnv)
})

test_that("Check value of `ones_twos_short`", {
  ones_twos_short <- get_var_for_test("ones_twos_short", testEnv)
  check <- c(1,2)
  if(identical(check, ones_twos_short)) {
    succeed()
  } else {
    msg <- sprintf("Your `ones_twos_short` vector (%s) is not (%s)", paste(ones_twos_short, collapse = ", "), paste(check, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `sums_2` variable", {
  expect_has_variable("sums_2", var_type="double", envir=testEnv)
})

test_that("Check value of sums_2", {
  by_two <- get_var_for_test("by_two", testEnv)
  ones_twos_short <- get_var_for_test("ones_twos_short", testEnv)
  sums_2 <- get_var_for_test("sums_2", testEnv)
  check <- ones_twos_short + by_two
  if(identical(check, sums_2)) {
    succeed()
  } else {
    msg <- sprintf("Your `sums_2` vector (%s) is not (%s)", paste(sums_2, collapse = ", "), paste(check, collapse = ", "))
    return(fail(msg))
  }
})

test_that("Define `are_equal` variable", {
  expect_has_variable("are_equal", var_type="logical", envir=testEnv)
})

test_that("Check value of `are_equal`", {
  sums <- get_var_for_test("sums", testEnv)
  sums_2 <- get_var_for_test("sums_2", testEnv)
  are_equal <- get_var_for_test("are_equal", testEnv)
  check <- sums == sums_2
  if(identical(sums, sums_2)) {
    if (identical(are_equal, check)) {
      succeed()
    } else {
      msg <- "Your `sums` and `sum_2` vectors are identical but your are_equal vector doesn't match"
      return(fail(msg))
    } 
  } else {
    msg <- "Your `sums` and `sum_2` vectors are not identical"
    return(fail(msg))
  }
})