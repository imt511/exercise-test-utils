# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 4-6")
library("lintr")
library("schoolmath")

#source("exercise-test-utils.R")

testEnv <- new.env()
file_name <- 'exercise_4-6.R'
file_path <- file_name
setwd("../../")

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
    if(anyNA(varOfInterest)){
      return() # don't continue if we couldn't read
    } else {
      varString <- paste(varOfInterest)
      if (length(varOfInterest) > 1) {
        varString <- paste(varOfInterest, collapse = '   ')
      }
      if(!grepl(varString, output[1], fixed=TRUE)) {
      msg <- sprintf(paste('The value of your', varName, 'variable (%s) not found in printed output (%s)'), varString, substring(output[1], 5))
      return(fail(msg))
      } else {
      succeed()
      }
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

test_that("Check `permits` variable", {
  expect_has_variable("permits", var_type="list", envir=testEnv)
})

test_that("Check type and dimensions of `permits` variable", {
  permits <- get_var_for_test("permits", testEnv)
  if(is.data.frame(permits)) {
    if (nrow(permits) == 142417){
      if (ncol(permits) == 24) {
        succeed()
      } else {
        msg <- 'Your `permits` data frame does not have the correct number of columns (24)'
        return(fail(msg))
      }
    } else {
      msg <- 'Your `permits` data frame does not have the correct number of rows (142417)'
      return(fail(msg))
    }
  } else {
    msg <- 'Your `permits` variable is not a data frame'
    return(fail(msg))
  }
})

test_that("Check `filtered_permits` variable", {
  expect_has_variable("filtered_permits", var_type="list", envir=testEnv)
})

test_that("Check type and dimensions of `filtered_permits` variable", {
  filtered_permits <- get_var_for_test("filtered_permits", testEnv)
  if(is.data.frame(filtered_permits)) {
    if (nrow(filtered_permits) == 21389){
      if (ncol(filtered_permits) == 25) {
        succeed()
      } else {
        msg <- 'Your `filtered_permits` data frame does not have the correct number of columns (25)'
        return(fail(msg))
      }
    } else {
      msg <- 'Your `filtered_permits` data frame does not have the correct number of rows (21389)'
      return(fail(msg))
    }
  } else {
    msg <- 'Your `filtered_permits` variable is not a data frame'
    return(fail(msg))
  }
})

test_that("Check `grouped_by_year` variable", {
  expect_has_variable("grouped_by_year", var_type="list", envir=testEnv)
})

test_that("Check type and dimensions of `grouped_by_year` variable", {
  grouped_by_year <- get_var_for_test("grouped_by_year", testEnv)
  if(is.data.frame(grouped_by_year)) {
    if (nrow(grouped_by_year) == 30){
      if (ncol(grouped_by_year) == 2) {
        succeed()
      } else {
        msg <- 'Your `grouped_by_year` data frame does not have the correct number of columns (30)'
        return(fail(msg))
      }
    } else {
      msg <- 'Your `grouped_by_year` data frame does not have the correct number of rows (2)'
      return(fail(msg))
    }
  } else {
    msg <- 'Your `grouped_by_year` variable is not a data frame'
    return(fail(msg))
  }
})

test_that("Check `highest_cost_by_year` variable", {
  expect_has_variable("highest_cost_by_year", var_type="list", envir=testEnv)
})

test_that("Check type and dimensions of `highest_cost_by_year` variable", {
  highest_cost_by_year <- get_var_for_test("highest_cost_by_year", testEnv)
  if(is.data.frame(highest_cost_by_year)) {
    if (nrow(highest_cost_by_year) == 1){
      if (ncol(highest_cost_by_year) == 2) {
        succeed()
      } else {
        msg <- 'Your `highest_cost_by_year` data frame does not have the correct number of columns (2)'
        return(fail(msg))
      }
    } else {
      msg <- 'Your `highest_cost_by_year` data frame does not have the correct number of rows (1)'
      return(fail(msg))
    }
  } else {
    msg <- 'Your `highest_cost_by_year` variable is not a data frame'
    return(fail(msg))
  }
})

test_that("Check values of `highest_cost_by_year`", {
  highest_cost_by_year <- get_var_for_test("highest_cost_by_year", testEnv)
  filtered_permits <- get_var_for_test("filtered_permits", testEnv)
  
  calcs <- filtered_permits %>%
  group_by(Year) %>%
  summarize(MeanEstCost = mean(EstProjectCost)) %>% 
  filter(Year > 2010) %>%
  filter(MeanEstCost == max(MeanEstCost))
  
  year <- pull(highest_cost_by_year, Year)
  value <- pull(highest_cost_by_year, MeanEstCost)
  
  year_check <- pull(calcs, Year)
  value_check <- pull(calcs, MeanEstCost)

  if (year == year_check) {
    if (value == value_check) {
      succeed()
    } else {
      msg <- 'The mean cost from your `highest_cost_by_year` data frame is incorrect'
      return(fail(msg))
    }
  } else {
    msg <- 'The year from your `highest_cost_by_year` data frame is incorrect'
    return(fail(msg))
  }
})

test_that("Check `grouped_by_year_and_class` variable", {
  expect_has_variable("grouped_by_year_and_class", var_type="list", envir=testEnv)
})

test_that("Check type and dimensions of `grouped_by_year_and_class` variable", {
  grouped_by_year_and_class <- get_var_for_test("grouped_by_year_and_class", testEnv)
  if(is.data.frame(grouped_by_year_and_class)) {
    if (nrow(grouped_by_year_and_class) == 125){
      if (ncol(grouped_by_year_and_class) == 4) {
        succeed()
      } else {
        msg <- 'Your `grouped_by_year_and_class` data frame does not have the correct number of columns (4)'
        return(fail(msg))
      }
    } else {
      msg <- 'Your `grouped_by_year_and_class` data frame does not have the correct number of rows (125)'
      return(fail(msg))
    }
  } else {
    msg <- 'Your `grouped_by_year_and_class` variable is not a data frame'
    return(fail(msg))
  }
})

test_that("Check column names of `grouped_by_year_and_class` variable", {
  grouped_by_year_and_class <- get_var_for_test("grouped_by_year_and_class", testEnv)
  cols <- colnames(grouped_by_year_and_class)
  check <- c("PermitClass", "Year", "MeanEstCost", "N")
  if(identical(cols, check)) {
    succeed()
  } else {
    msg <- 'Your `grouped_by_year_and_class` data frame does not have the correct columns. Did you use the correct case?'
    return(fail(msg))
  }
})


test_that("Check `min_cost_high_count` variable", {
  expect_has_variable("min_cost_high_count", var_type="list", envir=testEnv)
})

test_that("Check type and dimensions of `min_cost_high_count` variable", {
  min_cost_high_count <- get_var_for_test("min_cost_high_count", testEnv)
  if(is.data.frame(min_cost_high_count)) {
    if (nrow(min_cost_high_count) == 1){
      if (ncol(min_cost_high_count) == 4) {
        succeed()
      } else {
        msg <- 'Your `min_cost_high_count` data frame does not have the correct number of columns (4)'
        return(fail(msg))
      }
    } else {
      msg <- 'Your `min_cost_high_count` data frame does not have the correct number of rows (1)'
      return(fail(msg))
    }
  } else {
    msg <- 'Your `min_cost_high_count` variable is not a data frame'
    return(fail(msg))
  }
})

test_that("Check values of `min_cost_high_count`", {
  min_cost_high_count <- get_var_for_test("min_cost_high_count", testEnv)
  filtered_permits <- get_var_for_test("filtered_permits", testEnv)
  
  calcs <- filtered_permits %>%
    group_by(PermitClass, Year) %>%
    summarize(MeanEstCost = mean(EstProjectCost), N = n()) %>% 
    filter(N > 1000) %>%
    filter(MeanEstCost == min(MeanEstCost))
  
  value <- pull(min_cost_high_count, MeanEstCost)
    value_check <- pull(calcs, MeanEstCost)
  
  if (value == value_check) {
      succeed()
  } else {
    msg <- 'The cost from your `min_cost_high_count` data frame is incorrect'
    return(fail(msg))
  }
})