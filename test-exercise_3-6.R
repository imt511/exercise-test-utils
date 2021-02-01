# nolint

# Sys.setenv(NOT_CRAN = "true") ## ??
context("Exercise 3-6")
library("lintr")
library("schoolmath")

#source("exercise-test-utils.R")

testEnv <- new.env()
file_name <- 'exercise_3-6.R'
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

test_that("Check `products` variable", {
  expect_has_variable("products", var_type="character", envir=testEnv)
})

test_that("Check value of `products` variable", {
  products <- get_var_for_test("products", testEnv)
  check <- paste("Product", seq(1, 100))
  if (identical(check, products)) {
    succeed()
  } else {
    msg <- 'The values of your `products` vector are incorrect. Each should be "Product #" with # being a number from 1 to 100'
    return(fail(msg))
  }
})

test_that("Check `prices` variable", {
  expect_has_variable("prices", var_type="double", envir=testEnv)
})

test_that("Check length of `prices` variable", {
  prices <- get_var_for_test("prices", testEnv)
  if (length(prices) == 100) {
    succeed()
  } else {
    msg <- 'The length of your `prices` vector should be 100'
    return(fail(msg))
  }
})

test_that("Check `adjustments` variable", {
  expect_has_variable("adjustments", var_type="double", envir=testEnv)
})

test_that("Check length of `adjustments` variable", {
  adjustments <- get_var_for_test("adjustments", testEnv)
  if (length(adjustments) == 100) {
    succeed()
  } else {
    msg <- 'The length of your `adjustments` vector should be 100'
    return(fail(msg))
  }
})

test_that("Check `product_sheet` variable", {
  expect_has_variable("product_sheet", var_type="list", envir=testEnv)
})

test_that("Check type and dimensions of `product_sheet` variable", {
  product_sheet <- get_var_for_test("product_sheet", testEnv)
  if(is.data.frame(product_sheet)) {
    if (nrow(product_sheet) == 100){
      if (ncol(product_sheet) == 5) {
        succeed()
      } else {
        msg <- 'Your `product_sheet` data frame does not have the correct number of columns (5)'
        return(fail(msg))
      }
    } else {
      msg <- 'Your `product_sheet` data frame does not have the correct number of rows (100)'
      return(fail(msg))
    }
  } else {
    msg <- 'Your `product_sheet` variable is not a data frame'
    return(fail(msg))
  }
})

test_that("Check `product_50_price` variable", {
  expect_has_variable("product_50_price", var_type="double", envir=testEnv)
})

test_that("Check value of `product_50_price` variable", {
  product_50_price <- get_var_for_test("product_50_price", testEnv)
  product_sheet <- get_var_for_test("product_sheet", testEnv)
  check <- product_sheet[product_sheet$products == 'Product 50', ]$adjusted_price[1]
  if (product_50_price == check) {
    succeed()
  } else {
    msg <- 'The value of your `product_50_price` variable is incorrect. Are you indexing the correct row/column?'
    return(fail(msg))
  }
})

test_that("Check `num_of_big_sale_products` variable", {
  expect_has_variable("num_of_big_sale_products", var_type="integer", envir=testEnv)
})

test_that("Check value of `num_of_big_sale_products` variable", {
  num_of_big_sale_products <- get_var_for_test("num_of_big_sale_products", testEnv)
  product_sheet <- get_var_for_test("product_sheet", testEnv)
  check <- sum(product_sheet$big_sale)
  if (num_of_big_sale_products == check) {
    succeed()
  } else {
    msg <- 'The value of your `num_of_big_sale_products` variable is incorrect. Are you indexing the correct row/column?'
    return(fail(msg))
  }
})

test_that("Check `num_getting_markup` variable", {
  expect_has_variable("num_getting_markup", var_type="integer", envir=testEnv)
})

test_that("Check value of `num_getting_markup` variable", {
  num_getting_markup <- get_var_for_test("num_getting_markup", testEnv)
  product_sheet <- get_var_for_test("product_sheet", testEnv)
  check <- nrow(product_sheet[product_sheet$adjustments > 1.0, ])
  if (num_getting_markup == check) {
    succeed()
  } else {
    msg <- 'The value of your `num_getting_markup` variable is incorrect. Are you indexing the correct row/column?'
    return(fail(msg))
  }
})

test_that("Check `lowest_starting_price` variable", {
  expect_has_variable("lowest_starting_price", var_type="character", envir=testEnv)
})

test_that("Check value of `lowest_starting_price` variable", {
  lowest_starting_price <- get_var_for_test("lowest_starting_price", testEnv)
  product_sheet <- get_var_for_test("product_sheet", testEnv)
  check <- product_sheet[product_sheet$prices == min(product_sheet$prices), ]$products[1]
  if (lowest_starting_price == check) {
    succeed()
  } else {
    msg <- 'The value of your `lowest_starting_price` variable is incorrect. Are you indexing the correct row/column?'
    return(fail(msg))
  }
})

test_that("Check `highest_adjusted_price` variable", {
  expect_has_variable("highest_adjusted_price", var_type="character", envir=testEnv)
})

test_that("Check value of `highest_adjusted_price` variable", {
  highest_adjusted_price <- get_var_for_test("highest_adjusted_price", testEnv)
  product_sheet <- get_var_for_test("product_sheet", testEnv)
  check <- product_sheet[product_sheet$adjusted_price == max(product_sheet$adjusted_price), ]$products[1]
  if (highest_adjusted_price == check) {
    succeed()
  } else {
    msg <- 'The value of your `highest_adjusted_price` variable is incorrect. Are you indexing the correct row/column?'
    return(fail(msg))
  }
})