library("lintr")

# Define custom expectations

# envir=pos.to.env(-1L)
expect_has_variable <- function(object, var_name="", var_type=NULL, var_value=NULL, envir= ){

  if(var_name == ""){
    
  }
  
  # capture object and label ?? This is actual expectation object?
  act <- quasi_label(rlang::enquo(object), arg = "object")
  
  # do the expect()
  act$exists <- exists(var_name, envir = envir) # do the work
  expect(
    act$exists == TRUE, # what to check
    sprintf("Variable `%s` is not declared. Did you name it correctly? Case matters!", var_name)
  )
  
  # return result
  invisible(act$val)
}
