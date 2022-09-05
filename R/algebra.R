get_num_from_mat <- function(matrix_value){
  if(matrix_value == '-'){
    result <- -1
  }else if (matrix_value == 'o'){
    result <- 0
  }else if (matrix_value == '+'){
    result <- 1
  }else if (matrix_value == 'ι'){
    result <- 2
  }else {
    result <- 100
  }
  return(result)
}

get_mat_from_num <- function(numeric_value){
  if(numeric_value == -1){
    result <- '-'
  }else if (numeric_value == 0){
    result <- 'o'
  }else if (numeric_value == 1){
    result <- '+'
  }else if (numeric_value == 2){
    result <- 'ι'
  }else {
    result <- ''
  }
  return(result)
}

comp_mat_vals <- function(matrix_value1, matrix_value2){
  comp_val <- (2)
  if(matrix_value1 == matrix_value2 || matrix_value1 == 'ι' || matrix_value2 == 'o'){
    comp_val <- 0
  }else if (matrix_value1== 'o' || matrix_value2 == 'ι'){
    comp_val <- 1
  }
  
  return(comp_val)
}

infimum <- function(matrix_value1, matrix_value2){
  comp <- comp_mat_vals(matrix_value1, matrix_value2)
  result <- get_mat_from_num(0)
  
  if(comp == 0){
    result <- matrix_value2
  }else if (comp == 1){
    result <- matrix_value1
  }
  
  return(result)
}

supremum <- function(matrix_value1, matrix_value2){
  comp <- comp_mat_vals(matrix_value1, matrix_value2)
  result <- get_mat_from_num(2)
  
  if(comp == 0){
    result <- matrix_value1
  }else if (comp == 1){
    result <- matrix_value2
  }
  
  return(result)
}


