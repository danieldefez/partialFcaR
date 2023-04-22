.set_to_string <- function(S, attributes) {
  
  idx <- Matrix::which(S != 0)
  
  if (length(idx) > 0) {
    
    A <- S[idx]
    
    if (A[1] == 2) return("{oxy}")
    
    att <- attributes[idx] %>% stringr::str_trim("both")
    att[A == -1] <- paste0("-", att[A == -1])
    tmp <- paste0("{",
                  stringr::str_flatten(att,
                                       collapse = ", "), "}")
    
    return(tmp)
    
  } else {
    
    "{}"
    
  }
  
}


.concept_to_string <- function(vA, vB, objects, attributes) {

  A <- .set_to_string(vA, objects)
  B <- .set_to_string(vB, attributes)

  return(paste0("(", A, ", ", B, ")"))

}
