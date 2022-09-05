check_partial <- function(I) {
  
  suppressWarnings({
    val <- I %>%
      as.data.frame() %>%
      unlist() %>%
      as.vector() %>%
      as.numeric()})
  # val <- I$px
  return(any(val == 1 | val == 0 | val == -1))
  
}

error_partial <- function() {
  
  stop("This formal context is partial, and this operation needs it to be binary or fuzzy.",
       call. = FALSE)
  
}

error_not_partial <- function() {
  
  stop("This formal context is not partial, having more elements than allowed",
       call. = FALSE)
  
}