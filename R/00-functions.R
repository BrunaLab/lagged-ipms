#' Convert demographic survey data into binary survival
#' 
#' This function might only make sense to use if you continue surveying for
#' plants long after you plan to count them as dead.  It is convenient if you
#' have many NA entries after a plant has died (or is assumed dead) and you want
#' to know which years it was alive and which it was dead.  You want to ignore
#' NAs that are not trailing and you want to ignore trailing NAs < n
#'
#' @param x any ordered numeric vector such as height, size, or number of shoots, that is collected every observation unless plants are not detected.
#' @param n number of successive, trailing missing survey points required to consider an organism dead.
#'
#' @return a logical vector with TRUE = alive and FALSE = assumed dead
#' @examples
#' #Assume plants dead with after not detected for 2 years
#' dead  <- c(1, 3, NA, NA, 1, 2, 1, NA, NA)
#' alive <- c(1, 3, NA, NA, 1, 2, 1, 3, NA)
#' as_living(dead)
#' as_living(alive)
as_living <- function(x, n = 2) {
  
  #if at least last 2 values are NA
  trail <- (length(x) - n + 1):length(x)
  #exclude negative indexes
  trail <- trail[trail >= 0]
  
  if(all(is.na(x))){
    alive <- rep(NA, length(x))
  } else if (all(is.na(x[trail]))) {
    #find the last non-NA value
    y <- cumsum(!is.na(x))
    last_real <- which.max(y == max(y))
    alive <- c(rep(TRUE, last_real), rep(FALSE, length(x) - last_real))
  } else {
    alive <- rep(TRUE, length(x))
  }
  return(alive)
}