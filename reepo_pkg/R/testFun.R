# testFun()----------
#' @title a test function to see how it works. Meows
#' @description type a number, and it will gives you its square and cubic value.
#' Or a text, it will gives you the length of it.
#'
#' @param num A number
#' @param text A character string
#' @param cat either T or F
#' @return A list
#' @examples
#' testFun(num=3, cat=TRUE)
#' testFun(5, "oh hello", cat=TRUE)
testFun <- function(num = NULL, text = NULL, cat = TRUE) {
  res <- list("num_result" = NULL, "text_result" = NULL)
  if (!is.null(num)) {
    res$num_result <- c(num^2, num^3)
    if (cat == TRUE) {
      cat("we have a number\n")
    }
  }
  if (!is.null(text)) {
    res$text_result <- nchar(text)
    if (cat == TRUE) {
      cat("we have a text\n")
    }
  }
  return(res)
}
