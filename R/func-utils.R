#' String concatenation operator
#'
#' @description
#' Infix string concatenation operator
#' @param str1 lhs string
#' @param str2 rhs string
#' @examples
#' "Hello" %++% "world"

"%++%" <- function(str1, str2) {
  paste0(str1, str2)
}


#' Alias for {zeallot} destructuring operator
#'
#' @description
#' Operator %<-% is also used by {future} package and using operator with
#' package prefix i.e. {package}::%...% seems to be cumbersome.
#'
#' @examples
#' temp_list = list(a = 0, b = 1)
#' c(x, y) %<~% temp_list

"%<~%" <- zeallot::"%<-%"
