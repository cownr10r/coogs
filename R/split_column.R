#' Split a column into two columns by regex
#' @param v data frame
#' @param w column number
#' @param x regex in quotes
#' @param y name of first split column
#' @param z name of second split column
#' @return dataframe with split columns and original data frame
#' @export


split_column <- function(v,w,x,y = "firstname",z = "secondname"){
  a <- ""
  elems <- unlist(strsplit(v[,w], paste0(a,x) ) )
  m <- data.frame(matrix(elems, ncol = 2 , byrow = TRUE), stringsAsFactors = FALSE)
  n <- data.frame(cbind(v,m))
  names(n)[which(names(n) == "X1")] <- paste0(a,y)
  names(n)[which(names(n) == "X2")] <- paste0(a,z)
  return(n)
}
