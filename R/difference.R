#' examine the difference between newer datasets
#' @param a smaller data frame
#' @param b larger data frame
#' @param d column number for first data frame ID
#' @param p column number for second data frame ID
#' @param m column number for first index number
#' @param n column number for second index number
#' @return a data set of rows that remain after comparison
#' @export



difference <- function(a = smaller_datafile, b = larger_datafile, d = column_number_for_id_1, p = column_for_id_2, m = column_for_index1, n = column_number_for_index_2){
  f <- a[order(a[,d]),]
  g <- b[order(b[,p]),]
  f$num <- 1:length(f[,1])
  g$num <- 1:length(g[,1])
  e <- setdiff(g[,m], f[,n])
  h <- paste0(e)
  l <- b[c(h),]
  return(l)
}
