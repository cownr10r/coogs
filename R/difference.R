#' Examine the difference between newer datasets
#' @param a smaller datafile
#' @param b larger datafile
#' @param d column number for ID 1
#' @param e column number for ID 2
#' @param f column number for index 1
#' @param g columnn number for index 2
#' @return the columns that remain
#' @export

difference <- function(a,b,d,e,f,g){
  h <- a[order(a[,d]),]
  j <- b[order(b[,e]),]
  k <- setdiff(j[,g], h[,f])
  l <- paste0(k)
  m <- b[c(l),]
  return(m)
}
