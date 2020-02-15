#' Examine the difference between newer datasets
#' @param a smaller datafile
#' @param b larger datafile
#' @return the differences that remain
#' @export

difference <- function(a,b){
  a <- a[order(a[,11]),]
  b <- b[order(b[,11]),]
  a$id <- 1:nrow(a)
  b$id <- 1:nrow(b)
  d <- setdiff(b$id,a$id)
  e <- paste(d, collapse="|")
  f <- two[grepl(e, two$id),]
  g <- that[rev(order(f$Admin.Date)),]
  return(g)
}
