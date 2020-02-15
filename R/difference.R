#' Examine the difference between newer datasets
#' @param a smaller datafile
#' @param b larger datafile
#' @return the row differences that remain
#' @export

difference <- function(a,b){
  a <- a[order(a[,11]),]
  b <- b[order(b[,11]),]
  a$id <- 1:nrow(a)
  b$id <- 1:nrow(b)
  d <- setdiff(b$id,a$id)
  ifelse(length(d) == 1, e <- paste0("", d), e <- paste0("", d, collapse = "|"))
  f <- b[grepl(e, b$id),]
  g <- that[rev(order(f$Admin.Date)),]
  return(g)
}
