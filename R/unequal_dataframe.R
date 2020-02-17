#' An unequal dataframe with NAs to make up the difference
#' @param x smaller
#' @param y larger datafile
#' @return a dataframe of unequal lengths with NAs
#' @export

unequal_dataframe<- function(x,y){
  # source https://stackoverflow.com/questions/7196450/create-a-data-frame-of-unequal-lengths
  max.len = max(length(x), length(y))
  x = c(x, rep(NA, max.len - length(x)))
  y = c(y, rep(NA, max.len - length(y)))
  z <- data.frame(cbind(x,y))
  return(z)
}
