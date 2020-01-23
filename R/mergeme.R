#' merge two files
#' @param b larger dataset
#' @param d smaller dataset
#' @param e id field for original datasets in common
#' @return a merged set with inner, outer, and all joins.
mergeme <- function(b,d,e){ #b is larger dataset
  this <- merge(b,d, all.y = T, by = "e")
  that <- merge(b,d, all.x = T, by = "e")
  that$id <- 1:length(that[,1]) #larger
  this$id <- 1:length(this[,1])
  other <- merge(that, this, all.x = T, by = "id")
}

