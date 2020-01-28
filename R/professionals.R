#' Identify professional certs
#' @param a = filename
#' @return file with only professional certifications
#' @export


professionals <- function(a = filename){
  #source: https://stackoverflow.com/questions/22249702/delete-rows-containing-specific-strings-in-r
  d <- a[grepl("Principal|Superintendent|School Counselor|Reading Specialist", a$Test.Name),]
  return(d)
}
