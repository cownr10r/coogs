#' Identify professional certs
#' @param a file name
#' @return file with only professional certifications
#' @export


professionals <- function(a = file){
  #source: https://stackoverflow.com/questions/22249702/delete-rows-containing-specific-strings-in-r
  d <- a[grepl("Principal|Superintendent|School Counselor|Reading Specialist|Educational Diagnostician", a$Test.Name),]
  return(d)
}
