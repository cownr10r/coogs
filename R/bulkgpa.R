#' Bulk calculate gpas. Some pre-cleaning in excel may be required.
#' @param a transfer course file from peoplesoft
#' @param b fransfer hours file from peoplesoft
#' @param d UH courses file from peoplesoft
#' @return summary of gpa and summary of hours taken
#' @examples
#' bulkgpa(a,b,d)
#' @export


bulkgpa <- function(a = transfer_courses_file, b = transfer_hours_file, d = UH_courses){
  library(readxl)
  library(magrittr)
  library(dplyr)
  trans1 <- readxl::read_excel(a, col_types = "text")

  names(trans1)[11] <- "college.Descr"
  names(trans1)[7]<- "Course.Descript"
  names(trans1)[37] <- "Grade_pt_p_unit"
  names(trans1)[31] <- "Unit.Taken"

  trans <- trans1 %>%
    .[!grepl("North Harris Montgomery C C Di", .$college.Descr),] %>%
    .[!duplicated(.[,c("ID", "Subject", "Catalog")]),]

  hours <- readxl::read_excel(b, col_types = "text") %>%
    .[,c("Transfer", "ID", "Term")] %>%
    .[rev(order(.$ID,.$Term)),] %>%
    .[!duplicated(.$ID),]

  trans2 <- merge(trans,hours, by = "ID", all.x = TRUE) %>% .[,c("ID", "Name", "Term", "Subject", "Catalog", "Course.Descript", "Unit.Taken", "Grade", "Grade_pt_p_unit","college.Descr")]

  hou <- readxl::read_excel(d, col_types = "text") %>% .[,-8]

  colnames(hou) <- c("ID", "Name", "Term", "Subject", "Catalog", "Course.Descript", "Unit.Taken", "Grade", "Grade_pt_p_unit","college.Descr", "Drop.Date")

  hou1 <-hou[!(grepl("[[:digit:]]", hou$Drop.Date)),] %>% .[,-11]

  final <- data.frame(rbind(trans2,hou1)) %>% .[order(.$ID,.$Subject, .$Catalog),]

  final$Grade_pt_p_unit <- final$Grade_pt_p_unit %>% as.numeric()

  final$Unit.Taken <- final$Unit.Taken %>% as.numeric()

  fin <- final %>% dplyr::group_by(ID) %>% dplyr::mutate(grdpt_unit = Unit.Taken * Grade_pt_p_unit)

  sumgpu <- fin %>% dplyr::group_by(ID) %>% dplyr::summarise(sum(grdpt_unit))

  sum_u <- fin %>% dplyr::group_by(ID) %>% dplyr::summarise(sum(Unit.Taken))

  totals <- merge(sumgpu, sum_u, by = "ID", all.x = TRUE)

  totals$gpa <- totals[,2]/totals[,3]

  tr7 <- merge(fin, totals, by = "ID", all.x = TRUE)

  return(tr7)

}
