#' Bulk calculate gpas. Some pre-cleaning in excel may be required.
#' @param a transfer course file from peoplesoft
#' @param b fransfer hours file from peoplesoft
#' @param d UH courses file from peoplesoft
#' @param e Content subjects
#' @return content and cumulative gpa and summary of cumulative and content hours taken
#' @examples
#' bulkgpa(a,b,d, "core-ec-6")
#' @export


bulkgpa <- function(a = transfer_courses_file, b = transfer_hours_file, d = UH_courses, e = content_areas){

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

  trans2 <- merge(trans,hours, by = "ID", all.x = TRUE) %>% .[,c("ID", "Name", "Term", "Subject", "Catalog", "Course.Descript", "Unit.Taken", "Grade", "Grade_pt_p_unit","college.Descr")] %>%
    .[grepl("A|B|C", .$Grade),]

  hou <- readxl::read_excel(d, col_types = "text") %>% .[,-8]

  colnames(hou) <- c("ID", "Name", "Term", "Subject", "Catalog", "Course.Descript", "Unit.Taken", "Grade", "Grade_pt_p_unit","college.Descr", "Drop.Date")

  hou1 <-hou[!(grepl("[[:digit:]]", hou$Drop.Date)),] %>% .[,-11]


  f <- data.frame(rbind(trans2,hou1)) %>% .[order(.$ID,.$Subject, .$Catalog),]


  fi <- dplyr::filter(f, !grepl("ENGL",Subject))

  fin <- dplyr::filter(f, grepl("ENGL", Subject), !grepl("1303|1304", Catalog))

  final <- data.frame(rbind(fi,fin)) %>% .[order(.$ID,.$Subject,.$Catalog),]




  final$Grade_pt_p_unit <- final$Grade_pt_p_unit %>% as.numeric()

  final$Unit.Taken <- final$Unit.Taken %>% as.numeric()

  fin <- final %>% dplyr::group_by(ID) %>% dplyr::mutate(grdpt_unit = Unit.Taken * Grade_pt_p_unit)

  sumgpu <- fin %>% dplyr::group_by(ID) %>% dplyr::summarise(sum(grdpt_unit))

  sum_u <- fin %>% dplyr::group_by(ID) %>% dplyr::summarise(sum(Unit.Taken))

  totals <- merge(sumgpu, sum_u, by = "ID", all.x = TRUE)

  totals$GPA <- totals[,2]/totals[,3]

  tr7 <- merge(fin, totals, by = "ID", all.x = TRUE)


  # Here ends general gpa

  ifelse(e == "core-ec-6",   final1 <- final[grepl("ENGL|MATH|BCHM|BIOL|CHEM|GEOL|PHYS|ANTH|ECON|GEOG|HIST|POLS|PSYC|SOC|ARED|ART|ARTH|MUAP|MUED|MUSA|MUSI|DAN|THEA|KIN|PEB|HLT|NUTR", final$Subject),],
         ifelse(e == "art-ec-12",  final1 <- final[grepl("ART|ARED|ARTH", final$Subject),],
                ifelse(e == "dance-6-12", final1 <- final[grepl("DAN", final$Subject),],
                       ifelse(e =="math-4-8", final1 <- final[grepl("MATH", final$Subject),],
                              ifelse(e =="math-7-12", final1 <- final[grepl("MATH", final$Subject),],
                                     ifelse(e =="elar-4-8", final1 <- final[grepl("ENGL", final$Subject),],
                                            ifelse(e =="elar-7-12", final1 <- final[grepl("ENGL", final$Subject),],
                                                   ifelse(e =="chemistry-8-12", final1 <- final[grepl("BCHM|CHEM", final$Subject),],
                                                          ifelse(e =="LOTE-spanish-ec-12",  final1 <- final[grepl("SPAN", final$Subject),],
                                                                 ifelse(e =="physics-math-7-12", final1 <- final[grepl("MATH|PHYS", final$Subject),],
                                                                        ifelse(e =="life-science-7-12",  final1 <- final[grepl("BCHM|BIOL", final$Subject),],
                                                                               ifelse(e =="physical-science-7-12", final1 <- final[grepl("PHY", final$Subject),],
                                                                                      ifelse(e =="bilingual-generalist-ec-6", final1 <- final[grepl("ENGL|MATH|BCHM|BIOL|CHEM|GEOL|PHYS|ANTH|ECON|GEOG|HIST|POLS|SPYC|SOC|SPAN|ARED|ART|ARTH|MUAP|MUED|MUSA|MUSI|DAN|THEA|KIN|PEB|HLT|NUTR", final$Subject),],
                                                                                             ifelse(e =="science", final1 <- final[grepl("BIOL|BCHM|CHEM|GEOL|PHYS", final$Subject),],
                                                                                                    ifelse(e == "social-science", final1 <- final[grepl("HIST|ECON|GEOG|PLS|PSYC|SOC", final$Subject),],
                                                                                                           ifelse(e == "sped-ec-12", final1 <- final[grepl("EPSY|SPEC|ENGL|MATH|BCHM|BIOL|CHEM|GEOL|PHYS|ANTH|ECON|GEOG|HIST|POLS|PSYC|SOC|ARED|ART|ARTH|MUAP|MUED|MUSA|MUSI|DAN|THEA|KIN|PEB|HLT|NUTR", final$Subject),],


                                                                                                                                                                                                                                                                                                                                                ifelse(e=="journalism", final1 <- final[grepl("COMM", final$Subject),], NA)))))))))))))))))

  final1 <- final1[order(final1$ID, final1$Subject,final1$Catalog, final1$Grade),]

  final1 <- final1 %>% distinct(ID, Subject, Catalog, Grade, .keep_all = TRUE) %>% .[,-c(11,12,13,14)]


  ### add in cuin2320 for math 4-8, and core subjects

if(e == "core-ec-6") {final2 <- final[grepl("CUIN", final$Subject),] %>%
    .[grepl("2320", .$Catalog),] %>%
    .[order(.$ID, .$Subject,.$Catalog,.$Grade),] %>%
    .[!duplicated(.$ID),]}

  if(e == "math-4-8") {final2 <- final[grepl("CUIN", final$Subject),] %>%
    .[grepl("2320", .$Catalog),] %>%
    .[order(.$ID, .$Subject,.$Catalog,.$Grade),] %>%
    .[!duplicated(.$ID),]}


ifelse(e == "core-ec-6", final5 <- data.frame(rbind(final2,final1)) %>%.[order(.$ID,.$Subject,.$Catalog),],
                ifelse(e == "math-4-8", final5 <- data.frame(rbind(final2,final1)) %>%.[order(.$ID,.$Subject,.$Catalog),],final1))

ifelse(e == "core-ec-6", final5,
    ifelse(e == "math-4-8", final5, final5 <- final1))


  final5$Grade_pt_p_unit <- final5$Grade_pt_p_unit %>% as.numeric()

  final5$Unit.Taken <- final5$Unit.Taken %>% as.numeric()

  fin1 <- final5 %>% dplyr::group_by(ID) %>% dplyr::mutate(grdpt_unit = Unit.Taken * Grade_pt_p_unit)

  sumgpu1 <- fin1 %>% dplyr::group_by(ID) %>% dplyr::summarise(sum(grdpt_unit))

  sum_u1 <- fin1 %>% dplyr::group_by(ID) %>% dplyr::summarise(sum(Unit.Taken))

  totals1 <- merge(sumgpu1, sum_u1, by = "ID", all.x = TRUE)

  totals1$GPA <- totals1[,2]/totals1[,3]

  tr71 <- merge(fin1, totals1, by = "ID", all.x = TRUE) %>% .[,c(1,13,14)] %>% .[!duplicated(.$ID),]

  names(tr71)[2] <- "ContentHOURS"
  names(tr71)[3] <- "ContentGPA"

  tr8 <- merge(tr7, tr71, by = "ID", all.x = TRUE)

  names(tr8)[13] <- "CumulativeHOURS"
  names(tr8)[14] <- "CumulativeGPA"
  tr8 <- tr8[order(tr8$ID, tr8$Subject,tr8$Catalog),]
  return(tr8)
}
