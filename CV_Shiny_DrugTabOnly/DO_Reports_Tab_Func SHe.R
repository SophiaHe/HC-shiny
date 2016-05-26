# Tables Used: CV_reports (REPORT_ID, DATINTRECEIVED_CLEAN, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
#                          OTHER_MEDICALLY_IMP_COND)
#              Report_Drug (REPORT_ID, DRUGNAME)

#current_brand <- "REMICADE"
#date_ini <- ymd("19730601")
#date_end <- ymd("20160526")

reports_tab <- function(current_brand, date_ini, date_end) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  #cv_reports <- tbl(hcopen, "cv_reports") 
  #cv_report_drug <- tbl(hcopen,"cv_report_drug")
  
  cv_reports_sorted_rp <- 
    cv_reports %>%
    select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
           OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN) %>%
    filter(DATINTRECEIVED_CLEAN >= date_ini) %>%
    filter(DATINTRECEIVED_CLEAN <= date_end)
  
  cv_report_drug_rp <- if(is.na(current_brand) == FALSE){
                        cv_report_drug %>%
                        select(REPORT_ID, DRUGNAME) %>%
                        filter(DRUGNAME == current_brand)
                      } else {
                        cv_report_drug %>%
                        select(REPORT_ID, DRUGNAME)
                      }
  
  reports_tab_master <-  cv_reports_sorted_rp%>%
    inner_join(cv_report_drug_rp) %>%
    as.data.table(n=-1) 
  
  return(reports_tab_master) 
}