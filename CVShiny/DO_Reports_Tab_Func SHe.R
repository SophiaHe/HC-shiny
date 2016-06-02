# Tables Used: CV_reports (REPORT_ID, DATINTRECEIVED_CLEAN, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
#                          OTHER_MEDICALLY_IMP_COND)
#              Report_Drug (REPORT_ID, DRUGNAME)

current_brand <- NA
current_rxn <- NA
current_gender <- "Male"
current_date_range <- c(ymd("19650101", ymd("20160527")))


reports_tab <- function(current_brand,current_rxn,current_gender,current_date_range) { 
  # connect to CV database
  #hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  #escape.POSIXt <- dplyr:::escape.Date
  #cv_reports <- tbl(hcopen, "cv_reports")
  #cv_report_drug <- tbl(hcopen,"cv_report_drug")
  #cv_reactions <- tbl(hcopen,"cv_reactions")
  
  #ptm <- proc.time()
  cv_reports_sorted_rp <- if(current_gender != "All"){
                          cv_reports %>%
                          select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                                 OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN, GENDER_ENG) %>%
                          filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender)
                          } else {
                            cv_reports %>%
                              select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                                     OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN,GENDER_ENG) %>%
                              filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
                                      }
      
                
  #proc.time() - ptm
  cv_report_drug_rp <- if(is.na(current_brand) == FALSE){
                        cv_report_drug %>%
                        select(REPORT_ID, DRUGNAME) %>%
                        filter(DRUGNAME == current_brand)
                      } else {
                        cv_report_drug %>%
                        select(REPORT_ID, DRUGNAME)
                      }
  cv_reactions_rp <- if(is.na(current_rxn) == FALSE){
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG) %>%
                          filter(PT_NAME_ENG == current_rxn)
                      } else {
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG)
                      }
 
  reports_tab_master <-  cv_reports_sorted_rp%>%
                          inner_join(cv_report_drug_rp) %>%
                          semi_join(cv_reactions_rp) %>%
                          collect()
  
 
  
  return(reports_tab_master) 
}