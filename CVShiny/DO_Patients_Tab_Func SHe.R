# Tables Used: CV_reports (REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN)
#              Report_Drug (REPORT_ID, DRUGNAME)


patients_tab <- function( current_brand, current_rxn,current_gender,current_date_range) { 
  # connect to CV database
  #hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  #cv_reports <- tbl(hcopen, "cv_reports") 
  #cv_report_drug <- tbl(hcopen,"cv_report_drug")
  
  # Import tables with particular search items with method to deal with unspecified search term
  cv_reports_sorted_pt <- if(current_gender != "All"){
                          cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_Y,AGE_GROUP_CLEAN) %>%
                            filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender)
                          } else {
                            cv_reports %>%
                              dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_Y,AGE_GROUP_CLEAN) %>%
                              filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])  
                          }

  cv_report_drug_pt <- if(is.na(current_brand) == FALSE){
                          cv_report_drug %>%
      dplyr::select(REPORT_ID, DRUGNAME) %>%
                            filter(DRUGNAME == current_brand)
                        } else {
                          cv_report_drug %>%
                            dplyr::select(REPORT_ID, DRUGNAME)
                        }
  # cv_report_drug_pt <- cv_report_drug_pt[order(cv_report_drug_pt$DRUG_PRODUCT_ID),]  
  
  cv_reactions_pt <-if(is.na(current_rxn) == FALSE){
                      cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG) %>%
                        filter(PT_NAME_ENG == current_rxn)
                    } else {
                      cv_reactions %>%
                        dplyr::select(REPORT_ID, PT_NAME_ENG)
                    }
  patients_tab_master <-cv_reports_sorted_pt%>%
                          semi_join(cv_report_drug_pt) %>%
                          semi_join(cv_reactions_pt) %>%
                          collect()

  return(patients_tab_master) 
}