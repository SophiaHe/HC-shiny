# Tables used: CV_reports (REPORT_ID, DATINTRECEIVED_CLEAN)
#              CV_Report_Drug (REPORT_ID, DRUGNAME)

# current_brand <- NA
# current_rxn <- NA
# current_date_range <- c(ymd("19650101", ymd("20160527")))

reactions_tab <- function(current_brand,current_rxn,current_gender,current_date_range) { 
  # connect to CV database
  # hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  # Import tables with particular search items with method to deal with unspecified search term
  cv_reports_sorted_rxn <- if(current_gender != "All"){
                            cv_reports %>%
                              select(REPORT_ID, DATINTRECEIVED_CLEAN, OUTCOME_ENG,GENDER_ENG) %>%
                              filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
                              filter(GENDER_ENG == current_gender)
                          } else {
                            cv_reports %>%
                              select(REPORT_ID, DATINTRECEIVED_CLEAN, OUTCOME_ENG,GENDER_ENG) %>%
                              filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])  
                            } 
  
  cv_report_drug_rxn <- if(is.na(current_brand) == FALSE){
                          cv_report_drug %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                            filter(DRUGNAME == current_brand)
                        } else {
                          cv_report_drug %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
                        }
  cv_reactions_rxn <- if(is.na(current_rxn) == FALSE){
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG) %>%
                          filter(PT_NAME_ENG == current_rxn)
                      } else {
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG)
                      }
  
  # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each OUTCOME_ENG 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
  reactions_tab_master <- cv_reports_sorted_rxn %>%
                          semi_join(cv_report_drug_rxn) %>%
                          semi_join(cv_reactions_rxn) %>%
                          select(REPORT_ID, OUTCOME_ENG) %>%
                          collect()
  
  return(reactions_tab_master)
}



drugs_rxn <- function(current_brand,current_date_range){
  if(is.na(current_brand) == FALSE){
    cv_report_drug_rxn <- cv_report_drug %>%
      dplyr::select(REPORT_ID,DRUGNAME) %>%
      filter(DRUGNAME == current_brand)
    
    cv_reactions_rxn <- cv_reactions %>% select(REPORT_ID, PT_NAME_ENG)
    
    drugs_rxn_df <- cv_reactions_rxn %>% inner_join(cv_report_drug_rxn) %>% collect()
    drugs_rxn_result <- dplyr::summarise(group_by(drugs_rxn_df, DRUGNAME,PT_NAME_ENG),count=n_distinct(REPORT_ID))%>% 
      dplyr::arrange(desc(count))%>%
      top_n(10) %>%
      collect()
  } else {
    cv_reactions_rxn  <- cv_reactions %>% select(REPORT_ID, PT_NAME_ENG)
    
    toprxn <- dplyr::summarise(group_by(cv_reactions_rxn, PT_NAME_ENG),count=n_distinct(REPORT_ID))
    drugs_rxn_result <- toprxn %>% dplyr::arrange(desc(count)) %>% top_n(10) %>% select(PT_NAME_ENG, count) %>% collect()
  }
  
  return(drugs_rxn_result)
}