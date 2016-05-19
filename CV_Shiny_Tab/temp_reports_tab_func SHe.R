
reports_tab <- function(current_generic, current_brand,current_rxn, date_ini, date_end) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  
  
  # Import tables with particular search items with method to deal with unspecified search term
  
  #date_ini <- as.POSIXct(ymd(date_ini))
  #date_end <- as.POSIXct(ymd(date_end))
  
  #date_ini <- "19730101"
  #date_end <- "20150101"
  #current_generic <- "acetaminophen"
  #current_brand <- NA
  #current_rxn<-"Rash"
  #escape.POSIXt <- dplyr:::escape.Date
  
  cv_reports_sorted_rp <- 
    cv_reports %>%
    select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
           OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN) %>%
    filter(DATINTRECEIVED_CLEAN >= date_ini) %>%
    filter(DATINTRECEIVED_CLEAN <= date_end)
  
  
  
  # there are reports with YES to SERIOUSNESS_ENG, but no REASONS cols are filled: 
  #    count the amount of this report & elimiate them when calculating porprotions!!!!!!!!
  
  cv_drug_product_ingredients_rp <-  if(is.na(current_generic) == FALSE){
    cv_drug_product_ingredients %>%
      select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME) %>%
      filter(ACTIVE_INGREDIENT_NAME == current_generic)
  } else {
    cv_drug_product_ingredients %>%
      select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME)
  }
  
  cv_report_drug_rp <- if(is.na(current_brand) == FALSE){
    cv_report_drug %>%
      select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
      filter(DRUGNAME == current_brand)
  } else {
    cv_report_drug %>%
      select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
  }
  # cv_report_drug_rp <- cv_report_drug_rp[order(cv_report_drug_rp$DRUG_PRODUCT_ID),]  
  
  cv_reactions_rp <- if(is.na(current_rxn) == FALSE){
    cv_reactions %>%
      select(REPORT_ID, PT_NAME_ENG) %>%
      filter(PT_NAME_ENG == current_rxn)
  } else {
    cv_reactions %>%
      select(REPORT_ID, PT_NAME_ENG)
  }
  
  reports_tab_master <- cv_drug_product_ingredients_rp %>%
    left_join(cv_report_drug_rp) %>%
    filter(is.na(REPORT_ID) == FALSE) %>% # some drugs will have the same ingredient but the durg name doesn't match current_brand
    left_join(cv_reports_sorted_rp) %>%
    filter(as.character(DATINTRECEIVED_CLEAN) != "NA") %>%                    
    left_join(cv_reactions_rp) %>% 
    filter(is.na(PT_NAME_ENG) == FALSE) %>% as.data.frame() 
  
  #return(as.data.frame(reports_tab_master))  
}