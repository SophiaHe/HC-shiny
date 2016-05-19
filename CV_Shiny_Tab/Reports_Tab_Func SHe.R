library(dplyr)
library(lubridate)
library(testthat)


# sample search combination 
#  current_generic <- NA
#  current_brand <- NA
#  current_rxn <- "Nausea"
#  date_ini <- NA 
#  date_end <- NA
# "sulfisoxazole" "GANTRISIN" ymd("20000101") ymd("19730601")

#Function to merge tabled used for REPORTS TAB, which will go in server code, used after current_generic,brand,rxn & date_range are defined
  #Tables needed: cv_reports, cv_drug_product_ingredients, cv_report_drug & cv_reactions
  #  cv_reports: REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
  #             OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN
  #  cv_drug_product_ingredients: DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME
  #  cv_report_drug:REPORT_ID, DRUG_PRODUCT_ID, DRUGNAME
  #  cv_reactions:REPORT_ID, PT_NAME_ENG

reports_tab <- function(current_generic, current_brand,current_rxn, date_ini, date_end) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
 
  
  # Import tables with particular search items with method to deal with unspecified search term
  cv_reports <- tbl(hcopen, "cv_reports")
  date_ini <- ymd(date_ini)
  date_end <- ymd(date_end)
  
  
  cv_reports_sorted_rp <- 
          cv_reports %>%
            select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                   OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN) %>%
            filter(DATINTRECEIVED_CLEAN > date_ini) %>%
            filter(DATINTRECEIVED_CLEAN < date_end)
  
  
                          
  # there are reports with YES to SERIOUSNESS_ENG, but no REASONS cols are filled: 
  #    count the amount of this report & elimiate them when calculating porprotions!!!!!!!!

  cv_drug_product_ingredients_rp <-  if(is.na(current_generic) == FALSE){
                                            tbl(hcopen, "cv_drug_product_ingredients") %>%
                                            select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME) %>%
                                            filter(ACTIVE_INGREDIENT_NAME == current_generic)
                                     } else {
                                         tbl(hcopen, "cv_drug_product_ingredients") %>%
                                           select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME)
                                     }
    
  cv_report_drug_rp <- if(is.na(current_brand) == FALSE){
                            tbl(hcopen,"cv_report_drug") %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                            filter(DRUGNAME == current_brand)
                       } else {
                           tbl(hcopen,"cv_report_drug") %>%
                             select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
                       }
  # cv_report_drug_rp <- cv_report_drug_rp[order(cv_report_drug_rp$DRUG_PRODUCT_ID),]  
  
  cv_reactions_rp <- if(is.na(current_rxn) == FALSE){
                          tbl(hcopen,"cv_reactions") %>%
                          select(REPORT_ID, PT_NAME_ENG) %>%
                          filter(PT_NAME_ENG == current_rxn)
                     } else {
                         tbl(hcopen,"cv_reactions") %>%
                           select(REPORT_ID, PT_NAME_ENG)
                     }
  
  reports_tab_master <- cv_drug_product_ingredients_rp %>%
                        left_join(cv_report_drug_rp) %>%
                          filter(is.na(REPORT_ID) == FALSE) %>% # some drugs will have the same ingredient but the durg name doesn't match current_brand
                        left_join(cv_reports_sorted_rp) %>%
                          filter(as.character(DATINTRECEIVED_CLEAN) != "NA") %>%                    
                        left_join(cv_reactions_rp) %>%
                          filter(PT_NAME_ENG == current_rxn) %>%
                        as.data.frame(n=-1)
  
  #return(as.data.frame(reports_tab_master))  
}

reports_default_tab <- function(current_generic, current_brand,current_rxn, date_ini, date_end){
          # default selection & table merging
          hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
          
          date_ini = ymd("20000101")
          date_end=Sys.Date()
          
          cv_reports_default <- tbl(hcopen, "cv_reports") %>% 
            select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                   OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN) %>%
            filter(DATINTRECEIVED_CLEAN > date_ini) %>%
            filter(DATINTRECEIVED_CLEAN < date_end)
          
          
          cv_drug_product_ingredients_default <- tbl(hcopen,"cv_drug_product_ingredients") %>% 
            select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME, DRUGNAME) %>%
            filter(ACTIVE_INGREDIENT_NAME == "acetaminophen") 
          
          
          cv_report_drug_default <- tbl(hcopen,"cv_report_drug") %>%
            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
          
          cv_reactions_default <- tbl(hcopen,"cv_reactions") %>%
            select(REPORT_ID, PT_NAME_ENG)
          
          default_master <- cv_drug_product_ingredients_default %>%
            left_join(cv_report_drug_default) %>%
            filter(is.na(REPORT_ID) == FALSE) %>%
            left_join(cv_reports_default)%>%
            filter(as.character(DATINTRECEIVED_CLEAN) != "NA") %>%                    
            left_join(cv_reactions_default) %>%
            as.data.frame(n=-1)
}
# format to use the reports_tab function
#g <- reports_tab(current_generic="bacitracin",current_brand=NA,current_rxn="Erythema",date_ini=ymd("19650101"),date_end=ymd("20151231"))

#reports_tab("ampicillin","PENBRITIN","Urticaria",ymd("19730601"),ymd("20150101"))
#reports_tab("bacitracin","NEOSPORIN OINTMENT","Lacrimal disorder",ymd("19650101"),ymd("20151231"))
# "Erythema""NEOSPORIN OINTMENT"
