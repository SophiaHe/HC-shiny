# sample search combination 
  current_generic <- NA
  current_brand <- NA
  current_rxn <- "Urticaria"
  date_ini <- ymd("19730601")
  date_end <- ymd("20000101")

#Function to merge tabled used for REACTIONS TAB, which will go in server code, used after current_generic,brand,rxn & date_range are defined
#Tables needed: cv_reports, cv_drug_product_ingredients, cv_report_drug & cv_reactions
#  cv_reports: REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN, OUTCOME_ENG
#  cv_drug_product_ingredients: DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME
#  cv_report_drug:REPORT_ID, DRUG_PRODUCT_ID, DRUGNAME
#  cv_reactions:REPORT_ID, PT_NAME_ENG

reactions_tab <- function(current_generic, current_brand,current_rxn, date_ini, date_end) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")

  # Import tables with particular search items with method to deal with unspecified search term
  cv_reports_sorted_rxn <- cv_reports %>%
                          select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN, OUTCOME_ENG) %>%
                          filter(DATINTRECEIVED_CLEAN >= date_ini) %>%
                          filter(DATINTRECEIVED_CLEAN <= date_end)
  
  
  cv_drug_product_ingredients_rxn <- if(is.na(current_generic) == FALSE){
                                      cv_drug_product_ingredients %>%
                                        select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME) %>%
                                        filter(ACTIVE_INGREDIENT_NAME == current_generic)
                                    } else {
                                      cv_drug_product_ingredients %>%
                                        select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME)
                                    }
                                      
  cv_report_drug_rxn <- if(is.na(current_brand) == FALSE){
                          cv_report_drug %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                            filter(DRUGNAME == current_brand)
                        } else {
                          cv_report_drug %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
                        }
  # cv_report_drug_rxn <- cv_report_drug_rxn[order(cv_report_drug_rxn$DRUG_PRODUCT_ID),]  
  
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
  reactions_tab_master <- cv_drug_product_ingredients_rxn %>%
                          left_join(cv_report_drug_rxn) %>%
                            filter(is.na(REPORT_ID) == FALSE) %>% # some drugs will have the same ingredient but the durg name doesn't match current_brand
                          left_join(cv_reports_sorted_rxn) %>%
                            filter(as.character(DATINTRECEIVED_CLEAN) != "NA") %>%
                          left_join(cv_reactions_rxn) %>%
                            filter(is.na(PT_NAME_ENG) == FALSE) %>%
                          as.data.frame(n=-1)
 
  return(reactions_tab_master)
}

# format to use the reactions_tab function
#reactions_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20001231"))

#head(reactions_tab(current_generic=NA,current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20001231")))
#tail(reactions_tab(current_generic=NA,current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20001231")))

#head(reactions_tab(current_generic=NA,current_brand=NA,current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20001231")))
#head(reactions_tab(current_generic=NA,current_brand=NA,current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20001231")))
