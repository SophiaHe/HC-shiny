library(dplyr)
library(plyr)

############## Function to create top 1000 terms included in the dropdown menu #############
topingd_func <- function(n){
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  cv_reports_dm <- tbl(hcopen, sql("SELECT * FROM cv_reports")) %>% select(REPORT_ID)
  
  cv_report_drug_dm <- tbl(hcopen, sql("SELECT * FROM cv_report_drug")) %>%
    select(REPORT_ID, DRUG_PRODUCT_ID)
  
  cv_drug_product_ingredients_dm <- tbl(hcopen, sql("SELECT * FROM cv_drug_product_ingredients")) %>%
    select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME)
  
  ingd_master <-  cv_reports_dm %>% left_join(cv_report_drug_dm) %>% left_join(cv_drug_product_ingredients_dm) 
  
  topingd <-  dplyr::summarise(group_by(ingd_master, ACTIVE_INGREDIENT_NAME),count=n_distinct(REPORT_ID)) %>% as.data.frame()
  topingd_final <-  topingd %>% arrange(desc(count)) %>% top_n(n) %>% select(ACTIVE_INGREDIENT_NAME)
}

topdrug_func <- function(n){
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  cv_reports_dm <- tbl(hcopen, sql("SELECT * FROM cv_reports")) %>% select(REPORT_ID)
  
  cv_report_drug_dm <- tbl(hcopen, sql("SELECT * FROM cv_report_drug")) %>%
    select(REPORT_ID, DRUGNAME)
  
  drugs_master <-cv_reports_dm %>% left_join(cv_report_drug_dm) 
  
  topdrugs <- dplyr::summarise(group_by(drugs_master, DRUGNAME),count=n_distinct(REPORT_ID)) %>% as.data.frame()
  topdrugs_final <- topdrugs %>% arrange(desc(count)) %>% top_n(n) %>% select(DRUGNAME)
} 

toprxn_func <- function(n){
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  cv_reports_dm <- tbl(hcopen, sql("SELECT * FROM cv_reports")) %>% select(REPORT_ID)
  
  cv_reactions_dm <- tbl(hcopen, sql("SELECT * FROM cv_reactions")) %>% select(REPORT_ID, PT_NAME_ENG)
  
  rxn_master <- cv_reports_dm %>% left_join(cv_reactions_dm) 
  
  toprxn <- dplyr::summarise(group_by(rxn_master, PT_NAME_ENG),count=n_distinct(REPORT_ID)) %>% as.data.frame()
  toprxn_final <- toprxn %>% arrange(desc(count)) %>% top_n(n) %>% select(PT_NAME_ENG)
}