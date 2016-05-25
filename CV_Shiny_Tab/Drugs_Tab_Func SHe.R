library(dplyr)
library(lubridate)

# sample search combination 
  current_generic <- "acetaminophen"
  current_brand <- NA
  current_rxn <- NA
  date_ini <- ymd("19730601")
  date_end <- ymd("20001231")

  
   
#Function to merge tabled used for DRUGS TAB, which will go in server code, used after current_generic,brand,rxn & date_range are defined
#Tables needed: cv_reports, cv_drug_product_ingredients, cv_report_drug_indication, cv_report_drug & cv_reactions
#  cv_reports: REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN
#  cv_drug_product_ingredients: DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME
#  cv_report_drug_indication:REPORT_ID, DRUG_PRODUCT_ID, DRUGNAME, INDICATION_NAME_ENG
#     -This table can only be linked to cv_reports through REPORT_ID
#     -This table provides the information about indications associated with specific reports
#  cv_report_drug:REPORT_ID, DRUG_PRODUCT_ID, DRUGNAME
#  cv_reactions:REPORT_ID, PT_NAME_ENG

drugs_tab <- function(current_generic, current_brand,current_rxn, date_ini, date_end) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  date_ini <- ymd(date_ini)
  date_end <- ymd(date_end)
  
  # Import tables with particular search items
  cv_reports_sorted_drg <- cv_reports %>%
                            select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                            filter(DATINTRECEIVED_CLEAN >= date_ini) %>%
                            filter(DATINTRECEIVED_CLEAN <= date_end)
  


  cv_drug_product_ingredients_drg <-  if(is.na(current_generic) == FALSE){
                                        cv_drug_product_ingredients %>%
                                          select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME) %>%
                                          filter(ACTIVE_INGREDIENT_NAME == current_generic)
                                      } else {
                                        cv_drug_product_ingredients %>%
                                          select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME)
                                      }
  
  cv_report_drug_indication_drg <- cv_report_drug_indication %>%
                                      select(REPORT_ID, INDICATION_NAME_ENG)
  #, DRUG_PRODUCT_ID, DRUGNAME

  
  cv_report_drug_drg <- if(is.na(current_brand) == FALSE){
                          cv_report_drug %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                            filter(DRUGNAME == current_brand)
                        } else {
                          cv_report_drug %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
                        }
  # cv_report_drug_drg <- cv_report_drug_drg[order(cv_report_drug_drg$DRUG_PRODUCT_ID),]

  cv_reactions_drg <- if(is.na(current_rxn) == FALSE){
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG) %>%
                          filter(PT_NAME_ENG == current_rxn)
                      } else {
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG)
                      }
  
  
  # cv_reactions_drg2[cv_reactions_drg2$PT_NAME_ENG == "Drug level increased",] should return zero outcome
  
  
  # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
  # When generic name is unspecified, chart shows top 25 indications associated with specified brand + reaction + date_range
  # When generic & brand names are unspecified, chart shows top 25 indications associated with specified reaction + date_range
  # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
  drugs_tab_indication <- cv_drug_product_ingredients_drg %>%
                          left_join(cv_report_drug_drg, by="DRUG_PRODUCT_ID") %>%
                            filter(is.na(REPORT_ID) == FALSE) %>%
                          left_join(cv_reports_sorted_drg, by="REPORT_ID") %>%
                            filter(as.character(DATINTRECEIVED_CLEAN) != "NA") %>% 
                          left_join(cv_reactions_drg, by="REPORT_ID") %>%
                            filter(is.na(PT_NAME_ENG) == FALSE) %>%
                          left_join(cv_report_drug_indication_drg, by="REPORT_ID") %>% 
                            filter(is.na(INDICATION_NAME_ENG) == FALSE) %>%
                          as.data.table(n=-1)
  #drugs_tab_indication <- tbl_df(drugs_tab_indication)
  return(drugs_tab_indication)
}


########################################################################################################################
# sample search combination 
#current_generic <- NA
#current_brand <- "DILANTIN"
#current_rxn <- "Drug level increased"
#date_ini <- ymd("19730601")
#date_end <- ymd("20001231")

# Function to product DF of top_25_drugs in DRUGS Tab
drugs_tab2 <- function(current_generic, current_brand,current_rxn, date_ini, date_end) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  date_ini <- ymd(date_ini)
  date_end <- ymd(date_end)
  
  # Import tables with particular search items
  cv_reports_sorted_drg2 <- cv_reports %>%
                            select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                              filter(DATINTRECEIVED_CLEAN >= date_ini) %>%
                              filter(DATINTRECEIVED_CLEAN <= date_end)
  
  
  # Used to generate Top 25 drugs (in addition to search item)
  cv_drug_product_ingredients_drg2 <- if(is.na(current_generic) == FALSE){
                                        cv_drug_product_ingredients %>%
                                          select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME) %>%
                                          filter(ACTIVE_INGREDIENT_NAME == current_generic)
                                      } else {
                                        cv_drug_product_ingredients %>%
                                          select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME)
                                      }
  
  
  # Used to generate Top 25 drugs (in addition to search item)
  cv_report_drug_drg2 <- if(is.na(current_brand) == FALSE){
                            cv_report_drug %>%
                              select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                              filter(DRUGNAME == current_brand)
                          } else {
                            cv_report_drug %>%
                              select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
                          }
  #cv_report_drug_drg2 <- cv_report_drug_drg2[order(cv_report_drug_drg2$DRUG_PRODUCT_ID),]
  #cv_report_drug_drg2[cv_report_drug_drg2$DRUGNAME == "DILANTIN",] should return zero outcome

  
  cv_reactions_drg2 <- if(is.na(current_rxn) == FALSE){
                          cv_reactions %>%
                            select(REPORT_ID, PT_NAME_ENG) %>%
                            filter(PT_NAME_ENG == current_rxn)
                        } else {
                          cv_reactions %>%
                            select(REPORT_ID, PT_NAME_ENG)
                        }
  # cv_reactions_drg2[cv_reactions_drg2$PT_NAME_ENG == "Drug level increased",] should return zero outcome
  
  
  # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
  # When generic name is unspecified, chart shows top 25 indications associated with specified brand + reaction + date_range
  # When generic & brand names are unspecified, chart shows top 25 indications associated with specified reaction + date_range
  # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
  
  drugs_tab_topdrg <- cv_drug_product_ingredients_drg2 %>%
                      left_join(cv_report_drug_drg2, by="DRUG_PRODUCT_ID") %>%
                        filter(is.na(REPORT_ID) == FALSE) %>%
                      left_join(cv_reports_sorted_drg2, by="REPORT_ID") %>%
                        filter(as.character(DATINTRECEIVED_CLEAN) != "NA")  %>%
                      left_join(cv_reactions_drg2, by="REPORT_ID") %>%
                        filter(is.na(PT_NAME_ENG) == FALSE)  %>%
                      as.data.table(n=-1)
  #drugs_tab_topdrg <- tbl_df(drugs_tab_topdrg)
  return(drugs_tab_topdrg)
}

###########################################################################
# format to use the drugs_tab function
#a <- drugs_tab(current_generic="phenytoin",current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))  
#b <- drugs_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)  



#a1 <- drugs_tab(current_generic=NA,current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))

#a2 <- drugs_tab(current_generic=NA,current_brand=NA,current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))

#a3 <- drugs_tab(current_generic=NA,current_brand=NA,current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20001231"))  

####################################################################
# test drugs_tab2 function
#c <-drugs_tab2(current_generic="phenytoin",current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))  
# d <- c %>% distinct(REPORT_ID)

#d <- drugs_tab2(current_generic=NA,current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))
#d[d$DRUGNAME == "DILANTIN",]
#d[d$PT_NAME_ENG == "Drug level increased",] # should produce zero outcome

#e <- drugs_tab2(current_generic=NA,current_brand=NA,current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))
#head(e)
#e[e$PT_NAME_ENG == "Drug level increased",] # should produce zero outcome

#f <- drugs_tab2(current_generic=NA,current_brand=NA,current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20001231")) 
#head(f)
