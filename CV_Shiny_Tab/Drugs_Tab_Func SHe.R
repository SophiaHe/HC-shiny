library(dplyr)
library(lubridate)

# sample search combination 
  current_generic <- "phenytoin"
  current_brand <- "DILANTIN"
  current_rxn <- "Drug level increased"
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
  
  # Import tables with particular search items
  cv_reports <- as.data.frame(tbl(hcopen, sql("SELECT * FROM cv_reports")),n=-1)
  cv_reports_sorted_drg <- if(is.na(date_ini) == FALSE & is.na(date_end) == FALSE){
                              cv_reports %>%
                                select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                            } else if (is.na(date_ini) == TRUE & is.na(date_end) == FALSE){ 
                              date_ini = ymd("19650101")
                              cv_reports %>%
                                select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                            } else if(is.na(date_ini) == FALSE & is.na(date_end) == TRUE){
                              date_end=Sys.Date()
                              cv_reports %>%
                                select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                            } else {
                              date_ini = ymd("19650101")
                              date_end=Sys.Date()
                              cv_reports %>%
                                select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                            }
  
  
  cv_drug_product_ingredients_drg <- if(is.na(current_generic) == FALSE){
                                        as.data.frame(
                                          tbl(hcopen, sql("SELECT * FROM cv_drug_product_ingredients")) %>%
                                            select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME) %>%
                                            filter(ACTIVE_INGREDIENT_NAME == current_generic),
                                          n=-1)
                                      } else {
                                        as.data.frame(
                                          tbl(hcopen, sql("SELECT * FROM cv_drug_product_ingredients")) %>%
                                            select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME),
                                          n=-1)
                                      }

  
  cv_report_drug_indication_drg <- as.data.frame(
                                      tbl(hcopen,sql("SELECT * FROM cv_report_drug_indication")) %>%
                                      select(REPORT_ID, DRUG_PRODUCT_ID, DRUGNAME, INDICATION_NAME_ENG),
                                    n=-1)
  
  cv_report_drug_drg <- if(is.na(current_brand) == FALSE){
                          as.data.frame(
                            tbl(hcopen,sql("SELECT * FROM cv_report_drug")) %>%
                              select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                              filter(DRUGNAME == current_brand),
                            n=-1)
                        } else {
                          as.data.frame(
                            tbl(hcopen,sql("SELECT * FROM cv_report_drug")) %>%
                              select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME),
                            n=-1)
                        }
  cv_report_drug_drg <- cv_report_drug_drg[order(cv_report_drug_drg$DRUG_PRODUCT_ID),]
  

  
  cv_reactions_drg <- if(is.na(current_rxn) == FALSE){
                        as.data.frame(
                          tbl(hcopen,sql("SELECT * FROM cv_reactions")) %>%
                            select(REPORT_ID, PT_NAME_ENG) %>%
                            filter(PT_NAME_ENG == current_rxn),
                          n=-1)
                      } else {
                        as.data.frame(
                          tbl(hcopen,sql("SELECT * FROM cv_reactions")) %>%
                            select(REPORT_ID, PT_NAME_ENG),
                          n=-1)
                      }
  
  # cv_reactions_drg2[cv_reactions_drg2$PT_NAME_ENG == "Drug level increased",] should return zero outcome
  
  
  # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
  # When generic name is unspecified, chart shows top 25 indications associated with specified brand + reaction + date_range
  # When generic & brand names are unspecified, chart shows top 25 indications associated with specified reaction + date_range
  # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
  drugs_tab_indication <- cv_drug_product_ingredients_drg %>%
                          left_join(cv_report_drug_drg) %>%
                            filter(REPORT_ID != "NA") %>%
                          left_join(cv_report_drug_indication_drg) %>% 
                            filter(INDICATION_NAME_ENG != "NA") %>%
                          left_join(cv_reports_sorted_drg) %>%
                            filter(as.character(DATINTRECEIVED_CLEAN) != "NA") %>% # DATINTRECEIVED_CLEAN = NA means there're not within searched time range
                          left_join(cv_reactions_drg) %>%
                            filter(PT_NAME_ENG != "NA") %>%
                          distinct(REPORT_ID)
  
  #return(as.data.frame(drugs_tab_indication))
}

########################################################################################################################
# sample search combination 
current_generic <- NA
current_brand <- "DILANTIN"
current_rxn <- "Drug level increased"
date_ini <- ymd("19730601")
date_end <- ymd("20001231")

# Function to product DF of top_25_drugs in DRUGS Tab
drugs_tab2 <- function(current_generic, current_brand,current_rxn, date_ini, date_end) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  # Import tables with particular search items
  cv_reports <- as.data.frame(tbl(hcopen, sql("SELECT * FROM cv_reports")),n=-1)
  cv_reports_sorted_drg <- if(is.na(date_ini) == FALSE & is.na(date_end) == FALSE){
                              cv_reports %>%
                                select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                            } else if (is.na(date_ini) == TRUE & is.na(date_end) == FALSE){ 
                              date_ini = ymd("19650101")
                              cv_reports %>%
                                select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                            } else if(is.na(date_ini) == FALSE & is.na(date_end) == TRUE){
                              date_end=Sys.Date()
                              cv_reports %>%
                                select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                            } else {
                              date_ini = ymd("19650101")
                              date_end=Sys.Date()
                              cv_reports %>%
                                select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                            }
  
  
  # Used to generate Top 25 drugs (in addition to search item)
  cv_drug_product_ingredients_drg2 <- if(is.na(current_generic) == FALSE){
                                        as.data.frame(
                                          tbl(hcopen, sql("SELECT * FROM cv_drug_product_ingredients")) %>%
                                            select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME) %>%
                                            filter(ACTIVE_INGREDIENT_NAME != current_generic),
                                          n=-1)
                                      } else {
                                        as.data.frame(
                                          tbl(hcopen, sql("SELECT * FROM cv_drug_product_ingredients")) %>%
                                            select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME),
                                          n=-1)
                                      }
  
  
  # Used to generate Top 25 drugs (in addition to search item)
  cv_report_drug_drg2 <- if(is.na(current_brand) == FALSE){
                            as.data.frame(
                              tbl(hcopen,sql("SELECT * FROM cv_report_drug")) %>%
                                select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                                filter(DRUGNAME != current_brand),
                              n=-1)
                          } else {
                            as.data.frame(
                              tbl(hcopen,sql("SELECT * FROM cv_report_drug")) %>%
                                select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME),
                              n=-1)
                          }
  cv_report_drug_drg2 <- cv_report_drug_drg2[order(cv_report_drug_drg2$DRUG_PRODUCT_ID),]
  #cv_report_drug_drg2[cv_report_drug_drg2$DRUGNAME == "DILANTIN",] should return zero outcome

  
  cv_reactions_drg2 <- if(is.na(current_rxn) == FALSE){
                          as.data.frame(
                            tbl(hcopen,sql("SELECT * FROM cv_reactions")) %>%
                              select(REPORT_ID, PT_NAME_ENG) %>%
                              filter(PT_NAME_ENG != current_rxn),
                            n=-1)
                        } else {
                          as.data.frame(
                            tbl(hcopen,sql("SELECT * FROM cv_reactions")) %>%
                              select(REPORT_ID, PT_NAME_ENG),
                            n=-1)
                        }
  # cv_reactions_drg2[cv_reactions_drg2$PT_NAME_ENG == "Drug level increased",] should return zero outcome
  
  
  # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
  # When generic name is unspecified, chart shows top 25 indications associated with specified brand + reaction + date_range
  # When generic & brand names are unspecified, chart shows top 25 indications associated with specified reaction + date_range
  # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
  
  drugs_tab_topdrg <- cv_drug_product_ingredients_drg2 %>%
                      left_join(cv_report_drug_drg2) %>%
                        filter(REPORT_ID != "NA") %>%
                      left_join(cv_reports_sorted_drg) %>%
                        filter(as.character(DATINTRECEIVED_CLEAN) != "NA") %>% # DATINTRECEIVED_CLEAN = NA means there're not within searched time range
                      left_join(cv_reactions_drg2) %>%
                        filter(PT_NAME_ENG != "NA") %>%
                      distinct(REPORT_ID)
 
  #return(head(as.data.frame(drugs_tab_topdrg)))
}

###########################################################################
# format to use the drugs_tab function
a <- drugs_tab(current_generic="phenytoin",current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))  
b <- drugs_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)  



a1 <- drugs_tab(current_generic=NA,current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))

a2 <- drugs_tab(current_generic=NA,current_brand=NA,current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))

a3 <- drugs_tab(current_generic=NA,current_brand=NA,current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20001231"))  

####################################################################
# test drugs_tab2 function
c <-drugs_tab2(current_generic="phenytoin",current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))  
# d <- c %>% distinct(REPORT_ID)

d <- drugs_tab2(current_generic=NA,current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))
d[d$DRUGNAME == "DILANTIN",]
d[d$PT_NAME_ENG == "Drug level increased",] # should produce zero outcome

e <- drugs_tab2(current_generic=NA,current_brand=NA,current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20001231"))
head(e)
e[e$PT_NAME_ENG == "Drug level increased",] # should produce zero outcome

f <- drugs_tab2(current_generic=NA,current_brand=NA,current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20001231")) 
head(f)
