library(dplyr)
library(lubridate)
library(testthat)

#server <- function(input, output) {

  #create master tables based on searched items combination(cv_query), which will be later used as the table to create charts
  cv_query <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      NA,
                                      input$search_generic)) # %>%str_replace_all(" ", "+")
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) #  %>% str_replace_all(" ", "+")
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) #  %>% str_replace_all(" ", "+")
    current_date_range <- isolate(input$searchDateRange)
    querydate_ini <- paste(">=",input$current_date_range[1])
    querydate_end <- paste("<=",input$current_date_range[2])
    
    # implement reports_tab function to construct data frame needed to make reports tab
    reports_tab(current_generic, current_brand,current_rxn, querydate_ini, querydate_end)
  })
#}   


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
  cv_reports <- as.data.frame(tbl(hcopen, sql("SELECT * FROM cv_reports")),n=-1)
  cv_reports_sorted_rp <- if(is.na(date_ini) == FALSE & is.na(date_end) == FALSE){
                              cv_reports %>%
                              select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                                                OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                              arrange(REPORT_ID)
                          } else if (is.na(date_ini) == TRUE & is.na(date_end) == FALSE){ 
                              date_ini = ymd("19650101")
                              cv_reports %>%
                                select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                                       OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                          } else if(is.na(date_ini) == FALSE & is.na(date_end) == TRUE){
                              date_end=Sys.Date()
                              cv_reports %>%
                                select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                                       OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                          } else {
                              date_ini = ymd("19650101")
                              date_end=Sys.Date()
                              cv_reports %>%
                                select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                                       OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                                filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                                arrange(REPORT_ID)
                          }
                          
  # there are reports with YES to SERIOUSNESS_ENG, but no REASONS cols are filled: 
  #    count the amount of this report & elimiate them when calculating porprotions!!!!!!!!

  cv_drug_product_ingredients_rp <-  if(is.na(current_generic) == FALSE){
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
    
  cv_report_drug_rp <- if(is.na(current_brand) == FALSE){
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
  cv_report_drug_rp <- cv_report_drug_rp[order(cv_report_drug_rp$DRUG_PRODUCT_ID),]  
  
  cv_reactions_rp <- if(is.na(current_rxn) == FALSE){
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
  
  reports_tab_master <- cv_drug_product_ingredients_rp %>%
                        left_join(cv_drug_product_ingredients_rp) %>%
                        left_join(cv_report_drug_rp) %>%
                          filter(REPORT_ID != "NA") %>% # some drugs will have the same ingredient but the durg name doesn't match current_brand
                        left_join(cv_reports_sorted_rp) %>%
                          filter(as.character(DATINTRECEIVED_CLEAN) != "NA")%>% # DATINTRECEIVED_CLEAN = NA means there're not within searched time range
                        left_join(cv_reactions_rp) %>%
                          filter(PT_NAME_ENG == current_rxn)
  #return(as.data.frame(reports_tab_master))  
}

# format to use the reports_tab function
g <- reports_tab(current_generic="bacitracin",current_brand=NA,current_rxn="Erythema",date_ini=ymd("19650101"),date_end=ymd("20151231"))

#reports_tab("ampicillin","PENBRITIN","Urticaria",ymd("19730601"),ymd("20150101"))
#reports_tab("bacitracin","NEOSPORIN OINTMENT","Lacrimal disorder",ymd("19650101"),ymd("20151231"))
# "Erythema""NEOSPORIN OINTMENT"
