# sample search combination 
#  current_generic <- "ampicillin"
#  current_brand <- "PENBRITIN"
#  current_rxn <- "Urticaria"
#  date_ini <- ymd("19730601")
#  date_end <- ymd("20000101")




#Function to merge tabled used for PATIENTS TAB, which will go in server code, used after current_generic,brand,rxn & date_range are defined
#Tables needed: cv_reports, cv_drug_product_ingredients, cv_report_drug & cv_reactions
#  cv_reports: REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN
#  cv_drug_product_ingredients: DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME
#  cv_report_drug:REPORT_ID, DRUG_PRODUCT_ID, DRUGNAME
#  cv_reactions:REPORT_ID, PT_NAME_ENG

patients_tab <- function(current_generic, current_brand,current_rxn, date_ini, date_end) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  date_ini <- ymd(date_ini)
  date_end <- ymd(date_end)
  
  # Import tables with particular search items with method to deal with unspecified search term
  cv_reports <- as.data.frame(tbl(hcopen, sql("SELECT * FROM cv_reports")),n=-1)
  cv_reports_sorted_pt <- if(is.na(date_ini) == FALSE & is.na(date_end) == FALSE){
                            cv_reports %>%
                              select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                              arrange(REPORT_ID)
                          } else if (is.na(date_ini) == TRUE & is.na(date_end) == FALSE){ 
                            date_ini = ymd("19650101")
                            cv_reports %>%
                              select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                              arrange(REPORT_ID)
                          } else if(is.na(date_ini) == FALSE & is.na(date_end) == TRUE){
                            date_end=Sys.Date()
                            cv_reports %>%
                              select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                              arrange(REPORT_ID)
                          } else {
                            date_ini = ymd("19650101")
                            date_end=Sys.Date()
                            cv_reports %>%
                              select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) > as.POSIXct(date_ini)) %>%
                              filter(as.POSIXct(DATINTRECEIVED_CLEAN) < as.POSIXct(date_end)) %>%
                              arrange(REPORT_ID)
                          }
  
  cv_drug_product_ingredients_pt <- if(is.na(current_generic) == FALSE){
                                        tbl(hcopen, sql("SELECT * FROM cv_drug_product_ingredients")) %>%
                                          select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME) %>%
                                          filter(ACTIVE_INGREDIENT_NAME == current_generic)
                                    } else {
                                        tbl(hcopen, sql("SELECT * FROM cv_drug_product_ingredients")) %>%
                                          select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME)
                                    }
  
  cv_report_drug_pt <- if(is.na(current_brand) == FALSE){
                              tbl(hcopen,sql("SELECT * FROM cv_report_drug")) %>%
                                select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                                filter(DRUGNAME == current_brand)
                       } else {
                              tbl(hcopen,sql("SELECT * FROM cv_report_drug")) %>%
                                select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
                       }
  # cv_report_drug_pt <- cv_report_drug_pt[order(cv_report_drug_pt$DRUG_PRODUCT_ID),]  
  
  cv_reactions_pt <- if(is.na(current_rxn) == FALSE){
                          tbl(hcopen,sql("SELECT * FROM cv_reactions")) %>%
                            select(REPORT_ID, PT_NAME_ENG) %>%
                            filter(PT_NAME_ENG == current_rxn)
                      } else {
                          tbl(hcopen,sql("SELECT * FROM cv_reactions")) %>%
                            select(REPORT_ID, PT_NAME_ENG)
                      }
  
  patients_tab_master <- cv_drug_product_ingredients_pt %>%
                        # left_join(cv_drug_product_ingredients_pt) %>%
                        left_join(cv_report_drug_pt) %>%
                          filter(is.na(REPORT_ID) == FALSE) %>% # some drugs will have the same ingredient but the durg name doesn't match current_brand
                        left_join(cv_reactions_pt) %>%
                          filter(PT_NAME_ENG != "NA") %>%
                        as.data.frame()
  
  patients_tab_master <- patients_tab_master %>%
    left_join(cv_reports_sorted_pt) %>%
      filter(as.character(DATINTRECEIVED_CLEAN) != "NA") # DATINTRECEIVED_CLEAN = NA means there're not within searched time range
                          
  
  #return(as.data.frame(patients_tab_master)) 
}

# format to use the reports_tab function
patients_tab(current_generic="bacitracin",current_brand="NEOSPORIN OINTMENT",current_rxn="Lacrimal disorder",date_ini=ymd("19650101"),date_end=ymd("20151231"))

head(patients_tab(current_generic=NA,current_brand="NEOSPORIN OINTMENT",current_rxn="Lacrimal disorder",date_ini=ymd("19650101"),date_end=ymd("20151231")))
tail(patients_tab(current_generic=NA,current_brand="NEOSPORIN OINTMENT",current_rxn="Lacrimal disorder",date_ini=ymd("19650101"),date_end=ymd("20151231")))

head(patients_tab(current_generic=NA,current_brand=NA,current_rxn="Lacrimal disorder",date_ini=ymd("19650101"),date_end=ymd("20151231")))
tail(patients_tab(current_generic=NA,current_brand=NA,current_rxn="Lacrimal disorder",date_ini=ymd("19650101"),date_end=ymd("20151231")))

head(patients_tab(current_generic=NA,current_brand=NA,current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20151231")))
tail(patients_tab(current_generic=NA,current_brand=NA,current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20151231")))
