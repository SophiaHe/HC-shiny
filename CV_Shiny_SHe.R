library(shinydashboard)
library(jsonlite)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(plotly)
library(shiny)
library(DT)
library(googleVis)
library(openfda)
library(stringr)
library(plyr)

# connect to CV database
hcopen <- dbConnect(PostgreSQL(), host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")

# list all tables in database
dbListTables(hcopen)

#list fields/variables/cols in particular table
dbListFields(hcopen, 'cv_reports')



##########Check specific columns for missing/blank/NA in tables in hcopen##############
summary(cv_reports)
cv_reports[2,]

freq_REPORTER_TYPE_ENG <- as.data.frame(table(cv_reports$REPORTER_TYPE_ENG,useNA="always"))
freq_SERIOUSNESS_CODE <- as.data.frame(table(cv_reports$SERIOUSNESS_CODE,useNA="always"))
freq_DEATH <- as.data.frame(table(cv_reports$DEATH,useNA="always"))

#Reasons for serious report
as.data.frame(nrow(filter(cv_reports,SERIOUSNESS_CODE == 1 & LIFE_THREATENING == 1 & DISABILITY == 1)))

missing1 <- cv_reports[is.na(cv_reports$REPORTER_TYPE_ENG),]
missing <- cv_reports[is.na(cv_reports$SERIOUSNESS_CODE),]


with(cv_reports,sum(is.na(REPORTER_TYPE_ENG)))


##########Query to fetch specific results from database##########

#data frames used in DRUGS tab for top 25 drugs with most reports submitted
#Fetch & count generic names of drugs 
topdrugs <- fda_query("/drug/event.json") %>% 
  #fda_api_key(dbuijs_api_key) %>%
  fda_count("patient.drug.openfda.generic_name.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec()

#Fetch & count brand name
topbrands <- fda_query("/drug/event.json") %>%
  #fda_api_key(dbuijs_api_key) %>%
  fda_count("patient.drug.openfda.brand_name.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec()

#Count the number of times each unique value of field patient.reaction.reactionmeddrapt occurs in records matching the search parameters.
toprxns <- fda_query("/drug/event.json") %>% 
  #fda_api_key(dbuijs_api_key) %>%
  fda_count("patient.reaction.reactionmeddrapt.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec()


#########Create data frames to construct shiny App#########
reporter_code <- data.table(term = 1:5,
                            label = c("Physician",
                                      "Pharmacist",
                                      "Other Health Professional",
                                      "Lawyer",
                                      "Consumer or Non-Health Professional"))
outcome_code <- data.table(term = 1:6,
                           label = c("Recovererd/resolved",
                                     "Recovering/resolving",
                                     "Not recovered/not resolved",
                                     "Recovered/resolved with sequelae",
                                     "Fatal",
                                     "Unknown"))

sex_code <- data.table(term = 0:2,
                       label = c("Unknown",
                                 "Male",
                                 "Female"))

age_code <- data.table(term = 800:805,
                       label = c("Decade",
                                 "Year",
                                 "Month",
                                 "Week",
                                 "Day",
                                 "Hour"))


############### Create function ###################
# function to plot adverse reaction plot
 adrplot_test <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
 #adrplot_df <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))

count_func <- function(x){
  n <- tally(distinct(x,REPORT_ID))
}
 
 
adrplot <- function(adrplot_test, plottitle){
  adrplot_test <- adrplot_test %>% 
                    mutate( plot_date = floor_date(ymd(adrplot_test$DATINTRECEIVED_CLEAN), "month"))
  nreports <- ddply(adrplot_test,"plot_date",count_func)
  total_reports <- sum(nreports$n)
  

  plottitle <- paste(plottitle, " (", total_reports, " reports)") 
  #plottitle <- paste0(str_replace_all(plottitle, "\\+", " "), " (", total_reports, " reports)") 
  
  plot <- nreports %>%
    ggplot(aes(x = plot_date, y = n)) +
    geom_line(stat = "identity", size = 0.1) +
    stat_smooth(method = "loess", size = 0.1) +
    ggtitle(plottitle) + 
    xlab("Month") + 
    ylab("Number of Reports") +
    theme_bw() +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
}

format1K <- function(x){
  x/1000
}

#topdrugs
#topbrands
#toprxns

############## Server of shiny ########################
# connect to CV database
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")

############### Server Functions ###################
# import searched data into shiny
server <- function(input, output) {
  # Data frame generate reactive function to be used to assign: data <- cv_reports_tab() 
  cv_reports_tab <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      NA,
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    current_date_range <- isolate(input$searchDateRange)
    date_ini <- paste(">=",input$current_date_range[1])
    date_end <- paste("<=",input$current_date_range[2])
    
    reports_tab_df <- reports_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    return(reports_tab_df)
  })
  
  cv_patients_tab <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      NA,
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    current_date_range <- isolate(input$searchDateRange)
    date_ini <- paste(">=",input$current_date_range[1])
    date_end <- paste("<=",input$current_date_range[2])
    
    patients_tab_df <- patients_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    return(patients_tab_df)
  })
  
  cv_drug_tab_indc <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      NA,
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    current_date_range <- isolate(input$searchDateRange)
    date_ini <- paste(">=",input$current_date_range[1])
    date_end <- paste("<=",input$current_date_range[2])
    
    drug_tab_indc_df <- drugs_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    return(drug_tab_indc_df)
  })
  
  cv_drug_tab_topdrg <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      NA,
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    current_date_range <- isolate(input$searchDateRange)
    date_ini <- paste(">=",input$current_date_range[1])
    date_end <- paste("<=",input$current_date_range[2])
    
    drug_tab_topdrg_df <- drugs_tab2(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    return(drug_tab_topdrg_df)
  })
  
  cv_reactions_tab <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      NA,
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    current_date_range <- isolate(input$searchDateRange)
    date_ini <- paste(">=",input$current_date_range[1])
    date_end <- paste("<=",input$current_date_range[2])
    
    reactions_tab_df <- reactions_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    return(reactions_tab_df)
  })
  #############Re-format adverse event onset ages function###############
  ages <- reactive({
    data <- cv_query()
    
    
    #select report received date & count number of report 
    age_years <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "801") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec()  
    if(is.null(age_years)) age_years <- data.table(term = numeric(), count = numeric())
    
    age_decades <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "800") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec()  
    
    if(is.null(age_decades)) age_decades <- data.table(term = numeric(), count = numeric())
    age_decades <- age_decades %>%
      mutate(term = term*10)
    
    age_months <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "802") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec() 
    if(is.null(age_months)) age_months <- data.table(term = numeric(), count = numeric())
    age_months <- age_months %>% 
      mutate(term = term/12)
    
    age_weeks <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "803") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec() 
    if(is.null(age_weeks)) age_weeks <- data.table(term = numeric(), count = numeric())
    age_weeks <- age_weeks %>% 
      mutate(term = term/52)
    
    age_days <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "804") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec() 
    if(is.null(age_days)) age_days <- data.table(term = numeric(), count = numeric())
    age_days <- age_days %>%  mutate(term = term/365)
    
    age_hours <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "805") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec() 
    if(is.null(age_hours)) age_hours <- data.table(term = numeric(), count = numeric())
    age_hours <- age_hours %>%  mutate(term = term/(365*24))
    
    ages <- bind_rows(age_years, 
                      age_decades,
                      age_months,
                      age_weeks,
                      age_days,
                      age_hours) %>%
      count(term, wt = count)
  })

  
  output$search_url <- renderUI({url <- faers_query()$query_url 
  tags$a(url, href=url)
  })
#############################################  
  
  
  #define dropdown menu for generic name, brand name, adverse reaction term & date range
  #### NEED MORE WORK!!!!!!!!!!!!!!!!!!!!!!!!!!
  output$current_search <- renderTable({
    data <- cv_reports_tab()
    data.table(names = c("Generic Name:", 
                         "Brand Name:", 
                         "Adverse Reaction Term:",
                         "Date Range:"),
               values = c(ifelse(is.na(data$current_search[[1]]),
                                 "Not Specified",
                                 data$current_search[[1]]),
                          ifelse(is.na(data$current_search[[2]]),
                                 "Not Specified",
                                 data$current_search[[2]]),
                          ifelse(is.na(data$current_search[[3]]),
                                 "Not Specified (All)",
                                 data$current_search[[3]]),
                          paste(data$current_search[[4]], collapse = " to ")))
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  #IMPORTANT NOTE!!!!!!!!
  #create master tables based on searched items combination called cv_query, which will be later used as the table to create charts
  #reports_tab_df <- reports_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
  #patients_tab_df <- patients_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
  #drug_tab_indc_df <- drugs_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
  #drug_tab_topdrg_df <- drugs_tab2(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
  #reactions_tab_df <- reactions_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
  
  
  
  
  
  ############### Create time plot #####################
  output$timeplot <- renderPlotly({
    
    data <- cv_reports_tab()
    
    # specify the title of time plot based on reactive choice
    title <- ifelse(!is.na(current_generic), current_generic, "NA")
    plottitle <- paste("Drug Adverse Event Reports for", title)
    p <- adrplot(data, plottitle)
    #print(p)
    ggplotly(p)
  })
  
  ############### Create Reporter pie chart ##############  
  output$reporterplot <- renderGvis({
    data <- cv_reports_tab()
    # test
    # data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    reporter_df <- data %>%
      select(REPORT_ID, REPORTER_TYPE_ENG)
    
    # Use ddply & count_func to count number of unique report for each REPORTER_TYPE_ENG
    reporter_results<-as.data.frame(ddply(reporter_df,"REPORTER_TYPE_ENG",count_func))
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == ""] <- "Not Specified"
    
    gvisPieChart(reporter_results, 
                 labelvar = "REPORTER_TYPE_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
    # plot this pie chart
    # plot(gvisPieChart(reporter_results, 
    #                   labelvar = "REPORTER_TYPE_ENG",
    #                   numvar = "n", 
    #                   options = list(pieHole = 0.4)))
  })
  
  ################ Create Serious reports pie chart ##################   
  output$seriousplot <- renderGvis({
    data <- cv_reports_tab()
    
    serious_df <- data %>%
      select(REPORT_ID,SERIOUSNESS_ENG)
    
    serious_results <- as.data.frame(ddply(serious_df,"SERIOUSNESS_ENG",count_func))
    serious_results$SERIOUSNESS_ENG[serious_results$SERIOUSNESS_ENG == ""] <- "Not Specified"
    
    gvisPieChart(serious_results, 
                 labelvar = "SERIOUSNESS_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
    #plot(gvisPieChart(serious_results, 
    #                  labelvar = "SERIOUSNESS_ENG",
    #                  numvar = "n", 
    #                  options = list(pieHole = 0.4)))
  })
  
  ################ Create Serious Reason Reports chart ################## 
  output$seriousreasonsplot <- renderGvis({
    data <- cv_reports_tab()
    # test
    data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    # calculate total number of serious reports
    total_serious <- as.data.frame(ddply(data,"SERIOUSNESS_ENG",count_func))
    total_serious_final <- total_serious$n[total_serious$SERIOUSNESS_ENG == "Yes"]
    
    # create NotSpecified Column to indicate serious report with no reasons specified
    serious_reason <-data%>%
                     filter(SERIOUSNESS_ENG == "Yes" & is.na(DISABILITY)==TRUE & is.na(CONGENITAL_ANOMALY)==TRUE & is.na(LIFE_THREATENING)==TRUE & 
                                 is.na(HOSP_REQUIRED)==TRUE & is.na(OTHER_MEDICALLY_IMP_COND) == TRUE) %>%
                     mutate(NotSpecified = 1)%>%
                     select(REPORT_ID, NotSpecified) 
    serious_reason_df <- serious_reason %>% full_join(data) 
    
    # replace NA with character "NA" & convert them to factor in order to build freq table
    serious_reason_df$NotSpecified[is.na(serious_reason_df$NotSpecified) == TRUE] <- "No"
    serious_reason_df$CONGENITAL_ANOMALY[is.na(serious_reason_df$CONGENITAL_ANOMALY) == TRUE] <- "NA"
    serious_reason_df$HOSP_REQUIRED[is.na(serious_reason_df$HOSP_REQUIRED) == TRUE] <- "NA"
    serious_reason_df$DEATH[is.na(serious_reason_df$DEATH) == TRUE] <- "NA"
    serious_reason_df$DISABILITY[is.na(serious_reason_df$DISABILITY) == TRUE] <- "NA"
    serious_reason_df$LIFE_THREATENING[is.na(serious_reason_df$LIFE_THREATENING) == TRUE] <- "NA"
    serious_reason_df$OTHER_MEDICALLY_IMP_COND[is.na(serious_reason_df$OTHER_MEDICALLY_IMP_COND) == TRUE] <- "NA"
    
    serious_reason_df$NotSpecified <- as.factor(serious_reason_df$NotSpecified)
    serious_reason_df$CONGENITAL_ANOMALY <- as.factor(serious_reason_df$CONGENITAL_ANOMALY)
    serious_reason_df$HOSP_REQUIRED <- as.factor(serious_reason_df$HOSP_REQUIRED)
    serious_reason_df$DEATH <- as.factor(serious_reason_df$DEATH)
    serious_reason_df$DISABILITY <- as.factor(serious_reason_df$DISABILITY)
    serious_reason_df$LIFE_THREATENING <- as.factor(serious_reason_df$LIFE_THREATENING)
    serious_reason_df$OTHER_MEDICALLY_IMP_COND <- as.factor(serious_reason_df$OTHER_MEDICALLY_IMP_COND)
    
    # NotSpecified
    NotSpecified_results <- ddply(serious_reason_df,c("SERIOUSNESS_ENG","NotSpecified"),count_func)
    
    if(any(NotSpecified_results$NotSpecified == 1) ==TRUE){
      NotSpecified_results_final <-NotSpecified_results %>% 
        filter(NotSpecified_results$SERIOUSNESS_ENG == "Yes" & NotSpecified_results$NotSpecified == 1)%>%
        mutate(Reasons = "NotSpecified")%>%
        select(Reasons,n)
    } else {
      Reasons <- I("NotSpecified")
      n <- c(0)
      NotSpecified_results_final <- data.frame(Reasons,n)
    }
  
    # Congenital               
    congenital_results <- ddply(serious_reason_df,c("SERIOUSNESS_ENG","CONGENITAL_ANOMALY"),count_func)
    
    if(any(congenital_results$CONGENITAL_ANOMALY == 1) ==TRUE){
      congenital_results_final <-congenital_results %>% 
        filter(congenital_results$SERIOUSNESS_ENG == "Yes" & congenital_results$CONGENITAL_ANOMALY == 1)%>%
        mutate(Reasons = "Congenital")%>%
        select(Reasons,n)
    } else {
      Reasons <- I("Congenital")
      n <- c(0)
      congenital_results_final <- data.frame(Reasons,n)
    }
    
    # Death
    death_results <- ddply(serious_reason_df, c("SERIOUSNESS_ENG", "DEATH"),count_func)
    
    if(any(death_results$DEATH == 1) ==TRUE){
      death_results_final <-death_results %>% 
        filter(death_results$SERIOUSNESS_ENG == "Yes" & death_results$DEATH == 1)%>%
        mutate(Reasons = "DEATH")%>%
        select(Reasons,n)
    } else {
      Reasons <- I("DEATH")
      n <- c(0)
      death_results_final <- data.frame(Reasons,n)
    }
    
    # Disability
    disabling_results <-  ddply(serious_reason_df, c("SERIOUSNESS_ENG", "DISABILITY"),count_func)
    
    if(any(disabling_results$DISABILITY == 1) ==TRUE){
      disabling_results_final <-disabling_results %>% 
        filter(disabling_results$SERIOUSNESS_ENG == "Yes" & disabling_results$DISABILITY == 1)%>%
        mutate(Reasons = "DISABILITY")%>%
        select(Reasons,n)
    } else {
      Reasons <- I("DISABILITY")
      n <- c(0)
      disabling_results_final <- data.frame(Reasons,n)
    }
    
    # Hospitalization
    hospital_results <- ddply(serious_reason_df,c("SERIOUSNESS_ENG","HOSP_REQUIRED"),count_func)
    
    if(any(hospital_results$HOSP_REQUIRED == 1) ==TRUE ) {
      hospital_results_final <- hospital_results %>% 
        filter(hospital_results$SERIOUSNESS_ENG == "Yes" & hospital_results$HOSP_REQUIRED == 1) %>%
        mutate(Reasons = "Hospitalization") %>%
        select(Reasons,n)
    } else {
      Reasons <- I("Hospitalization")
      n <- c(0)
      hospital_results_final <- data.frame(Reasons,n)
    }
   
    # Lifethreatening                  
    lifethreaten_results <- ddply(serious_reason_df, c("SERIOUSNESS_ENG", "LIFE_THREATENING"),count_func)
    
    if(any(lifethreaten_results$LIFE_THREATENING == 1) ==TRUE){
      lifethreaten_results_final <-disabling_results %>% 
        filter(lifethreaten_results$SERIOUSNESS_ENG == "Yes" & lifethreaten_results$LIFE_THREATENING == 1)%>%
        mutate(Reasons = "LIFE_THREATENING")%>%
        select(Reasons,n)
    } else {
      Reasons <- I("LIFE_THREATENING")
      n <- c(0)
      lifethreaten_results_final <- data.frame(Reasons,n)
    }
    
    # Other
    serother_results <- ddply(serious_reason_df, c("SERIOUSNESS_ENG", "OTHER_MEDICALLY_IMP_COND"),count_func)
    
    if(any(serother_results$OTHER_MEDICALLY_IMP_COND == 1) ==TRUE){
      serother_results_final <-disabling_results %>% 
        filter(serother_results$SERIOUSNESS_ENG == "Yes" & serother_results$OTHER_MEDICALLY_IMP_COND == 1)%>%
        mutate(Reasons = "OTHER_MEDICALLY_IMP_COND")%>%
        select(Reasons,n)
    } else {
      Reasons <- I("OTHER_MEDICALLY_IMP_COND")
      n <- c(0)
      serother_results_final <- data.frame(Reasons,n)
    }
    
    # Combine all SeriousReasons Frequency tables together
    serious_reasons_restults <- congenital_results_final %>%
      full_join(death_results_final) %>%
      full_join(disabling_results_final) %>%
      full_join(hospital_results_final) %>%
      full_join(lifethreaten_results_final) %>%
      full_join(NotSpecified_results_final) %>%
      full_join(serother_results_final)
    
    # Calculate the percentage of each reason
    serious_reasons_restults <- mutate(serious_reasons_restults, percentage = n/total_serious_final*100 %>% signif(digits = 3)) %>% 
      select(-n) %>%
      arrange(desc(percentage))
    
    # GoogleVis plot html: use plot() to graph it
    gvisBarChart(serious_reasons_restults, 
                 xvar = "Reasons",
                 yvar = "percentage", 
                 options = list(title = paste0("Reasons for serious reports (", total_serious_final, " serious reports)"),
                                legend = "{position:'none'}",
                                bars = 'horizontal',
                                #hAxis = "{format:'percent'}",
                                axes= "x: {
                                0: { side: 'top', label: 'Percentage'} 
  }",
                                bar = list(groupWidth =  '90%')
                 )
                 )
  })
  
  ################ Create Gender pie chart in Patient tab ##################  
  output$sexplot <- renderGvis({
    data <- cv_patients_tab()
    # test
    data <-patients_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    #data_drugs_indt <- drugs_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    #data_drugs_topdg <- drugs_tab2(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    #data_reaction <- reactions_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    # replace blank in GENDER_ENG with character "Unknown" & convert it to factor in order to build freq table
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Unknown"
    data$GENDER_ENG <- as.factor(data$GENDER_ENG)
    
    sex_results <- ddply(data, "GENDER_ENG",count_func)
    
    gvisPieChart(sex_results, 
                 labelvar = "GENDER_ENG",
                 numvar = "count", 
                 options = list(pieHole = 0.4, pieSliceText="percentage", fontSize=12))
  })
  
  ################ Create Age Group pie chart in Patient tab ##################      
  output$agegroupplot <- renderGvis({
    #ages <- ages()
    
    data <- cv_patients_tab()
    # test
    data <-patients_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    # Age groups frequency table
    age_groups <- ddply(data, "AGE_GROUP_CLEAN",count_func)
    
    gvisPieChart(age_groups, 
                 labelvar = "AGE_GROUP_CLEAN",
                 numvar = "n", 
                 options = list(pieHole = 0.4, pieSliceText='percentage', fontSize=12) )
  })
  
  ################ Create Age Group histogram in Patient tab ##################   
  output$agehist <- renderPlotly({
    ages <- ages()
    
    age_groups <- mutate(ages,
                         age_group = NA,
                         age_group = ifelse(term <= 28/365, "Neonate", age_group),
                         age_group = ifelse(term <= 2 & term > 28/365, "Infant", age_group),
                         age_group = ifelse(term > 2 & term < 12, "Child", age_group),
                         age_group = ifelse(term >= 12 & term < 18, "Adolescent", age_group),
                         age_group = ifelse(term >= 18 & term < 65, "Adult", age_group),
                         age_group = ifelse(term >= 65, "Elderly", age_group)) %T>%
                         {unknown <<- filter(., term >= 130) %>% nrow()} %>%
      filter(term < 130) #exclude the unknown
    
    plottitle <- paste0("Histogram of Patient Ages")
    if(unknown > 0) plottitle <- paste0(plottitle, "<br>(", unknown, "Reports with Unknown Age Group Excluded)")
    
    hist <- ggplot(age_groups, aes(x = AGE_GROUP_CLEAN, weight = n, fill = age_group)) +
      geom_histogram() +
      ggtitle(plottitle) + 
      xlab("Age at onset (years)") + 
      ylab("Number of Reports") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    
    ggplotly(hist)
    
  })
  ################ Create drug plots in Drug tab ################## 
  output$drugplot <- renderPlot({
    
    data <- faers_query()
    
    drugs <- data$openfda_query %>%
      fda_count("patient.drug.openfda.generic_name.exact") %>%
      fda_limit(1000) %>%
      fda_exec()
    
    if(is.null(drugs)) drugs <- data.table(term = character(), count = numeric())
    if(!is.na(data$current_search[[1]])) drugs <- filter(drugs, !term == data$current_search[[1]])
    
    p <- ggplot(drugs[1:25,], aes(x = term, y = count, fill = term)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(drugs$term[1:25])) + 
      coord_flip() +
      ggtitle("Top 25 Drugs (in addition to search term)") +
      xlab("Drug (generic name)") + 
      ylab("Number of Reports (thousands)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(labels = format1K)
    p
  })
  
  output$drugclassplot <- renderPlot({
    data <- faers_query()
    
    drugclass <- data$openfda_query %>%
      fda_count("patient.drug.openfda.pharm_class_epc.exact") %>%
      fda_limit(1000) %>%
      fda_exec()
    
    if(is.null(drugclass)) drugclass <- data.table(term = character(), count = numeric())
    
    p <- ggplot(drugclass[1:25,], aes(x = term, y = count, fill = term)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(drugclass$term[1:25])) + 
      coord_flip() +
      ggtitle("Top 25 Drug Classes \n(including search term)") +
      xlab("Established Pharmaceutical Class") + 
      ylab("Number of Instances (thousands)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(labels = format1K)
    p
  })
  
  ################ Create Outcomes(all reactions) pie chart in Reaction tab ################## 
  output$outcomeplot <- renderGvis({
    data <- faers_query()
    
    outcome_results <- data$openfda_query %>% 
      fda_count("patient.reaction.reactionoutcome") %>% 
      fda_exec() 
    if(is.null(outcome_results)) outcome_results <- data.table(term = numeric(), count = numeric())
    
    outcome_results <- outcome_results %>%  
      left_join(outcome_code) %>%
      select(label, count)
    
    gvisPieChart(outcome_results, 
                 labelvar = "label",
                 numvar = "count", 
                 options = list(pieHole = 0.4))
  })
  
  output$indicationplot <- renderPlot({
    data <- faers_query()
    
    indications <- data$openfda_query %>%
      fda_count("patient.drug.drugindication.exact") %>%
      fda_limit(1000) %>%
      fda_exec()
    
    if(is.null(indications)) indications <- data.table(term = character(), count = numeric())
    
    p <- ggplot(indications[1:25,], aes(x = term, y = count, fill = term)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(indications$term[1:25])) + 
      coord_flip() +
      ggtitle("Top 25 Indications (All Drugs)") +
      xlab("Indication") + 
      ylab("Number (thousands)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(labels = format1K)
    p
  })
  
  
  
  #   output$indicationstable
  #   output$drugclassestable
  #   output$drugstable
  #   output$aerchart
  #   output$aertermstable
  
  }

#######################################################################################


############ UI for shiny ################
ui <- dashboardPage(
  dashboardHeader(title = "Shiny FAERS (v0.04)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    selectizeInput("search_generic", 
                   "Generic Name (Active Ingredient)", 
                   topdrugs$term,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    selectizeInput("search_brand", 
                   "Brand Name (US Trade Name)",
                   topbrands$term,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    selectizeInput("search_rxn", 
                   "Adverse Event Term",
                   toprxns$term,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    dateRangeInput("searchDateRange", 
                   "Date Range", 
                   start = "2000-01-01", 
                   startview = "year"),
    actionButton("searchButton", "Search"),
    tags$br(),
    tags$h3(strong("Current Query:")),
    tableOutput("current_search")
  ),
  
  dashboardBody(
    fluidRow(
      box(plotlyOutput(outputId = "timeplot"),
          tags$br(),
          tags$p("Reports by month from Canada Vigilance Adverse Reaction Online Database. 
                 Trendline is a local non-parametric regression calculated with the LOESS model. 
                 The shaded area is an approximation of the 95% confidence interval of the regression.")
          #tags$p("Search URL:"),
          #uiOutput(outputId = "search_url"),
          #width = 12
          )
      ),
    tabItems(
      tabItem(tabName = "reportdata",
              fluidRow(
                box(htmlOutput("reporterplot"), 
                    tags$br(),
                    tags$p("Qualification of the person who filed the report."),
                    tags$p("Unknown is the number of reports without the primarysource.qualification field."),
                    title = tags$h2("Reporter"), width = 4),
                box(htmlOutput("seriousplot"), 
                    tags$br(),
                    tags$p("Reports marked as serious."),
                    title = tags$h2("Serious reports"), width = 4),
                box(htmlOutput("seriousreasonsplot"), 
                    tags$br(),
                    tags$p("Total sums to more than 100% because reports can be marked serious for multiple reasons."),
                    title = tags$h2("Reasons for serious reports"), width = 4)
                #box(htmlOutput("countryplot"), 
                    #tags$br(),
                    #tags$p("Country the reaction(s) occurred in. This is not necessarily the same country the report was received from."),
                    #title = tags$h2("Country"), width = 4)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                box(htmlOutput("sexplot"),
                    tags$br(),
                    tags$p("Unknown includes both reports explicitly marked unknown and reports with no gender information."),
                    title = tags$h2("Gender"), width = 4),
                box(htmlOutput("agegroupplot"),
                    tags$br(),
                    tags$p("Unknown includes reports with no age information."), 
                    title = tags$h2("Age Groups"), width = 4),
                box(plotlyOutput("agehist"), title = tags$h2("Age Histogram"), width = 4)
              )
      ),
      tabItem(tabName = "drugdata",
              fluidRow(
                box(plotOutput("indicationplot"),
                    tags$br(),
                    tags$p("This plot includes all indications for all drugs associated with the matching reports.
                           The open.fda.gov search API does not allow searching or filtering within drugs.
                           The search query filters unique reports, which may have one or more drugs associated with them.
                           It is not currently possible to search for only those indications associated with a specific drug.
                           "), width = 4),
                box(plotOutput("drugplot"),
                    tags$br(),
                    tags$p("This plot includes all drugs associated with the matching reports, except the search term.
                           The open.fda.gov search API does not allow searching or filtering within drugs.
                           The search query filters unique reports, which may have one or more drugs associated with them.
                           It is not currently possible to retrieve correlations between drugs."), width = 4),
                box(plotOutput("drugclassplot"),
                    tags$br(),
                    tags$p("This plot includes all drug classes associated with the matching reports, including the search term.
                           The total number of instances for each class will be geater 
                           than the number of reports when reports include more than one drug of the same class.
                           The open.fda.gov search API does not allow searching or filtering within drugs.
                           The search query filters unique reports, which may have one or more drugs associated with them.
                           It is not currently possible to retrieve correlations between drugs."), width = 4)
                    )
                    ),
      tabItem(tabName = "rxndata",
              fluidRow(
                box(htmlOutput("outcomeplot"), title = tags$h2("Outcomes (all reactions)"))
              )
      ),
      tabItem(tabName = "aboutinfo",
              tags$p("Data provided by the Canada Vigilance Adverse Reaction Online Database (http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php)"),
              tags$h2("About page goes here")
      )
              )
), 
skin = "blue"
)
###########################################################################


shinyApp(ui, server)
