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
library(data.table)


########## Codes to fetch top 1000 specific results to be used in dropdown menu ##########

#data frames used in DRUGS tab for top 1000 drugs with most reports submitted
#Fetch top 1000 most-reported ingredients
setwd("~/CV_Shiny_Tab")
source("Dropdown_Menu_Func.R")
topingd <- topingd_func(n=1000)

#Fetch top 1000 most-reported brand/drug names
topbrands <- topdrug_func(n=1000)

#Count the number of times each unique value of field patient.reaction.reactionmeddrapt occurs in records matching the search parameters.
toprxns <- toprxn_func(n=1000)


############### Create function ###################
# function to plot adverse reaction plot
 # adrplot_test <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
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
server <- function(input, output) {
  # Data frame generate reactive function to be used to assign: data <- cv_reports_tab() 
  source("Reports_Tab_Func SHe.R")
  source("Patients_Tab_Func SHe.R")
  source("Drugs_Tab_Func SHe.R")
  source("Reactions_Tab_Func SHe.R")
  
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
  
  cv_search_tab <- reactive({
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
    
    reactions_tab_df <- data.table(names = c("Generic Name:", 
                                             "Brand Name:", 
                                             "Adverse Reaction Term:",
                                             "Date Range:"),
                                   terms = c(current_generic,current_brand,current_rxn,paste(date_ini," to ", date_end)))
    return(reactions_tab_df)
  })
  
  #current_generic = "a"
  #current_brand = NA
  #current_rxn = "c"
  #date_ini = "d"
  #date_end = "f"

  

#############################################  
  
  
  ############## Construct Current_Query_Table for generic name, brand name, adverse reaction term & date range searched ############
  output$current_search <- renderTable({
    data <- cv_search_tab()
    
    reactions_tab_df$terms[is.na(reactions_tab_df$terms) == TRUE] <- "Not Specified"
    
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  #IMPORTANT NOTE!!!!!!!!
  #create master tables based on searched items combination called cv_XX_tab, which will be later used as the table to create charts
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
    # data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
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
    # data <-patients_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
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
    # data <-patients_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    # Age groups frequency table
    age_groups <- ddply(data, "AGE_GROUP_CLEAN",count_func)
    
    gvisPieChart(age_groups, 
                 labelvar = "AGE_GROUP_CLEAN",
                 numvar = "n", 
                 options = list(pieHole = 0.4, pieSliceText='percentage', fontSize=12) )
  })
  
  ################ Create Age Group histogram in Patient tab ##################   
  output$agehist <- renderPlotly({
    age_groups_hist <- age_groups %>% 
      filter(AGE_GROUP_CLEAN != "Unknown") #exclude the unknown
    
    unknown <- age_groups$n[age_groups$AGE_GROUP_CLEAN == "Unknown"]
    
    plottitle <- paste0("Histogram of Patient Ages")
    if(unknown > 0) plottitle <- paste0(plottitle, "<br>(", unknown, "Reports with Unknown Age Group Excluded)")
    
    hist <- ggplot(age_groups_hist, aes(x = AGE_GROUP_CLEAN,  weight=n, fill=AGE_GROUP_CLEAN)) +
      geom_bar()+
      ggtitle(plottitle) + 
      xlab("Age at onset (years)") + 
      ylab("Number of Reports") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    
    ggplotly(hist)
    
  })
  
  # CHECK for missing values in indication, drugnames for those 2 tabs!!!!!!!!!!!!!!
  ################ Create drug plots in Drug tab ################## 
  output$drugplot <- renderPlot({
    
    data <- cv_drug_tab_topdrg()
    # test NEED WORK HERE to optimize the drugs_tab2 process!!!!!!!!!!!!!!!!!!
    # data <-drugs_tab2(current_generic="phenytoin",current_brand="DILANTIN",current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    drugs <- ddply(data,"DRUGNAME", count_func) 
    drugs_sorted <- drugs[order(-drugs$n),]         
    
    p <- ggplot(drugs_sorted[1:25,], aes(x = DRUGNAME, y = n, fill = DRUGNAME)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(drugs_sorted$DRUGNAME[1:25])) + 
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
  
  output$indicationplot <- renderPlot({
    data <- cv_drug_tab_indc()
    # test
    # data <-drugs_tab(current_generic="phenytoin",current_brand="DILANTIN",current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    indications <- ddply(data,"INDICATION_NAME_ENG", count_func) 
    indications_sorted <- indications[order(-indications$n),]
    
    p <- ggplot(indications_sorted[1:25,], aes(x = INDICATION_NAME_ENG, y = n, fill = INDICATION_NAME_ENG)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(indications_sorted$INDICATION_NAME_ENG[1:25])) + 
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
  
  ################ Create Outcomes(all reactions) pie chart in Reaction tab ################## 
  output$outcomeplot <- renderGvis({
    data <- cv_reactions_tab()
    
    #test
    # data <- reactions_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    outcome_results <- ddply(data, "OUTCOME_ENG",count_func)
    
    gvisPieChart(outcome_results, 
                 labelvar = "OUTCOME_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
  })
  
  
  
  
  
  #   output$indicationstable
  #   output$drugclassestable
  #   output$drugstable
  #   output$aerchart
  #   output$aertermstable
  
  }

#######################################################################################


# add UI code HERE !!!!!!!!!


shinyApp(ui, server)
