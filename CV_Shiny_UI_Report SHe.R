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
# library(openfda)
library(stringr)
library(plyr)
library(data.table)


########## Codes to fetch top 1000 specific results to be used in dropdown menu ###############

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
    select(REPORT_ID,DATINTRECEIVED_CLEAN) %>%
    mutate( plot_date = floor_date(ymd(adrplot_test$DATINTRECEIVED_CLEAN), "month")) %>%
    select(REPORT_ID,plot_date)
  
  nreports <- ddply(adrplot_test,"plot_date",count_func)
  total_reports <- sum(nreports$n)
  
  
  plottitle <- paste(plottitle, " (", total_reports, " reports)") 
 
  
  plot <- nreports %>%
    ggplot(aes(x = plot_date, y = n)) +
    geom_line(stat = "identity", size = 0.1) +
    stat_smooth(method = "loess", size = 0.1) +
    ggtitle(plottitle) + 
    xlab("Month") + 
    ylab("Number of Reports") +
    theme_bw() +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
    
 # ggplotly(p= plot)
  
  
}

format1K <- function(x){
  x/1000
}


############ UI for REPORT Tab shiny ################
ui <- dashboardPage(
  dashboardHeader(title = "Shiny FAERS (v0.04)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")), 
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o"))

    ),
    selectizeInput("search_generic", 
                   "Generic Name (Active Ingredient)", 
                   topingd$ACTIVE_INGREDIENT_NAME,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    selectizeInput("search_brand", 
                   "Brand Name (US Trade Name)",
                   topbrands$DRUGNAME,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    selectizeInput("search_rxn", 
                   "Adverse Event Term",
                   toprxns$PT_NAME_ENG,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    #dateRangeInput("searchDateRange", 
    #               "Date Range", 
    #               start = "1973-01-01", 
    #               end = Sys.Date(),
    #               startview = "year",
    #               format = "yyyy-mm-dd"),
    dateInput("search_date_ini",
              label = 'Date initial input: yyyy-mm-dd',
              value = "1973-01-01"
    ),
    
    dateInput("search_date_end",
              label = 'Date end input: yyyy-mm-dd',
              value = Sys.Date()
    ),
    
    actionButton("searchButton", "Search"),
    tags$br(),
    tags$h3(strong("Current Search:")),
    tableOutput("current_search")
  ), 
  
  dashboardBody(
    fluidRow(
      box(plotlyOutput(outputId = "timeplot"),
          tags$br(),
          tags$p("Reports by month from Canada Vigilance Adverse Reaction Online Database. 
                 Trendline is a local non-parametric regression calculated with the LOESS model. 
                 The shaded area is an approximation of the 95% confidence interval of the regression."),
          width = 12
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
                box(htmlOutput("outputReports"), 
                    tags$br(),
                    #tags$p("Country the reaction(s) occurred in. This is not necessarily the same country the report was received from."),
                    title = tags$h2("dataset used"), width = 4)
              )
      ),
      tabItem(tabName = "rxndata",
              fluidRow(
                box(htmlOutput("outcomeplot"), title = tags$h2("Outcomes (all reactions)"))
              )
      ) # more tabs added here!
    )
  ), 
  skin = "blue"
)


############## Server of shiny ########################
# connect to CV database
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")

#options(shiny.error = browser)
options(shiny.trace = TRUE, shiny.reactlog=TRUE)

# Temperary solution: fetch all tables to local and run functions on them
cv_reports <- tbl(hcopen, "cv_reports") 
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients")
cv_report_drug <- tbl(hcopen,"cv_report_drug")
cv_reactions <- tbl(hcopen,"cv_reactions") 
cv_report_drug_indication <- tbl(hcopen,"cv_report_drug_indication")

############### Server Functions ###################
server <- function(input, output) {
  # Data frame generate reactive function to be used to assign: data <- cv_reports_tab() 
  #setwd("~/CV_Shiny_Tab")
  #source("Patients_Tab_Func SHe.R")
  #source("Drugs_Tab_Func SHe.R")
  #source("Reactions_Tab_Func SHe.R")
  
  cv_reports_tab <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      "acetaminophen",
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    date_ini <- isolate(as.POSIXct(input$search_date_ini))
    date_end <- isolate(as.POSIXct(input$search_date_end))
    #current_date_range <- isolate(input$searchDateRange)
    #date_ini <- ifelse(input$searchDateRange[1]== "",as.POSIXct(ymd("19730101")), input$searchDateRange[1])
    #date_end <- ifelse(input$searchDateRange[2]== "",as.POSIXct(ymd("20150101")), input$searchDateRange[2])

    setwd("~/CV_Shiny_Tab")
    source("temp_Reports_Tab_Func SHe.R")
    reports_tab_df <- reports_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    
    return(reports_tab_df)
  })
  
  # sample datasets of what is being graphed/used
  #output$outputReports <- renderTable({
  #  cv_reports_tab()[1:4,c("ACTIVE_INGREDIENT_NAME","DRUGNAME","DATINTRECEIVED_CLEAN","PT_NAME_ENG")]
  #})
  
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  cv_search_tab <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      "acetaminophen",
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    #current_date_range <- isolate(input$searchDateRange)
    date_ini <- isolate(input$search_date_ini)
    date_end <- isolate(input$search_date_end)

    
    search_tab_df <- data.frame(names = c("Generic Name:", 
                                          "Brand Name:", 
                                          "Adverse Reaction Term:",
                                          "Date Range:"),
                                terms = c(current_generic,current_brand,current_rxn,paste(date_ini, " to ",date_end)),
                                stringsAsFactors=FALSE)
    search_tab_df$terms[is.na(search_tab_df$terms) == TRUE] <- "Not Specified"
    return(search_tab_df)
  })
  
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  cv_patients_tab <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      "acetaminophen",
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    date_ini <- isolate(as.POSIXct(input$search_date_ini))
    date_end <- isolate(as.POSIXct(input$search_date_end))
    #date_ini <- ifelse(input$searchDateRange[1]== "",as.POSIXct(ymd("19730101")), input$searchDateRange[1])
    #date_end <- ifelse(input$searchDateRange[2]== "",as.POSIXct(ymd("20150101")), input$searchDateRange[2])
    
    setwd("~/CV_Shiny_Tab")
    source("Patients_Tab_Func SHe.R")
    
    patients_tab_df <- patients_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    
    return(patients_tab_df)
  })
  
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  cv_drug_tab_topdrg <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      "acetaminophen",
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    date_ini <- isolate(as.POSIXct(input$search_date_ini))
    date_end <- isolate(as.POSIXct(input$search_date_end))
    
    setwd("~/CV_Shiny_Tab")
    source("Drugs_Tab_Func SHe.R")
    
    drugs_tab_df2 <- drugs_tab2(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    
    
    #drugs <- ddply(drugs_tab_df,"DRUGNAME", count_func) 
    #drugs_sorted <- drugs[order(desc(drugs$n)),] 
    #drugs_sorted <- drugs %>% arrange(desc(n)) %>% top_n(25)
    
    return(drugs_tab_df2)
  })
  
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  cv_drug_tab_indc <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      "acetaminophen",
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    date_ini <- isolate(as.POSIXct(input$search_date_ini))
    date_end <- isolate(as.POSIXct(input$search_date_end))
    
    setwd("~/CV_Shiny_Tab")
    source("Drugs_Tab_Func SHe.R")
    
    drugs_tab_df <- drugs_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    #drugs_tab_df2 <- setDF(drugs_tab_df2)
    
    
    
    #drugs_tab_df2 <- tbl_df(drugs_tab_df2)
    #indications <- dplyr::summarise(group_by(drugs_tab_df2, INDICATION_NAME_ENG),count=n_distinct(REPORT_ID))
    #indications_sorted <- indications %>% arrange(desc(n)) %>% top_n(n=25)
    
    return(drugs_tab_df)
  })
  
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  cv_reactions_tab <- reactive({
    input$searchButton
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_generic <- isolate(ifelse(input$search_generic == "",
                                      "acetaminophen",
                                      input$search_generic)) 
    current_brand <- isolate(ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    date_ini <- isolate(as.POSIXct(input$search_date_ini))
    date_end <- isolate(as.POSIXct(input$search_date_end))
    
    setwd("~/CV_Shiny_Tab")
    source("Reactions_Tab_Func SHe.R")
    
    reactions_tab_df <- reactions_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
   
    return(reactions_tab_df)
  })
  
  
  ############## Construct Current_Query_Table for generic name, brand name, adverse reaction term & date range searched ############
  output$current_search <- renderTable({
    data_search <- cv_search_tab()
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  ############### Create time plot #####################
  output$timeplot <- renderPlotly({

      data <- cv_reports_tab()
      generic_selected <- data$ACTIVE_INGREDIENT_NAME[1] 
    
    
    # specify the title of time plot based on reactive choice
    title <- ifelse(!is.na(generic_selected), generic_selected, "NA")
    plottitle <- paste("Drug Adverse Event Reports for", title)
    p <- adrplot(adrplot_test = data, plottitle = plottitle)
    print(p)
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
    reporter_results<-ddply(reporter_df,"REPORTER_TYPE_ENG",count_func)
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
                                0: { side: 'top', label: 'Count'} 
  }",
                                bar = list(groupWidth =  '90%')
                 )
    )
  })
  
  ################ Create Gender pie chart in Patient tab ##################  
  output$sexplot <- renderGvis({
    data <- cv_patients_tab()
    
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
    data <- cv_patients_tab()
   
    # Age groups frequency table
    age_groups <- ddply(data, "AGE_GROUP_CLEAN",count_func)
    
    gvisPieChart(age_groups, 
                 labelvar = "AGE_GROUP_CLEAN",
                 numvar = "n", 
                 options = list(pieHole = 0.4, pieSliceText='percentage', fontSize=12) )
  })
  
  ################ Create Age Group histogram in Patient tab ##################   
  output$agehist <- renderPlotly({
    data <- cv_patients_tab()
    age_groups <- ddply(data, "AGE_GROUP_CLEAN",count_func)
    
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
            theme(plot.title = element_text(lineheight=.8, size = rel(1),face="bold")) + 
            theme(axis.title.x = element_text(size = rel(0.8)))
    
    print(hist)
    ggplotly(hist)
    
  })
  
  ################ Create drug plots in Drug tab ################## 
  output$drugplot <- renderPlot({
    
    data <- cv_drug_tab_topdrg()
    # test
    # data <-drugs_tab2(current_generic="phenytoin",current_brand=NA,current_rxn="Drug level increased",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    drugs <-  dplyr::summarise(group_by(data, DRUGNAME),count=n_distinct(REPORT_ID))
    drugs_sorted<- drugs %>% arrange(desc(count)) %>% top_n(n=25)
    
    library(scales)
    p <- ggplot(drugs_sorted, aes(x = DRUGNAME, y = n, fill = DRUGNAME)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(drugs_sorted$DRUGNAME[1:25])) + 
      coord_flip() +
      ggtitle("Top 25 Drugs (in addition to search term)") +
      xlab("Drug (generic name)") + 
      ylab("Number of Reports (thousands)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(limits=  c(0, NA))
    p
  })
  
  output$indicationplot <- renderPlot({
    data <- cv_drug_tab_indc()
    # test
    # data <-drugs_tab(current_generic="phenytoin",current_brand="DILANTIN",current_rxn=NA,date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    #indications <- ddply(data,"INDICATION_NAME_ENG", count_func) 
    #indications_sorted <- indications[order(desc(indications$n)),]
    #indications_sorted <- indications_sorted %>% top_n(n=25) 
    
    indications <-  dplyr::summarise(group_by(data, INDICATION_NAME_ENG),count=n_distinct(REPORT_ID))
    indications_sorted<- indications %>% arrange(desc(count)) %>% top_n(n=25)
    
    library(scales)
    p <- ggplot(indications_sorted, aes(x = INDICATION_NAME_ENG, y = n, fill = INDICATION_NAME_ENG)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(indications_sorted$INDICATION_NAME_ENG)) + 
      coord_flip() +
      ggtitle("Top 25 Indications (All Drugs)") +
      xlab("Indication") + 
      ylab("Number") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(limits=  c(0, NA))
    p
  })
  
  # sample datasets of what is being graphed/used
  output$outputReports <- renderTable(
    cv_drug_tab_indc()
  )
  
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
}

shinyApp(ui, server)
