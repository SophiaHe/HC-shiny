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


############ UI for REPORT Tab shiny ################
ui <- dashboardPage(
  dashboardHeader(title = "Shiny FAERS (v0.04)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o"))
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
    dateRangeInput("searchDateRange", 
                   "Date Range", 
                   start = "1973-01-01", 
                   end = Sys.Date(),
                   startview = "year",
                   format = "yyyy-mm-dd"),
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
      )
    )
  )
)


############## Server of shiny ########################
# connect to CV database
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")

#options(shiny.error = browser)
options(shiny.trace = TRUE, shiny.reactlog=TRUE)

# Temperary solution: fetch all tables to local and run temp_reports_tab_func on them
cv_reports <- tbl(hcopen, "cv_reports") 
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients")
cv_report_drug <- tbl(hcopen,"cv_report_drug")
cv_reactions <- tbl(hcopen,"cv_reactions") 

############### Server Functions ###################
server <- function(input, output) {
  # Data frame generate reactive function to be used to assign: data <- cv_reports_tab() 
  setwd("~/CV_Shiny_Tab")
  source("temp_reports_tab_func SHe.R")
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
                                    "TYLENOL",
                                    input$search_brand)) 
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn)) 
    current_date_range <- isolate(input$searchDateRange)
    #date_ini <- ifelse(input$current_date_range[1]== "","19730101", input$current_date_range[1])
    #date_end <- ifelse(input$current_date_range[2]== "","20150101", input$current_date_range[2])

  
    reports_tab_df <- reports_tab(current_generic=current_generic,current_brand=current_brand,current_rxn=current_rxn,date_ini=date_ini,date_end=date_end)
    
    return(reports_tab_df)
  })
  
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
    current_date_range <- isolate(input$searchDateRange)
    date_ini <- ifelse(input$current_date_range[1]== "","19730101", input$current_date_range[1])
    date_end <- ifelse(input$current_date_range[2]== "","20150101", input$current_date_range[2])
    
    #date_ini <- ymd(date_ini)
    #date_end <- ymd(date_end)
    
    search_tab_df <- data.frame(names = c("Generic Name:", 
                                          "Brand Name:", 
                                          "Adverse Reaction Term:",
                                          "Date Range:"),
                                terms = c(current_generic,current_brand,current_rxn,paste(current_date_range, collapse=" to ")),
                                stringsAsFactors=FALSE)
    search_tab_df$terms[is.na(search_tab_df$terms) == TRUE] <- "Not Specified"
    return(search_tab_df)
  })
  
############## Construct Current_Query_Table for generic name, brand name, adverse reaction term & date range searched ############
  output$current_search <- renderTable({
    data_search <- cv_search_tab()
  }, include.rownames = FALSE, include.colnames = FALSE)
  
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

}

shinyApp(ui, server)
