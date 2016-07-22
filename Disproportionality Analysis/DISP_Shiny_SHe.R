library(shinydashboard)
library(jsonlite)
library(lubridate)
library(plyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(plotly)
library(shiny)
library(DT)
library(googleVis)
# library(openfda)
library(stringr)
library(PhViD)
library(utils)
library(dplyr)

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_prr <- tbl(hcopen, "cv_prr_160713") %>% as.data.frame()
cv_bcpnn <- tbl(hcopen, "cv_bcpnn_160712")%>% as.data.frame()
cv_ror <- tbl(hcopen, "cv_ror_160714") %>% as.data.frame()

#a <- cv_prr[order(cv_prr$PRR, decreasing = TRUE),]52199.00
# THIAMAZOLE +CARDIAC FAILURE CONGESTIVE

cv_prr[mapply(is.infinite, cv_prr)] <- 99999.99

cv_prr <- cv_prr[order(cv_prr$.id,cv_prr$PRR, decreasing = TRUE),]
cv_prr <- cv_prr[order(cv_prr$.id,decreasing = FALSE),]

# cv_ror <- cv_ror[order(cv_ror$.id, cv_ror$ROR, decreasing=TRUE),] 48613.000
# cv_ror <- cv_ror[order(cv_ror$.id, decreasing = FALSE),]
cv_ror[mapply(is.infinite, cv_ror)] <- 99999.99



#Oxycodone (ing= OXYCODONE HYDROCHLORIDE) -drug dependence
# pioglitazone- bladder cance
#captopril (check spelling) and coughing
# PENICILLIN V + RASH
# MEPERIDINE HYDROCHLORIDE + NAUSEA
# IRON + BACK PAIN in BCPNN
# NICOTINE + DIZZINESS in BCPNN
# NICOTINE + CHEST PAIN in BCPNN
# AMPICILLIN +RASH !!!!

########################### Functions ############################


#topdrugs <- cv_prr %>% distinct(drug.code) %>% as.data.frame()
#toprxns <- cv_prr %>% distinct(event.effect) %>% as.data.frame()

topdrugs <- cv_prr %>% dplyr::distinct(drug.code)%>% dplyr::top_n(2000) %>% as.data.frame()
toprxns <- cv_prr %>% semi_join(topdrugs) %>% dplyr::distinct(event.effect)%>% as.data.frame()
quarters <- levels(as.factor(cv_prr$`.id`))

#%>% dplyr::top_n(2000)
############ UI for DISP shiny ################
ui <- dashboardPage(
  dashboardHeader(title = "CV Shiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PRR", tabName = "prrdata", icon = icon("fa fa-cogs")),
      menuItem("BCPNN", tabName = "bcpnndata", icon = icon("fa fa-binoculars")),
      menuItem("ROR", tabName = "rordata", icon = icon("fa fa-database")),
      # menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    selectizeInput("search_generic", 
                   "Generic Name/Ingredient",
                   topdrugs$drug.code,
                   #multiple = TRUE,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    
    selectizeInput("search_rxn", 
                   "Adverse Event Term",
                   choices = NULL,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    selectizeInput("start_quarter", 
                   "Display Results Starting From (Quarterly): ",
                   quarters,
                   #multiple = TRUE,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    
    selectizeInput("end_quarter", 
                   "Display Results Ending at (Quarterly): ",
                   quarters,
                   #multiple = TRUE,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    # dateRangeInput("searchDateRange",
    #                "Date Range",
    #                start = "1965-01-01",
    #                end = Sys.Date(),
    #                startview = "year",
    #                format = "yyyy-mm-dd"),
    ##add more menu filter here
    
    actionButton("searchButton", "Search")
    # tags$br(),
    # tags$h3(strong("Current Search:")),
    # tableOutput("current_search")
  ), 
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "prrdata",
              fluidRow(
                tabBox(
                  tabPanel(
                  "Time Plot",
                  plotlyOutput(outputId = "prr_timeplot"),
                  tags$br(),
                  tags$p("Proportional Reporting Ratio (PRR) results by quarter. 
                          Value of PRR indicates the signal strength of the particular Drug & Adverse_reaction pair. 
                          Details can be found in DISP_about documentation"),
                  width = 12), 
                  
                  tabPanel(
                    "Word Cloud",
                    
                  )
                width=12
                )
                # box(htmlOutput("sexplot"),
                #     tags$br(),
                #     tags$p("Unknown includes reports explicitly marked unknown and Not Specified includes reports with no gender information."),
                #     title = tags$h2("Gender"), width = 4),
                # box(htmlOutput("agegroupplot"),
                #     tags$br(),
                #     tags$p("Unknown includes reports with no age information."), 
                #     title = tags$h2("Age Groups"), width = 4),
                # box(plotlyOutput("agehist"), title = tags$h2("Age Histogram"), width = 4)
              )
      ),
      
      tabItem(tabName = "bcpnndata",
              fluidRow(
                box(plotlyOutput(outputId = "bcpnn_timeplot"),
                    tags$br(),
                    tags$p("Reports by month from Canada Vigilance Adverse Reaction Online Database.
                 Trendline is a local non-parametric regression calculated with the LOESS model.
                 The shaded area is an approximation of the 95% confidence interval of the regression."),
                    width = 12
                )
      #           # box(htmlOutput("reporterplot"), 
      #           #     tags$br(),
      #           #     tags$p("Qualification of the person who filed the report."),
      #           #     tags$p("Unknown is the number of reports without the primarysource.qualification field."),
      #           #     title = tags$h2("Reporter"), width = 4),
      #           # box(htmlOutput("seriousplot"), 
      #           #     tags$br(),
      #           #     tags$p("Reports marked as serious."),
      #           #     title = tags$h2("Serious reports"), width = 4),
      #           # box(htmlOutput("seriousreasonsplot"), 
      #           #     tags$br(),
      #           #     tags$p("Total sums to more than 100% because reports can be marked serious for multiple reasons."),
      #           #     title = tags$h2("Reasons for serious reports"), width = 4)
              )
      ),

      tabItem(tabName = "rordata",
              fluidRow(
                box(plotlyOutput(outputId = "ror_timeplot"),
                    tags$br(),
                    tags$p("Reports by month from Canada Vigilance Adverse Reaction Online Database.
                 Trendline is a local non-parametric regression calculated with the LOESS model.
                 The shaded area is an approximation of the 95% confidence interval of the regression."),
                    width = 12
                )
                #           # box(htmlOutput("reporterplot"), 
                #           #     tags$br(),
                #           #     tags$p("Qualification of the person who filed the report."),
                #           #     tags$p("Unknown is the number of reports without the primarysource.qualification field."),
                #           #     title = tags$h2("Reporter"), width = 4),
                #           # box(htmlOutput("seriousplot"), 
                #           #     tags$br(),
                #           #     tags$p("Reports marked as serious."),
                #           #     title = tags$h2("Serious reports"), width = 4),
                #           # box(htmlOutput("seriousreasonsplot"), 
                #           #     tags$br(),
                #           #     tags$p("Total sums to more than 100% because reports can be marked serious for multiple reasons."),
                #           #     title = tags$h2("Reasons for serious reports"), width = 4)
              )
      ),
      
      # tabItem(tabName = "downloaddata",
      #         fluidRow(
      #           box(
      #             title = tags$h2("Download Dataset for Disproportionality Analysis"),
      #             dateRangeInput("searchDateRange_DISP",
      #                            "Date Range",
      #                            start = "1965-01-01",
      #                            end = Sys.Date(),
      #                            startview = "year",
      #                            format = "yyyy-mm-dd"),
      #             actionButton("searchDISPButton", "Search"),
      #             tableOutput("current_DISP_search"),
      #             tableOutput("current_DISP_size"),
      #             downloadButton('downloadData_DISP', 'Download')
      #           ),
      #           
      #           box(
      #             tags$h2("Download Data Used for Current Searched Combination"),
      #             tags$h3("Please select an information category: "),
      #             selectizeInput("search_dataset_type",
      #                            "Information Category",
      #                            choices = c("Report Info", "Drug Info", "Reaction Info"),  
      #                            options = list(create = TRUE,
      #                                           placeholder = 'Please select an option below',
      #                                           onInitialize = I('function() { this.setValue(""); }'))),
      #             actionButton("search_report_type_dl","Go"),
      #             tableOutput("download_reports_type"),
      #             tableOutput("download_reports_size"),
      #             downloadButton('download_reports', 'Download')
      #           )
      #           # box(plotOutput("drugplot"),
      #           #     tags$br(),
      #           #     tags$p("This plot includes top_10 most-reported drugs with most-reported indication assocaiated with the seached drug."), width = 4)
      #         )
      # ),
      tabItem(tabName = "aboutinfo",
              tags$h2("About the Shiny App"),
              tags$p("This is a prototyping platform to utilize open data sources (e.g. Canada Vigilance Adverse Reaction Online Database) 
                      and provide visualizations in an interactive format. Further analysis can be conducted and added onto this platform to make 
                      better use of the data. Data provided by the Canada Vigilance Adverse Reaction Online Database: "),
              tags$a(href="http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php", "Click here!"),
              tags$br(),
              tags$strong("Authors:"),
              fluidRow(
                box(
                  tags$p("Daniel Buijs, MSc"),
                  tags$p("Data Scientist, Health Products and Food Branch"),
                  tags$p("Health Canada / Government of Canada"),
                  tags$p("daniel.buijs@hc-sc.gc.ca")
                ),
                box(
                  tags$p("Sophia He, BSc in Progress"),
                  tags$p("Jr. Data Scientist Co-op, Health Products and Food Branch"),
                  tags$p("Health Canada / Government of Canada"),
                  tags$p("sophia.he@canada.ca")
                ),
                box(
                  tags$h3("Disproportionality Analysis - Bayesian confidence propagation neural network (BCPNN) Method"),
                  tags$p("..."),
                  tags$br(),
                  tags$h3("Disproportionality Analysis - Proportional Reporting Ratio (PRR) Method"),
                  tags$p("...")
                )
              )
      )
    )
    ), 
  
  skin = "blue"
  )



options(shiny.trace = TRUE, shiny.reactlog=TRUE)
#drug_option = "SILVER NITRATE"

############################# Server of DISP Shiny #####################
server <- function(input, output, session) {

  # Design Reaction Dropdown Menu based on Drug selection
  observe({
    if(is.na(input$search_generic) == TRUE){
      drug_option <- "OXYCODONE HYDROCHLORIDE"
    } else {
      drug_option <- input$search_generic 
    }
    
    # if(is.na(input$search_rxn) == TRUE){
    #   current_rxn <- "DRUG DEPENDENCE"
    # } else {
    #   current_rxn <- input$search_rxn
    # }
        
      # adverse events available 
      event_option <- cv_prr %>% dplyr::filter(drug.code == drug_option) %>% dplyr::distinct(event.effect) %>% dplyr::select(event.effect) #%>% arrange(event.effect)
      
      updateSelectInput(session, "search_rxn",
                        label = "Select Adverse Event:",
                        choices = event_option)
      
      # quarters avaiable
      quarter_option <- cv_prr %>% dplyr::filter(drug.code == drug_option) %>% dplyr::distinct(.id) %>% dplyr::select(.id)
      
      updateSelectInput(session, "start_quarter",
                        label = "Display Results Starting From (Quarterly): ",
                        choices = quarter_option)
      
      updateSelectInput(session, "end_quarter",
                        label = "Display Results Ending At (Quarterly): ",
                        choices = quarter_option)
      
  })
  
  # PRR tab 
  cv_prr_tab <- reactive({
    input$searchButton
    #codes about dplyr::select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_drug <- isolate(ifelse(input$search_generic == "",
                                    "OXYCODONE HYDROCHLORIDE",
                                    input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "DRUG DEPENDENCE",
                                  input$search_rxn))
    start_quarter <- isolate(ifelse(input$start_quarter == "",
                                    "1965.1",
                                    input$start_quarter))
    end_quarter <- isolate(ifelse(input$end_quarter == "",
                                  "2015.1",
                                  input$end_quarter))
    
    
    #prr_tab_df <- cv_prr[cv_prr$drug.code== current_drug & cv_prr$event.effect == current_rxn,]
    
    prr_tab_df <- cv_prr %>% filter(drug.code == current_drug, event.effect == current_rxn, as.numeric(.id) >= as.numeric(start_quarter), as.numeric(.id) <= as.numeric(end_quarter))
      
    return(prr_tab_df)
  })
  
  
  # PRR Time Plot
  output$prr_timeplot <- renderPlotly({
    
    df <- cv_prr_tab()
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "OXYCODONE HYDROCHLORIDE",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "DRUG DEPENDENCE",
                                  input$search_rxn))
    plottitle <- paste("Non-Cumulative PRR Time Plot for:", current_drug, "&", current_rxn)

    p <- df %>%
      ggplot(aes(x = `.id`, y = PRR,group = 1)) +
      geom_line() + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("Non Cumulative PRR") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
    ggplotly(p)
  })
  

  a <- cv_prr %>% filter(drug.code == "AMPICILLIN")
  # PRR Event Wordcloud based on PRR
  output$prr_event_wordcloud <- renderPlotly({
    df <- cv_prr_tab()
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "OXYCODONE HYDROCHLORIDE",
                                   input$search_generic))
    start_quarter <- isolate(ifelse(input$start_quarter == "",
                                    "1965.1",
                                    input$start_quarter))
    end_quarter <- isolate(ifelse(input$end_quarter == "",
                                  "2015.1",
                                  input$end_quarter))
    plottitle <- paste("PRR Events that Contain", current_drug, "Between", start_quarter, "and", end_quarter)

    data <- cv_prr %>% filter(drug.code =="AMPICILLIN")
    p <- wordcloud::wordcloud(data$event.effect, data$PRR)

  })
  
  # BCPNN Tab
  cv_bcpnn_tab <- reactive({
    input$searchButton
    #codes about dplyr::select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "OXYCODONE HYDROCHLORIDE",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "DRUG DEPENDENCE",
                                  input$search_rxn))
    
    start_quarter <- isolate(ifelse(input$start_quarter == "",
                                    "1965.1",
                                    input$start_quarter))
    end_quarter <- isolate(ifelse(input$end_quarter == "",
                                  "2015.1",
                                  input$end_quarter))
    
    bcpnn_tab_df <- cv_bcpnn %>% filter(drug.code == current_drug, event.effect == current_rxn, as.numeric(.id) >= as.numeric(start_quarter), as.numeric(.id) <= as.numeric(end_quarter))
    
    return(bcpnn_tab_df)
  })
  
  # BCPNN Time Plot
  output$bcpnn_timeplot <- renderPlotly({
    df <- cv_bcpnn_tab()
    
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "OXYCODONE HYDROCHLORIDE",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "DRUG DEPENDENCE",
                                  input$search_rxn))
    plottitle <- paste("Non-Cumulative Information Component (IC) Time Plot for:", current_drug, "&", current_rxn)
    
    p <- df %>%
      ggplot(aes(x = `.id`, y = `Q_0.025.log.IC..`,group = 1)) +
      geom_line() + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("2.5% Quantile of Posterior Distribution of IC") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
    ggplotly(p)
  })
  
  # ROR Tab
  cv_ror_tab <- reactive({
    input$searchButton
    #codes about dplyr::select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "OXYCODONE HYDROCHLORIDE",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "DRUG DEPENDENCE",
                                  input$search_rxn))
    
    start_quarter <- isolate(ifelse(input$start_quarter == "",
                                    "1965.1",
                                    input$start_quarter))
    end_quarter <- isolate(ifelse(input$end_quarter == "",
                                  "2015.1",
                                  input$end_quarter))
    
    ror_tab_df <- cv_ror %>% filter(drug.code == current_drug, event.effect == current_rxn, as.numeric(.id) >= as.numeric(start_quarter), as.numeric(.id) <= as.numeric(end_quarter))
    
    return(ror_tab_df)
  })
  
  # ROR Time Plot
  output$ror_timeplot <- renderPlotly({
    df <- cv_ror_tab()
    
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "OXYCODONE HYDROCHLORIDE",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "DRUG DEPENDENCE",
                                  input$search_rxn))
    plottitle <- paste("Non-Cumulative Reporting Odds Ratio Time Plot for:", current_drug, "&", current_rxn)
    
    p <- df %>%
      ggplot(aes(x = `.id`, y = ROR,group = 1)) +
      geom_line() + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("ROR") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
    ggplotly(p)
  })
  
  
}
shinyApp(ui, server)
