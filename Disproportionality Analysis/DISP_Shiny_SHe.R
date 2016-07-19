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
cv_prr <- tbl(hcopen, "cv_prr_160713") 
cv_bcpnn <- tbl(hcopen, "cv_bcpnn")
data <- bayes_PRR_result_all
data[data$drug.code == "BENZOCAINE",]

head(bayes_result_all)
quarter1 <- bayes_result_all[bayes_result_all$quarter == 1965.1,]
quarter2 <- bayes_result_all[bayes_result_all$quarter == 1965.2,]

common <- merge(quarter1, quarter2, by= c("drug.code", "event.effect"))
# CHLORPROMAZINE Dysphagia
#quarter1[quarter1$drug.code == "CHLORPROMAZINE" & quarter1$event.effect == "Dysphagia",]
#quarter2[quarter2$drug.code == "CHLORPROMAZINE" & quarter2$event.effect == "Dysphagia",]

#Oxycodone (ing= OXYCODONE HYDROCHLORIDE) -drug dependence
# pioglitazone- bladder cance
#captopril (check spelling) and coughing

data[data$drug.code== "OXYCODONE HYDROCHLORIDE" & data$event.effect == "Drug dependence",]
data[data$drug.code== "Oxycodone",]
cv_report_drug[cv_report_drug$DRUGNAME == "OXYCODONE",]
cv_drug_product_ingredients <-  cv_drug_product_ingredients %>% as.data.frame()
cv_drug_product_ingredients[cv_drug_product_ingredients$DRUGNAME == "OXYCODONE",]
oxy <- cv_drug_rxn[cv_drug_rxn$DRUGNAME == "OXYCODONE",]
oxy$ing <- as.factor(oxy$ing)
levels(oxy$ing)
########################### Functions ############################
topdrug_func <- function(n){
  cv_report_drug_dm <- cv_report_drug %>% dplyr::select(REPORT_ID, DRUGNAME)
  
  topdrugs <- dplyr::summarise(group_by(cv_report_drug_dm, DRUGNAME),count=n_distinct(REPORT_ID)) %>% as.data.table(n=-1)
  topdrugs_final <- topdrugs %>% arrange(desc(count)) %>% top_n(n) %>% dplyr::select(DRUGNAME)
  topdrugs_final <- cv_prr %>%
  return(topdrugs_final)
} 

toprxn_func <- function(n){
  cv_reactions_dm <- cv_reactions %>% dplyr::select(REPORT_ID, PT_NAME_ENG)
  
  toprxn <- dplyr::summarise(group_by(cv_reactions_dm, PT_NAME_ENG),count=n_distinct(REPORT_ID)) %>% as.data.table(n=-1)
  toprxn_final <- toprxn %>% dplyr::arrange(desc(count)) %>% top_n(n) %>% dplyr::select(PT_NAME_ENG)
  
  return(toprxn_final)
}

topdrugs <- cv_prr %>% distinct(drug.code) %>% as.data.frame()
toprxns <- cv_prr %>% distinct(event.effect) %>% as.data.frame()




############ UI for DISP shiny ################
ui <- dashboardPage(
  dashboardHeader(title = "CV Shiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PRR", tabName = "prrdata", icon = icon("fa fa-cogs")),
      menuItem("BCPNN", tabName = "bcpnndata", icon = icon("fa fa-binoculars")),
      menuItem("ROR", tabName = "rordata", icon = icon("fa fa-cogs")),
      menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
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
                   toprxns$event.effect,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    dateRangeInput("searchDateRange",
                   "Date Range",
                   start = "1965-01-01",
                   end = Sys.Date(),
                   startview = "year",
                   format = "yyyy-mm-dd"),
    ##add more menu filter here
    
    actionButton("searchButton", "Search"),
    tags$br(),
    tags$h3(strong("Current Search:")),
    tableOutput("current_search")
  ), 
  
  dashboardBody(
    # fluidRow(
    #   box(plotlyOutput(outputId = "timeplot"),
    #       tags$br(),
    #       tags$p("Reports by month from Canada Vigilance Adverse Reaction Online Database. 
    #              Trendline is a local non-parametric regression calculated with the LOESS model. 
    #              The shaded area is an approximation of the 95% confidence interval of the regression."),
    #       width = 12
    #       )
    #   ),
    tabItems(
      tabItem(tabName = "prrdata",
              fluidRow(
                box(plotlyOutput(outputId = "prr_timeplot"),
                    tags$br(),
                    tags$p("Reports by month from Canada Vigilance Adverse Reaction Online Database. 
                 Trendline is a local non-parametric regression calculated with the LOESS model. 
                 The shaded area is an approximation of the 95% confidence interval of the regression."),
                    width = 12
                ),
                box(htmlOutput("sexplot"),
                    tags$br(),
                    tags$p("Unknown includes reports explicitly marked unknown and Not Specified includes reports with no gender information."),
                    title = tags$h2("Gender"), width = 4),
                box(htmlOutput("agegroupplot"),
                    tags$br(),
                    tags$p("Unknown includes reports with no age information."), 
                    title = tags$h2("Age Groups"), width = 4),
                box(plotlyOutput("agehist"), title = tags$h2("Age Histogram"), width = 4)
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
                ),
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
      
      tabItem(tabName = "downloaddata",
              fluidRow(
                box(
                  title = tags$h2("Download Dataset for Disproportionality Analysis"),
                  dateRangeInput("searchDateRange_DISP",
                                 "Date Range",
                                 start = "1965-01-01",
                                 end = Sys.Date(),
                                 startview = "year",
                                 format = "yyyy-mm-dd"),
                  actionButton("searchDISPButton", "Search"),
                  tableOutput("current_DISP_search"),
                  tableOutput("current_DISP_size"),
                  downloadButton('downloadData_DISP', 'Download')
                ),
                
                box(
                  tags$h2("Download Data Used for Current Searched Combination"),
                  tags$h3("Please select an information category: "),
                  selectizeInput("search_dataset_type",
                                 "Information Category",
                                 choices = c("Report Info", "Drug Info", "Reaction Info"),  
                                 options = list(create = TRUE,
                                                placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }'))),
                  actionButton("search_report_type_dl","Go"),
                  tableOutput("download_reports_type"),
                  tableOutput("download_reports_size"),
                  downloadButton('download_reports', 'Download')
                )
                # box(plotOutput("drugplot"),
                #     tags$br(),
                #     tags$p("This plot includes top_10 most-reported drugs with most-reported indication assocaiated with the seached drug."), width = 4)
              )
      ),
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





############################# Server of DISP Shiny #####################
server <- function(input, output) {
  observe({
    drug_option <- input$search_generic
    event_option <- cv_prr$event.effect[cv_prr$drug.code == drug_option]
    
    updateSelectInput(session, "drug_option",
                      label = "Select Adverse Event:",
                      value = event_option)
  })
  
  
  output$prr_timeplot <- renderPlotly({
    
    df <- data[data$drug.code== "OXYCODONE HYDROCHLORIDE" & data$event.effect == "DRUG DEPENDENCE",]
    
    #drug_selected <- data$DRUGNAME[1] 
    
    # data1 <- cv_search_tab()
    # drug_selected <- data1$terms[1]
    # #drug_selected <- "abc"
    # 
    # # specify the title of time plot based on reactive choice
    # title <- ifelse(drug_selected == "Not Specified (All)", "All Drugs",drug_selected)
    # plottitle <- paste("Strengh of Drug*AR pairs for", title)
    p <- df %>%
      ggplot(aes(x = `.id`, y = PRR,group = 1)) +
      geom_line() + geom_point()  + 
      ggtitle("plottitle1") + 
      xlab("Quarter") + 
      ylab("Strength of Drug*AR Pairs") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
    ggplotly(p)
  })
  
}
shinyApp(ui, server)
