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
library(utils)
library(dplyr)
library(tm)
library(wordcloud)

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_prr <- tbl(hcopen, "cv_prr_160713") %>% as.data.frame()
cv_bcpnn <- tbl(hcopen, "cv_bcpnn_160805")%>% as.data.frame()
cv_ror <- tbl(hcopen, "cv_ror_160714") %>% as.data.frame()



# Change Inf value to 0 because there's no ZERO value in all datasets
cv_prr[mapply(is.infinite, cv_prr)] <- 0

cv_prr <- cv_prr[order(cv_prr$.id,cv_prr$PRR, decreasing = TRUE),]
cv_prr <- cv_prr[order(cv_prr$.id,decreasing = FALSE),]

# cv_ror <- cv_ror[order(cv_ror$.id, cv_ror$ROR, decreasing=TRUE),] 48613.000
# cv_ror <- cv_ror[order(cv_ror$.id, decreasing = FALSE),]
cv_ror[mapply(is.infinite, cv_ror)] <- 0


# cv_ror[cv_ror$ROR == 0,]
# cv_prr[cv_prr$PRR == 0,]
# cv_bcpnn[cv_bcpnn$Q_0.025.log.IC.. == 0,]

# signals with IC >= 0.05
cv_bcpnn <- cv_bcpnn[order(cv_bcpnn$.id,cv_bcpnn$Q_0.025.log.IC.., decreasing = TRUE),]
cv_bcpnn <- cv_bcpnn[order(cv_bcpnn$.id,decreasing = FALSE),]
bcpnn_signals <- cv_bcpnn %>% filter( `Q_0.025.log.IC..` >= 0.05)
prr_signals <- cv_prr %>% filter(`LB95.log.PRR..` >=1, count >1)

#Oxycodone (ing= OXYCODONE HYDROCHLORIDE) -drug dependence & + DRUG WITHDRAWL SYDROME
# PIOGLITAZONE + BLADDER CANCER
#CAPTOPRIL + COUGH
# PENICILLIN V + RASH
# MEPERIDINE HYDROCHLORIDE + NAUSEA
# IRON + BACK PAIN in BCPNN
# NICOTINE + DIZZINESS in BCPNN
# NICOTINE + CHEST PAIN in BCPNN
# AMPICILLIN +RASH !!!!
# THIAMAZOLE +CARDIAC FAILURE CONGESTIVE

########################### Functions ############################


#topdrugs <- cv_prr %>% distinct(drug.code) %>% as.data.frame()
#toprxns <- cv_prr %>% distinct(event.effect) %>% as.data.frame()

topdrugs <- cv_prr %>% dplyr::distinct(drug.code) %>% as.data.frame()
# toprxns <- cv_prr %>% semi_join(topdrugs) %>% dplyr::distinct(event.effect)%>% as.data.frame()
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
    
    actionButton("searchButton", "Search")
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
                  dataTableOutput("Rxn_table_prr"),
                  width = 12), 
                  
                  tabPanel(
                    "Word Cloud Based on PRR",
                    plotOutput("prr_event_PRRwordcloud"),
                    tags$br(),
                    tags$p("This Word Cloud demonstrates Top20 Adverse Events Related to Selected Drug Based on PRR value. 
                           The size of the words are proportional to the value of PRR.")
                  ),
                  tabPanel(
                    "Word Cloud Based on Frequency",
                    plotOutput("prr_event_FREQwordcloud"),
                    tags$br(),
                    tags$p("This Word Cloud demonstrates Top20 Adverse Events Related to Selected Drug Based on Event Frequency During Selected Time Range. 
                           The size of the words are proportional to the number of occurence of that adverse event.")
                  ),
                  
                width=12
                )
              )
      ),
      
      tabItem(tabName = "bcpnndata",
              fluidRow(
                tabBox(
                  tabPanel(
                    "Time Plot",
                    plotlyOutput(outputId = "bcpnn_timeplot"),
                    tags$br(),
                    tags$p("Information Component (IC) results by quarter. 
                          Value of IC indicates the signal strength of the particular Drug & Adverse_reaction pair. 
                          Details can be found in DISP_about documentation"),
                    plotlyOutput(outputId = "bcpnn_timeplot_cumulative"),
                    dataTableOutput("Rxn_table_bcpnn"),
                    width = 12), 
                  
                  tabPanel(
                    "Word Cloud Based on IC",
                    plotOutput("bcpnn_event_ICwordcloud"),
                    tags$br(),
                    tags$p("This Word Cloud demonstrates Top20 Adverse Events Related to Selected Drug Based on IC value. 
                           The size of the words are proportional to the value of IC.")
                  ),
                  tabPanel(
                    "Word Cloud Based on Frequency",
                    plotOutput("bcpnn_event_FREQwordcloud"),
                    tags$br(),
                    tags$p("This Word Cloud demonstrates Top20 Adverse Events Related to Selected Drug Based on Event Frequency During Selected Time Range. 
                           The size of the words are proportional to the number of occurence of that adverse event.")
                  ),
                  width=12
                )
              )

   
      ),

      tabItem(tabName = "rordata",
              fluidRow(
                tabBox(
                  tabPanel(
                    "Time Plot",
                    plotlyOutput(outputId = "ror_timeplot"),
                    tags$br(),
                    tags$p("Reporting Odds Ratio (ROR) results by quarter. 
                          Value of ROR indicates the signal strength of the particular Drug & Adverse_reaction pair. 
                          Details can be found in DISP_about documentation"),
                    dataTableOutput("Rxn_table_ror"),
                    width = 12), 
                  
                  tabPanel(
                    "Word Cloud Based on ROR",
                    plotOutput("ror_event_RORwordcloud"),
                    tags$br(),
                    tags$p("This Word Cloud demonstrates Top20 Adverse Events Related to Selected Drug Based on ROR value. 
                           The size of the words are proportional to the value of ROR.")
                  ),
                  tabPanel(
                    "Word Cloud Based on Frequency",
                    plotOutput("ror_event_FREQwordcloud"),
                    tags$br(),
                    tags$p("This Word Cloud demonstrates Top20 Adverse Events Related to Selected Drug Based on Event Frequency During Selected Time Range. 
                           The size of the words are proportional to the number of occurence of that adverse event.")
                  ),
                  width=12
                )

              )
      ),
      
      tabItem(tabName = "aboutinfo",
              tags$h2("About the Shiny App"),
              tags$p("This is a prototyping platform to utilize open data sources (e.g. Canada Vigilance Adverse Reaction Online Database) 
                      to conduct disproportionality analysis for safety signal detection. 
                      It provides visualizations in an interactive format to demonstrate the results of multiple disproportionality analysis. 
                      Data provided by the Canada Vigilance Adverse Reaction Online Database: "),
              tags$a(href="http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php", "Click here!"),
              tags$p("Detailed documentation on all disproportionality analyses can be found in here: "),
              #tags$a(href = "https://rstudio.hres.ca/?view=rmarkdown", "Documentation of Analysis"),
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
                  tags$p("sophia.he@canada.ca & yunqingh@sfu.ca")
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
  menu_options <- reactive({if(is.na(input$search_generic) == TRUE){
    drug_option <- "OXYCODONE HYDROCHLORIDE"
  } else {
    drug_option <- input$search_generic 
  }
    
    
    # adverse events available 
    event_option <- cv_prr %>% dplyr::filter(drug.code == drug_option) %>% dplyr::distinct(event.effect) %>% dplyr::select(event.effect) #%>% arrange(event.effect)
    
    # quarters avaiable
    quarter_option <- cv_prr %>% dplyr::filter(drug.code == drug_option) %>% dplyr::distinct(.id) %>% dplyr::select(.id)
    
    return(
      list(event_option <- event_option,
           quarter_option <- quarter_option)
    )
  })
  
  # Design Reaction Dropdown Menu based on Drug selection
  observe({
    updateSelectInput(session, "search_rxn",
                      label = "Select Adverse Event:",
                      choices = as.data.frame(menu_options()[1])$event.effect)
    
    
    
    updateSelectInput(session, "start_quarter",
                      label = "Display Results Starting From (Quarterly): ",
                      choices = as.data.frame(menu_options()[2])$.id)
    
    updateSelectInput(session, "end_quarter",
                      label = "Display Results Ending At (Quarterly): ",
                      choices = as.data.frame(menu_options()[2])$.id)
    
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
    prr_tab_wc <- cv_prr %>% filter(drug.code == current_drug, as.numeric(.id) >= as.numeric(start_quarter), as.numeric(.id) <= as.numeric(end_quarter))  
    #prr_tab_rxn <- cv_prr %>% 
    
    return(list(prr_tab_df <- prr_tab_df,
                prr_tab_wc <- prr_tab_wc))
  })
  
  
  # PRR Time Plot
  output$prr_timeplot <- renderPlotly({
    df <- as.data.frame(cv_prr_tab()[1])
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
  

  
  # PRR Tab: Event Wordcloud based on PRR
  output$prr_event_PRRwordcloud <- renderPlot({
    df <- as.data.frame(cv_prr_tab()[2])

    data <- df %>% filter(PRR < 99999.99) %>% arrange(desc(PRR)) %>% dplyr::select(event.effect, PRR) %>% slice(1:20)

    wordcloud::wordcloud(words = data$event.effect, freq = data$PRR, scale = c(1.5, 0.2), colors=brewer.pal(8, "Dark2"))

  })
  # scale = c(1, 0.2)
  
  # PRR Tab: Event Wordcloud based on frequency of event.effect
  output$prr_event_FREQwordcloud <- renderPlot({
    df <- as.data.frame(cv_prr_tab()[2])
    
    data2 <- df %>% group_by(event.effect)%>% dplyr::summarise(count = n()) %>% arrange(desc(count))%>% slice(1:20)

    wordcloud::wordcloud(words = data2$event.effect, freq = data2$count, scale = c(1.5, 0.2), colors=brewer.pal(8, "Dark2"))
  })
  
  # PRR Tab: Reactions based on PRR associated with selected drug
  output$Rxn_table_prr <- renderDataTable({
    df <- as.data.frame(cv_prr_tab()[2]) 
    data <- df %>% filter(PRR < 99999.99) %>% arrange(desc(PRR)) %>% select(-c(row.names,FDR))
    data
  })

  ###################### BCPNN Tab #######################
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
    bcpnn_tab_wc <- cv_bcpnn %>% filter(drug.code == current_drug, as.numeric(.id) >= as.numeric(start_quarter), as.numeric(.id) <= as.numeric(end_quarter))
    
    return(list(bcpnn_tab_df <- bcpnn_tab_df,
                bcpnn_tab_wc <-bcpnn_tab_wc))
  })
  
  # BCPNN Time Plot
  output$bcpnn_timeplot <- renderPlotly({
    df <- as.data.frame(cv_bcpnn_tab()[1])
    
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "OXYCODONE HYDROCHLORIDE",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "DRUG DEPENDENCE",
                                  input$search_rxn))
    plottitle <- paste("Non-Cumulative Information Component (IC) Time Plot for:", current_drug, "&", current_rxn)
    
    p <- df %>%
      ggplot(aes(x = `.id`, y = `IC`,group = 1)) +
      geom_line() + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("Information Component (IC)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
    ggplotly(p)
  })
  
  # cumulative time plot
  output$bcpnn_timeplot_cumulative <- renderPlotly({
    df <- as.data.frame(cv_bcpnn_tab()[1])
    df1 <- df %>% group_by(drug.code,event.effect) %>% mutate(IC_Cumulative = cumsum(IC)) 
    
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "OXYCODONE HYDROCHLORIDE",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "DRUG DEPENDENCE",
                                  input$search_rxn))
    plottitle <- paste("Cumulative Information Component (IC) Time Plot for:", current_drug, "&", current_rxn)
    
    p <- df1 %>%
      ggplot(aes(x = `.id`, y = `IC_Cumulative`,group = 1)) +
      geom_line() + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("Cumulative Information Component (IC)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
    ggplotly(p)
  })
  
  
  # BCPNN Tab: Event wordcloud based on IC
  output$bcpnn_event_ICwordcloud <- renderPlot({
    df <- as.data.frame(cv_bcpnn_tab()[2])
    
    data <- df %>% arrange(desc(IC)) %>% dplyr::select(event.effect,`IC` ) %>% slice(1:20)
    wordcloud::wordcloud(words = data$event.effect, freq = data$IC, scale = c(1.5, 0.2), colors=brewer.pal(8, "Dark2"))
  })
  
  # BCPNN Tab: Event wordcloud based on adverse event frequency
  output$bcpnn_event_FREQwordcloud <- renderPlot({
    df <- as.data.frame(cv_bcpnn_tab()[2])
    
    data2 <- df %>% group_by(event.effect)%>% dplyr::summarise(count = n()) %>% arrange(desc(count))%>% slice(1:20)
    wordcloud::wordcloud(words = data2$event.effect, freq = data2$count, scale = c(1.5, 0.2), colors=brewer.pal(8, "Dark2"))
  })
  
  # BCPNN Tab: Reactions based on IC associated with selected drug
  output$Rxn_table_bcpnn <- renderDataTable({
    df <- as.data.frame(cv_bcpnn_tab()[2]) 
    data <- df %>% arrange(desc(IC)) %>% select(-c(row.names,drug.code,expected.count,Se,Sp))
    data
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
    ror_tab_wc <- cv_ror %>% filter(drug.code == current_drug, as.numeric(.id) >= as.numeric(start_quarter), as.numeric(.id) <= as.numeric(end_quarter))
    
    return(list(ror_tab_df <- ror_tab_df,
                ror_tab_wc <- ror_tab_wc))
  })
  
  # ROR Time Plot
  output$ror_timeplot <- renderPlotly({
    df <- as.data.frame(cv_ror_tab()[1])
    
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
  
  # ROR Tab: Wordcloud based on ROR
  output$ror_event_RORwordcloud <- renderPlot({
    df <- as.data.frame(cv_ror_tab()[2])
    
    data <- df  %>% arrange(desc(ROR)) %>% dplyr::select(event.effect,ROR ) %>% slice(1:20)
    wordcloud::wordcloud(words = data$event.effect, freq = data$ROR, scale = c(1.5, 0.2), colors=brewer.pal(8, "Dark2"))
  })
  
  # ROR Tab: wordcloud based on adverse event frequency
  output$ror_event_FREQwordcloud <- renderPlot({
    df <- as.data.frame(cv_ror_tab()[2])
    
    data2 <- df %>% group_by(event.effect)%>% dplyr::summarise(count = n()) %>% arrange(desc(count))%>% slice(1:20)
    wordcloud::wordcloud(words = data2$event.effect, freq = data2$count, scale = c(1.5, 0.2), colors=brewer.pal(8, "Dark2"))
  })
  
  # ROR Tab: Reactions based on ROR associated with selected drug
  output$Rxn_table_ror <- renderDataTable({
    df <- as.data.frame(cv_ror_tab()[2]) 
    data <- df %>% arrange(desc(ROR)) %>% select(-c(row.names,FDR))
    data
  })
  
}
shinyApp(ui, server)
