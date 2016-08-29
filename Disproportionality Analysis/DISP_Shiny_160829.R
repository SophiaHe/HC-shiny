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

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_prr <- tbl(hcopen, "PRR_160826") %>% as.data.frame()%>% select(-row.names)
cv_bcpnn <- tbl(hcopen, "IC_160829")%>% as.data.frame()%>% select(-c(row.names,count,`expected count`,`n11/E`,`drug margin`, `event margin`,FDR,FNR,Se,Sp,postH0))
names(cv_bcpnn)[names(cv_bcpnn)=="drug code"] <- "drug_code"
names(cv_bcpnn)[names(cv_bcpnn)=="event effect"] <- "event_effect"

cv_ror <- tbl(hcopen, "ROR_160826") %>% as.data.frame()%>% select(-row.names)
cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn")

cv_drug_rxn_2006 <- cv_drug_rxn %>% filter(quarter >= 2006.1)
count_df_quarter <- dplyr::summarise(group_by(cv_drug_rxn_2006,ing,PT_NAME_ENG,quarter), count = n_distinct(REPORT_ID)) %>% as.data.frame()

master_table <- cv_prr %>% inner_join(cv_bcpnn) %>% inner_join(cv_ror) %>% filter(is.na(ROR) != TRUE)
master_table <- plyr::rename(master_table,c("Q_0.025(log(IC))" = "LB95_IC", "Q_0.975(log(IC))" = "UB95_IC", "LB95_CI_PRR" = "LB95_PRR"))

head(cv_prr)
head(cv_bcpnn)
head(cv_ror)
head(master_table)

# drug and adverse event dropdown menu
topdrugs <- cv_prr %>% dplyr::distinct(drug_code) %>% as.data.frame()
toprxns <- cv_prr %>% semi_join(topdrugs) %>% dplyr::distinct(event_effect)%>% as.data.frame()

############ UI for DISP shiny ################
ui <- dashboardPage(
  dashboardHeader(title = "CV Shiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("fa fa-cogs")),
      # menuItem("BCPNN", tabName = "bcpnndata", icon = icon("fa fa-binoculars")),
      # menuItem("ROR", tabName = "rordata", icon = icon("fa fa-database")),
      # # menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    selectizeInput("search_generic", 
                   "Generic Name/Ingredient",
                   topdrugs$drug_code,
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
    actionButton("searchButton", "Search")
  ), 
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                tabBox(
                  tabPanel(
                    "Time Plot",
                    plotlyOutput(outputId = "top10_prr_timeplot"),
                    tags$br(),
                    tags$p("Proportional Reporting Ratio (PRR) results by quarter. 
                           Value of PRR indicates the signal strength of the particular Drug & Adverse_reaction pair. 
                           Details can be found in DISP_about documentation"),
                    dataTableOutput("master_table"),
                    width = 12),
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

############################# Server of DISP Shiny #####################
server <- function(input, output, session) {
  menu_options <- reactive({if(is.na(input$search_generic) == TRUE){
    drug_option <- "OXYCODONE HYDROCHLORIDE"
  } else {
    drug_option <- input$search_generic 
  }
    
    # adverse events available 
    event_option <- cv_prr %>% dplyr::filter(drug_code == drug_option) %>% dplyr::distinct(event_effect) %>% dplyr::select(event_effect) #%>% arrange(event.effect)
    
    return(event_option <- event_option)
  })
  
  # Design Reaction Dropdown Menu based on Drug selection
  observe({
    updateSelectInput(session, "search_rxn",
                      label = "Select Adverse Event:",
                      choices = as.data.frame(menu_options())$event_effect)
  })
  
  # PRR tab 
  cv_prr_tab <- reactive({
    input$searchButton
    #codes about dplyr::select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "ALL",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "ALL",
                                  input$search_rxn))
    
    # Inf in PRR and ROR means there are no other drug associated with that specific adverse reaction, so denomimator is zero!
    # master_table[master_table$event_effect == "Refraction disorder",]
    
    if(current_drug == "ALL" & current_rxn == "ALL"){
      default_pairs <- master_table %>% filter(PRR != Inf) %>% arrange(desc(PRR)) %>% top_n(10, PRR) %>% select(drug_code, event_effect)
      default_count_df <- count_df_quarter[count_df_quarter$ing %in% default_pairs$drug_code & count_df_quarter$PT_NAME_ENG %in% default_pairs$event_effect,]
      
      timeplot_df <- default_count_df
      prr_tab_df <- master_table
    } else {
      timeplot_df <- count_df_quarter %>% filter(ing == current_drug, PT_NAME_ENG == current_rxn)
      prr_tab_df <- master_table %>% filter(drug_code == current_drug, event_effect == current_rxn)
    }

    return(list(
      timeplot_df <- timeplot_df,
      prr_tab_df <- prr_tab_df))
  })
  
  
  # PRR Time Plot
  output$top10_prr_timeplot <- renderPlotly({
    df <- as.data.frame(cv_prr_tab()[1])
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "All Drugs",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "All Reactions",
                                  input$search_rxn))
    plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)
    
    p <- df %>%
      ggplot(aes(x = quarter, y = count,group = ing)) +
      geom_line() + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("Report Count") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
    ggplotly(p)
    
  })

  # NEED WORK HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # head(default_count_df)
  # default_count_df <- default_count_df %>% mutate(pair = paste(ing, "&", PT_NAME_ENG))
  # p <- default_count_df %>%
  #   ggplot(aes(x = quarter, y = count)) +
  #   geom_line(aes(colour=pair)) + 
  #   geom_point()+
  #   #ggtitle(plottitle) + 
  #   xlab("Quarter") + 
  #   ylab("Report Count") +
  #   theme_bw() +
  #   theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
  # ggplotly(p)
  
  
  # PRR Tab: Reactions based on PRR associated with selected drug
  output$master_table <- renderDataTable({
    df <- as.data.frame(cv_prr_tab()[2]) 
    df <- df %>%  select(-c(log_PRR,LB95_log_PRR,UB95_log_PRR,log_ROR,UB95_log_ROR,LB95_log_ROR))
    data <- df[c(1,2,3,5,4,8,6,7,9,11,10)]
    data
  })
  
}
shinyApp(ui, server)