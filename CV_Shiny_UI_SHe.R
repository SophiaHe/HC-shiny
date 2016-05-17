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