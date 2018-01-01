library(shiny)
library(shinydashboard)
library(googleVis)
library(DT)
library(rhandsontable)
library(corrplot)
library(ggplot2)


#data <- read.csv("C:/Users/Karima/Desktop/data1_democracy.csv", sep=";")

# Header ------------------------------------------------------------

header<-dashboardHeader(title = "Shiny Dashboard")

# Sidebar -----------------------------------------------------------

sidebar<-dashboardSidebar(
  
  sidebarMenu(
   
    sidebarSearchForm(textId="", buttonId="", label = "Search...",icon = icon("search")),
    menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
    
    menuItem(" Data Upload", tabName = "Accueil", icon = icon("upload",lib="glyphicon")),
    menuItem("Descriptive Statistics", tabName = "summary", icon = icon("line-chart"),
             collapsible = 
               menuSubItem('Sub-Item One', tabName = 'sub1'),
             menuSubItem('Data Description', tabName = 'summary') ,
             menuSubItem('Plots', tabName = 'plots')),
    
    menuItem("Regression", tabName = "Regression", icon = icon("table"),
             collapsible = 
               menuSubItem('Sub-Item One', tabName = 'subItem1'),
             menuSubItem('Correlation', tabName = 'subItemOne') ,
             menuSubItem('Regression', tabName = 'subItemTwo'),
             menuSubItem('Residuals', tabName = 'subItemthree')
             
    ),
    menuItem("About the Application", tabName = "about", icon = icon("copyright"))
  )
)#Fin dashboardSidebar

# Body ---------------------------------------------------------------

body <- dashboardBody( 
  fluidPage(theme = "bootstrap.css",
            tags$head(
              tags$style(HTML("
                              @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                              
                              h1 {
                              font-family: 'Lobster', cursive;
                              font-weight: bold;
                              font-size: 24px;
                              line-height: 1.1;
                              color: #ad1d28;  
                              }
                              "))),  ##48ca3b; green color
            tags$head(
               tags$style(HTML('
                    .main-header .logo {
                       font-family: "Georgia", Times, "Times New Roman", serif;
                       font-weight: bold;
                      }
                  '))),
            tabItems(
              
              tabItem(tabName ="Accueil",
                      headerPanel("Data Visualisation and Regression Models"),
                      
                      fluidRow( box( fileInput("file", "Upload data-file:"),background = "aqua"),
                                box( selectInput("readFunction", "Function to read data:", c("read.csv2")),background = "aqua",
                                br()
                                ) ),
                      # Argument field:
                      
                      fluidRow( box(htmlOutput("ArgText"),background = "aqua"),
                                box(htmlOutput("ArgSelect"),background = "aqua"),
                                # Argument selecter:
                                box(title = "Variables names",width=12,solidHeader = TRUE,collapsible = T,
                                    background = "blue" ,
                                    htmlOutput("varselect"),br())
                      ),
                      
                      dataTableOutput('table'),
                      
                      
                      tags$style(type='text/css', ".well { max-width: 20em; }"),
                      # Tags:
                      tags$head(
                        tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
                        tags$style(type="text/css", "select { width: 100%}"),
                        tags$style(type="text/css", "input { width: 19em; max-width:100%}")
                      ),
                      
                      downloadLink('downloadDump', 'Download source'),
                      downloadLink('downloadSave', 'Download binary')
           
              ),#FIN ACCUEIL
              
              tabItem(tabName = "introduction" ,
                      tags$div(
                        tags$h1("Data Democracy Survey :" ,
                                style = "font-family: 'Lobster', cursive;
                                font-weight: 500; line-height: 1.1; 
                                color:#bf0c51 ;"
                        )
                      ),
                      tags$hr(),
                      tags$div(
                        tags$h1("Introduction :" ,
                                style = "font-family: 'Georgia', cursive;
                                font-weight: 100; line-height: 1.1; 
                                color:#361685 ;"),
                        
                        tags$p("Based on our theoretical concept of democracy, it is feasible to measure a country’s quality of 
                               democra-cy for a given point in time. Nevertheless, the quality of the whole endeavor is not only
                                the result of an adequate theoretical concept but equally depends on the quality of the measurement itself.")
                        ),
                      
                        tags$p(" The purpose of this survey is to describe and analyze statistically the democracy's index (the
                           variable **score**) from 163 countries and the different variables that are related to this score."),
                        
                        tags$hr(),
                        
                         fluidRow(box(title = "General description", solidHeader = TRUE, status = "info", width = 12, #background = "light-blue",
                                   "Since we're handling quantitave data , we used Regression Models tools to explore and study the survey.")),
                      
                      
                         fluidRow(box(title = "", width = 12, solidHeader = TRUE, status = "info",#background = "light-blue",
                         "The present Rshiny application is a complete statistical tool for the survey's data analysis built under R 3.4.2."))
                      
                         ),
                    
              tabItem(tabName = "summary" ,
                      tabsetPanel(
                        tabPanel("Data", dataTableOutput("data")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Structure",verbatimTextOutput("structure")),
                        tabPanel("Missing values",
                                 tags$hr(),
                                 fluidRow(# A static valueBox
                                   valueBoxOutput('valueBox')),
                                 dataTableOutput("mis_values")
                                )
                                 )
                    ),
              tabItem(tabName = "plots" ,
                      fluidRow(box(uiOutput('dvY'),background = "aqua"),box( uiOutput('dvX'),background = "aqua")),
                      fluidRow(box(checkboxGroupInput("dataset", strong("You can choose just one graph"),
                                choices =c( "Histogram","Pie Chart","Boxplot","Dot Plot"),#, "Timeseries"),
                                selected = list()),background = "light-blue"),
                               box(verbatimTextOutput("event"),background ="teal")),plotlyOutput('plot')
              ),
              
              tabItem(tabName = "subItemOne",
                      
                      #headerPanel("reg of data"),
                      
                      tabsetPanel(
                        
                        tabPanel("Correlations",                   
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     
                                     selectInput("corMethod", "Correlation Method",c("pearson", "kendall", "spearman")),#eval(formals(cor)$method)),
                                     selectInput("corUse", "NA Action",
                                                 c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")),
                                     tags$hr(),
                                     
                                     #Only works if we are not showing confidence interval
                                     conditionalPanel("!input.showConf",
                                                      selectInput("plotMethod", "Plot Method",
                                                                  list("mixed", all = eval(formals(corrplot)$method)), "circle"),
                                                      conditionalPanel("input.plotMethod === 'mixed'",
                                                                       wellPanel(
                                                                         selectInput("plotLower", "Lower Method", eval(formals(corrplot)$method)),
                                                                         selectInput("plotUpper", "Upper Method", eval(formals(corrplot)$method)))
                                                      )
                                     ),
                                     conditionalPanel("input.showConf || input.plotMethod !== 'mixed'",
                                                      selectInput("plotType", "Plot Type",
                                                                  eval(formals(corrplot)$type))),
                                     
                                     selectInput("plotOrder", "Reorder Correlation",
                                                 eval(formals(corrplot)$order)),
                                     conditionalPanel("input.plotOrder === 'hclust'",
                                                      wellPanel(
                                                        selectInput("plotHclustMethod", "Method",
                                                                    eval(formals(corrplot)$hclust.method)),
                                                        numericInput("plotHclustAddrect", "Number of Rectangles", 3, 0, NA))),
                                     
                                     tags$hr(),
                                     checkboxInput("sigTest", "Significance Test"),
                                     conditionalPanel("input.sigTest",
                                                      numericInput("sigLevel", "Significane Level",
                                                                   0.05, 0, 1, 0.01),
                                                      selectInput("sigAction", "Insignificant Action",
                                                                  eval(formals(corrplot)$insig))),
                                     checkboxInput("showConf", "Show Confidence Interval"),
                                     conditionalPanel("input.showConf",
                                                      selectInput("confPlot", "Ploting Method",
                                                                  eval(formals(corrplot)$plotCI)[-1]),
                                                      numericInput("confLevel", "Confidence Level",
                                                                   0.95, 0, 1, 0.01))
                                   ),
                                   
                                   # Show a plot of the generated correlation
                                   mainPanel(
                                     
                                     column(1, "variablesStyle"="Checkbox",
                                            
                                            conditionalPanel("input.variablesStyle === 'Selectize'",
                                                             sortableSelectizeInput("variables", "", c("Loading..."), multiple = T, options = list(plugins = list("remove_button")))
                                            )),
                                     column(9, 
                                            plotOutput("corrPlot", height = 500),
                                            uiOutput("warning"))
                                     
                                   )
                                 )
                        )  #    
                        
                        
                      )),
              tabItem(tabName = "subItemTwo",
                      
                      #headerPanel(""),
                      
                      tabsetPanel(
                        tabPanel("Bivariate Regression",
                                 fluidRow(box(uiOutput('bivarY'),background = "aqua"),
                                          box( uiOutput('bivarX'),background = "aqua")),
                                 box(verbatimTextOutput("msg1"),background = "blue",width=15),
                                 verbatimTextOutput("modelbivar"),
                                 box(verbatimTextOutput("msg11"),background = "blue",width=15),
                                 verbatimTextOutput("conft3")
                        ),
                        
                        tabPanel("Polynomial Regression",
                                 box(htmlOutput("varselect2"),background = "aqua"),
                                 box(textInput("X", label = "Enter your model", value = "Y~X1+X2..."),background = "aqua",height = 120),
                                 br(),br(),
                                 box(verbatimTextOutput("msg2"),background = "blue",width=12),
                                 verbatimTextOutput("model"),
                                 box(verbatimTextOutput("msg22"),background = "blue",width=12),
                                 verbatimTextOutput("conft")
                        )
                        
                      )),
              
              
              tabItem(tabName = "subItemthree",
                      
                      tabsetPanel(
                        
                        tabPanel("Bivariate Regression Residuals",
                                 
                                 plotOutput("residuals_hist"),
                                 plotOutput("error1")  
                       
                        ),
                        tabPanel("Polynomial Regression Residuals",
                                 
                                 plotOutput("residuals_hist3"),
                                 plotOutput("error3")  
                        )
                        
                      )),

              
              
              #FIN summary
              
              
              tabItem(tabName = "about" , 
                      
                      HTML('<center><img src = "pic.png", width = "250px", height = "200px"></center>'),
                      tags$div(
                        tags$h1("Powered by:" ,
                                style = "font-family: 'Lobster', cursive;
                                font-weight: 500; line-height: 1.1; 
                                color:#5824DA ;"
                        )
                      ),
                      tags$hr(),
                      tags$div(
                        HTML("<h1> <center> <strong> Karima Touati </strong></center></h1>")
                      ),
                      tags$hr(),
                    
                      
                      tags$div(
                        tags$p("This Shiny Application is for Data Visualisation and Regression Modeling : 
                               it allows to import a .csv file and to visualize the  variables.")
                              ),
                      
                      tags$div(
                        tags$p("In fact, It offers the possibility to make a general summary of the Data and 
                               to represent the variables using  many types of graphics such as histogram, pie-chart, boxplot, 
                               and dot-plot.")
                             ),
                      
                      tags$div(
                        tags$p("It also allows to test the correlation between the variables, to apply several regression methods 
                               like Bivariate Regression and Polynomial Regression (for the quantitative variables)
                              and to study  the residuals of each model.")
                              )
                                   )#Fin about
                                 ) ))# FIN dashboardBody


dashboardPage(header,sidebar, body,skin ="purple") 
shinyUI(dashboardPage(header,sidebar, body,skin ="purple"))


