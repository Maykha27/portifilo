library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(DT)


ui <- dashboardPage(
  dashboardHeader(
    title = "Look.data",
    dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2023-03-02"
                               )
                  ),
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "5 new users today",
                   icon("users")
                 ),
                 notificationItem(
                   text = "12 items delivered",
                   icon("truck"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "Server load at 86%",
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
    ),
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Documentation"
                 ),
                 taskItem(value = 1, color = "aqua",
                          "Project X"
                 ),
                 taskItem(value = 5, color = "yellow",
                          "Server deployment"
                 ),
                 taskItem(value = 80, color = "red",
                          "Overall project"
                 )
    )
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Paramètre", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h1("Lecture des données"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              h3("Parametres"),
              # Input: Checkbox if file has header
              radioButtons(inputId = "header", 
                           label = "Header",
                           choices = c("Yes" = TRUE,
                                       "No" = FALSE),
                           selected = TRUE, inline=T),
              # Input: Select separator ----
              radioButtons(inputId = "sep", 
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "t"),
                           selected = "t", inline=T),
              # Input: Select quotes ----
              radioButtons(inputId = "quote", 
                           label= "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = "", inline=T),
              h3("File preview"),
              dataTableOutput(outputId = "preview")
      ),
      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              h2("Exploration du tableau"),
              sidebarLayout(
                sidebarPanel(
                  selectInput('plot_type',
                              'choisis le type de graphique',
                              choices = c('Histogram', 'Scatter Plot')),
                  selectInput('var', 'choisie une variable :', choices = ""),
                  conditionalPanel(
                    condition = "input.plot_type == 'Scatter Plot'",
                    selectInput('var2', 'choisis une 2e variable :', choices = "")
                  )
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel('Data', DTOutput('khady'),
                             downloadButton('save_data', 'save to csv')),
                    tabPanel('statistiques', verbatimTextOutput('summary')),
                    tabPanel('Graphique', 
                             conditionalPanel(
                               condition = "input.plot_type == 'Histogram'",
                               plotOutput('hist')),
                             
                             conditionalPanel(
                               condition = "input.plot_type == 'Scatter Plot'",
                               plotOutput('nuage'))
                    )
                  )
                )),
              
              h2("Graphiques"),
              fluidRow(
                column(10,plotOutput("plotAvecR") )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  #résume statistique
  output$summary <- renderPrint({
    summary()
  })
  
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  myData <- reactive({
    inFile <- input$dataFile
    if (is.null(inFile)) {
      d <- return(input$dataFile$datapath)
    } else {
      d <- read.csv(input$dataFile$datapath,
                    header = as.logical(input$header),
                    sep = input$sep,
                    quote = input$quote)
    }
    d
  })
  base3 <- reactive({
    read.csv(input$dataFile$datapath,
                      header = as.logical(input$header),
                      sep = input$sep,
                      quote = input$quote)

    })

  
  
# observe({
#   if(is.null(input$dataFile)){
#     return(NULL)
#   }
#   updateSelectInput(session, 'var', label = "Test",
#                     choices = base3)
#   # updateSelectInput(session, 'var2', label = "Test22",
#   #                   choices = colnames(base3()))
#   })


  output$khady <-  renderDT({
    # base <- read.csv(input$dataFile$datapath,
    #                  header = as.logical(input$header),
    #                  sep = input$sep,
    #                  quote = input$quote
    # )
    base3()
  })
  
  #Résumé statistique
  output$summary <- renderPrint({
    # base <- read.csv(input$dataFile$datapath,
    #                  header = as.logical(input$header),
    #                  sep = input$sep,
    #                  quote = input$quote
    # )
    summary(base3())
  })
  
  # histogramme
  output$hist <- renderPlot({
    # base <- read.csv(input$dataFile$datapath,
    #                  header = as.logical(input$header),
    #                  sep = input$sep,
    #                  quote = input$quote
    # )
    hist(base3()[, input$var], main = 'Histogramme', xlab = input$var)
  })
  
  #sauvegarde de df au format csv
  output$save_data <- downloadHandler(
    filename <- function(){
      paste("data_", Sys.Date(),".csv", sep = ',')
    },
    content <- function(file){
      write.csv(df(), file)
    }
  )
  
  # Nuage de points
  output$nuage <- renderPlot({
    # base <- read.csv(input$dataFile$datapath,
    #                  header = as.logical(input$header),
    #                  sep = input$sep,
    #                  quote = input$quote
    # )
    plot(base3()[, input$var], base3()[, input$var2],
         xlab = input$var, ylab = input$var2)
  })
  
  
  
  output$preview <-  renderDataTable({
    
    req(input$dataFile)
    
    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   nrows=4000
    )
  },  options = list(scrollX = TRUE , dom = 't'))
  
  observe({
    updateSelectInput(session, "var",
                      label = "choisie une variable :",
                      choices = colnames(myData()))
    
    updateSelectInput(session, "var2",
                      label = "choisie une 2è variable :",
                      choices = colnames(myData()))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
