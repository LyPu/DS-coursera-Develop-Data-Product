rm(list = ls())
library(shiny)
library(shinydashboard)
library(xts)
library(DT)
library(quantmod)
library(PerformanceAnalytics)

ui <- dashboardPage( 
    skin="purple",
    dashboardHeader(title = "Stock Price Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Data", tabName = "data", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(
                            title = "Controls",
                            textInput("stock", "Please input a stock ticker:", "AAPL"),
                            dateRangeInput("dates", "Please input a date range:", start = "2020-03-01", end="2020-04-26"),
                            checkboxInput("log", "Plot y axis on log scale", FALSE),
                            submitButton("Submit")
                        ),
                        box(plotOutput("plot1", height = 250)),
                        box(plotOutput("plot2", height = 250)),
                    )
            ),
            tabItem(tabName = "data",
                   fluidPage(
                       h1("Data"),
                       dataTableOutput("stockdata")
                   )
            )
        )
    )
        
)

server <- function(input, output) {
    dataInput <- reactive({
        getSymbols(input$stock, src = "yahoo",
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)
    })
    
    output$plot1 <- renderPlot({   
        data <- dataInput()
        chartSeries(data, theme = chartTheme("white"),
                    type = "line", log.scale = input$log, TA = NULL)
    })
    output$plot2 <- renderPlot({   
        data <- dataInput()
        candleChart(data, multi.col=TRUE, theme = chartTheme("white"))
    })
    
    
    output$stockdata <- DT::renderDataTable(datatable(as.data.frame(dataInput())))
    #output$stockdata <- DT::renderDataTable(datatable(as.data.frame(Cl(dataInput()))))
}

shinyApp(ui, server)