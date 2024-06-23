
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("BI Data Visualization Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis Variable:", choices = names(mtcars)),
      selectInput("yvar", "Y-axis Variable:", choices = names(mtcars), selected = "mpg"),
      selectInput("colorvar", "Color Variable:", choices = c("None", names(mtcars))),
      selectInput("facetvar", "Facet by Variable:", choices = c("None", names(mtcars))),
      sliderInput("bins", "Number of Bins:", min = 5, max = 50, value = 20),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
        tabPanel("Histogram", plotlyOutput("histPlot")),
        tabPanel("Summary Table", DTOutput("summaryTable"))
      )
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    mtcars
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(aes_string(color = ifelse(input$colorvar == "None", NULL, input$colorvar))) +
      theme_minimal()
    
    if (input$facetvar != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facetvar)))
    }
    
    ggplotly(p)
  })
  
  output$histPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes_string(x = input$xvar)) +
      geom_histogram(bins = input$bins, fill = "blue", alpha = 0.7) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$summaryTable <- renderDT({
    datatable(filteredData())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
