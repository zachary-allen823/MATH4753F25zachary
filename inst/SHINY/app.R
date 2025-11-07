library(shiny)

ui <- fluidPage(
  titlePanel("Simple MLE Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution:", c("Normal","Exponential")),
      numericInput("n", "Sample size", 100, min=1),
      actionButton("go", "Simulate and Estimate")
    ),
    mainPanel(
      plotOutput("histPlot"),
      verbatimTextOutput("outText")
    )
  )
)

server <- function(input, output, session){
  data <- eventReactive(input$go,{
    if (input$dist=="Normal") rnorm(input$n, 0, 1)
    else rexp(input$n, 1)
  })

  output$histPlot <- renderPlot({
    x <- data()
    req(x)
    hist(x, probability=TRUE, main=paste("Histogram of", input$dist))
  })

  output$outText <- renderPrint({
    x <- data()
    req(x)
    if (input$dist=="Normal") {
      cat("MLE mean:", mean(x), "MLE sd:", sqrt(sum((x-mean(x))^2)/length(x)))
    } else {
      cat("MLE rate:", 1/mean(x))
    }
  })
}

shinyApp(ui, server)

