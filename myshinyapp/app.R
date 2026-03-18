#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui<- fluidPage(
  theme = bslib::bs_theme(bootswatch = "sandstone"),
  titlePanel("Linear Regression Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("samplesize", label = "Sample size is:", min = 0, max = 100, value = 20),
      actionButton("sample", "Sample!"),
      verbatimTextOutput("modelinfo")
    ),
    mainPanel(
      plotOutput("scatter")
    )
  )
)

server <- function(input, output, session) {
  x <- eventReactive(input$sample,{rnorm(n=input$samplesize,
                      mean=runif(1,min=0,max=100),
                      sd=runif(1,min=1,max=10)
  )})
  alpha <- eventReactive(input$sample,{runif(1,min=0,max=20)})
  beta <- eventReactive(input$sample,{runif(1,min=-1,max=1)})
  errors <- eventReactive(input$sample,{rnorm(n=input$samplesize,mean=0,sd=runif(1,min=0,max=3))})
  y<- eventReactive(input$sample,{alpha()+x()*beta()+errors()})
  output$scatter<- renderPlot({
  tempx <- x()
  tempy <- y()
  data <- data.frame(tempx,tempy)
  model <- lm(tempy ~ tempx,data=data)
  scatter <- par(mfrow=c(1,2))
  plot(data,
       pch=20,
       col="navy",
       main="Scatter Plot",
       xlab="x values",
       ylab="y values")
  abline(lm(tempy~tempx), col="maroon")
          plot(
           fitted(model), resid(model),
           main = "Residual Plot",
           xlab = "Fitted Values",
           ylab = "Residuals",
           pch = 20, col = "navy"
         )
         abline(h = 0, col = "maroon", lwd = 2)
         print(scatter)
  })
  output$modelinfo <- renderPrint({
    x <- x()
    y <- y()
    data <- data.frame(x,y)
    model <- lm(y ~ x,data=data)
    print(round(coefficients(model)[1],2))
    print(round(coefficients(model)[2],2))
  })
}

shinyApp(ui, server)
