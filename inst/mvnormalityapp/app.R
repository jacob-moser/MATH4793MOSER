
library(shiny)
library(ggplot2)


ui <- fluidPage(

  titlePanel("Interactive Mulitvariate Normality App"),

  #Arrange inputs and outputs on shiny dashboard. uiOutput allows for dynamic inputs adjusting to uploaded data

  sidebarLayout(
    sidebarPanel(
      fileInput("file",
                "Upload a dataset as a .csv file - A dataset must be uploaded for the interface to load properly",
                accept = ".csv"),
      uiOutput("var1"),
      uiOutput("var2"),
      uiOutput("var3")
    ),

    mainPanel(
      tableOutput("proportiontest"),
      tableOutput("violinProp"),
      plotOutput("normqq"),
      plotOutput("chisqplot"),
      textOutput("bivariatenormcheck")
    )
  )
)

server <- function(input, output) {

  # reactive (only executes on command) function to pull the data set uplaoded

  data <- reactive({
    as.data.frame(read.csv(input$file$datapath))
  })

  # execute univariate proportion normality test and render the table output on the app interface

  output$proportiontest <- renderTable({
    MATH4793MOSER::proportionnormaltest(data())
  })

  # Render violin plot showing univariate distribution and standard deviation ranges

  output$violinvar <- renderUI({
    varSelectInput("violvar", "Variable for the Violin Plot",data())
  })

  output$violinProp <- renderTable({
    dat <- data()
    data <- data.frame(dat[1,])

    data
  })

  ## Renders Chi Squared QQ Plot for data set... consider moving to an independent function?

  output$chisqplot <- renderPlot({
    data <- data()
    xbar <- matrix(colMeans(data), nrow = length(data[1,]), ncol = 1)
    S <- cov(data)
    Sinv <- solve(S)
    d <- matrix(nrow = length(data[,1]), ncol = 1)
    for(i in 1:length(data[,1])){
      d[i,1] = t(t(data[i,])-xbar)%*%Sinv%*%(t(data[i,])-xbar)
    }
    d <- sort(d)
    chivals <- matrix(nrow = length(data[,1]),ncol = 1)
    for(i in 1: length(data[,1])){
      chivals[i,1] = qchisq((i-0.5)/length(data[,1]),df=3)
    }

    plot(chivals,d, main = "Chi-Squared QQ Plot", xlab = "Theoretical Quantiles", ylab = "Computed Quantiles")

  })

  ## Render standard normal qq plot for data set

  output$normqq <- renderPlot({
    data <- data()
    a <- data[,toString(input$var3)]
    vals <- univariateqq(a)
    plot(vals$TheoreticalQuantiles, vals$Data, main = "Standard Normal QQ Plot for Selected Variable", xlab = "Theoretical Quantiles", ylab = "Actual Data")

  })

  ## Render variable choices for bivariate normal evaluation

  output$var1 <- renderUI({
    varSelectInput("var1","First Variable for Bivariate Normality Check",data())
  })

  output$var2 <- renderUI({
    varSelectInput("var2","Second Variable for Bivariate Normality Check",data())
  })

  ## Input for dynamic standard QQ plot

  output$var3 <- renderUI({
    varSelectInput("var3","Variable for Univariate Standard Normal QQ Plot",data())
  })

  ## Output of the bivariate normality check

  output$bivariatenormcheck <- renderPrint({
    dat <- data()
    bivariatenormcheck(dat[,toString(input$var1)],dat[,toString(input$var2)])
  })

}



# Run the application
shinyApp(ui = ui, server = server)
