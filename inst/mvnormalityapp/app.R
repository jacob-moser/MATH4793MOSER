
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
      uiOutput("var3"),
      uiOutput("var4"),
      sliderInput("alpha", "alpha value to draw confidence ellipse around points being checked for bivariate normality", min = 0, max = 1, value = 0.05, step = 0.01)
    ),

    mainPanel(
      tableOutput("proportiontest"),
      plotOutput("violinProp"),
      plotOutput("normqq"),
      plotOutput("chisqplot"),
      textOutput("bivariatenormcheck"),
      plotOutput("confidenceellipse")
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

  output$violinProp <- renderPlot({
    data <- data()
    dat <- data
    a <- vector()
    for(i in 1:length(dat[1,])){
      a <- c(a,dat[,i])
    }

    b <- vector()
    for(i in 1:length(dat[1,])){
      b <-c(b,rep(names(dat)[i],length(dat[,1])))
    }

    d <- vector()
    for(j in 1:length(dat[1,])){
      for(i in 1:length(dat[,1])){
        if(abs(dat[i,j]-mean(dat[,j])) <= var(dat[,j])^0.5){
          d <- c(d, "Within 1 Standard Deviation of Mean")
        }
        if(abs(dat[i,j]-mean(dat[,j])) <= 2 * var(dat[,j])^0.5 & abs(dat[i,j]-mean(dat[,j])) > var(dat[,j])^0.5){
          d <- c(d, "Within 2 Standard Deviations of Mean")
        }
        if(abs(dat[i,j]-mean(dat[,j])) > 2 * var(dat[,j])^0.5){
          d <- c(d, "Greater Than 2 Standard Deviations Away from Mean")
        }
      }
    }

    use <- data.frame(matrix(c(a,b,d), nrow = length(a), ncol = 3))

    g <- ggplot2::ggplot(use, mapping = ggplot2::aes(x = X2, y = as.numeric(as.character(X1)))) + ggplot2::geom_violin() + ggplot2::geom_point(aes(color = X3)) +
      ggplot2::xlab("Variable") + ggplot2::ylab("Value")
    g
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

  ## Output p-values and rq values for each variable

  output$pvalsandrq <- renderTable({
    dat <- data()
    rq(dat[,1])$coeff

  })

  ## Confidence ellipse for the variables being checked for bivariate normality

  output$confidenceellipse <- renderPlot({
    dat <- data()
    use <- matrix(c(dat[,toString(input$var1)],dat[,toString(input$var2)]), nrow = length(dat[,toString(input$var1)]), ncol = 2)

    xbar <- matrix(colMeans(use), nrow = 2, ncol = 1)
    S <- cov(use)
    chiquant <- qchisq(1-input$alpha,2)

    use2 <- data.frame(use)
    use2$X1 <- as.numeric(as.character(use2$X1))
    use2$X2 <- as.numeric(as.character(use2$X2))

    e <- eigen(S)

    g <- ggplot2::ggplot(use2, ggplot2::aes(x = X1, y = X2)) + ggplot2::geom_point() + ggplot2::ggtitle("Confidence Ellipse") + ggplot2::xlab("First Variable") + ggplot2::ylab("Second Variable")
    g <- g + ggforce::geom_ellipse(ggplot2::aes(x0 = xbar[1,1], y0 = xbar[2,1], a = (e$values[1]*chiquant)^0.5, b = (e$values[2]*chiquant)^0.5, angle = atan(e$vectors[2,1]/e$vectors[1,1])))
    g
  })

}



# Run the application
shinyApp(ui = ui, server = server)
