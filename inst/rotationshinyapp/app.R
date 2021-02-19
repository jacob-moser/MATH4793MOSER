
library(shiny)
library(ggplot2)
library(dplyr)
library(rvest)

my.tilde <- function(x1, x2, thet){
  x1t = x1*cos(thet) + x2*sin(thet)
  x2t = x1*-1*sin(thet) + x2*cos(thet)
  list(x1t=x1t,x2t=x2t)
}

ui <- fluidPage(

  titlePanel("Interactive Data Dashboard"),

  #Arrange inputs and outputs on shiny dashboard. uiOutput allows for dynamic inputs adjusting to uploaded data

  sidebarLayout(
    sidebarPanel(
      fileInput("file",
                "Upload a dataset as a .csv file - A dataset must be uploaded for the interface to load properly",
                accept = ".csv"),
      uiOutput("xvar"),
      uiOutput("yvar"),
      uiOutput("option1"),
      uiOutput("option2"),
      sliderInput("theta",
                  "Theta Value for Axis Rotation",
                  min = 0,
                  max = round(2*pi,4),
                  value = 0)
    ),

    mainPanel(
      plotOutput("Plot1", click = "plot_click"),
      textOutput("docorrelation"),
      plotOutput("Plot2"),
      textOutput("rotcor"),
      textOutput("thetsolve")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # reactive (only executes on command) function to pull the data set uplaoded

  data <- reactive({
    as.data.frame(read.csv(input$file$datapath))
  })

  # server code to enable dynamic inputs

  output$xvar <- renderUI({
    varSelectInput("var1","X Variable:",data())
  })

  output$yvar <- renderUI({
    varSelectInput("var2","Y Variable:",data())
  })

  output$option1 <- renderUI({
    varSelectInput("var3","Variable for Color",data())
  })

  output$option2 <- renderUI({
    varSelectInput("var4","Variable for Size",data())
  })

  # server code to display the drop one correlation upon plot click

  output$docorrelation <- renderPrint({
    req(input$plot_click)
    dat1<-as.data.frame(data())
    dat2<-nearPoints(dat1,input$plot_click,allRows = TRUE)
    dat3<-filter(dat2,selected_ == FALSE)
    core<-cor(dat3[,toString(input$var1)],dat3[,toString(input$var2)])
    cat("The drop-one correlation for the X and Y variables excluding the selected point is ", core)
  })

  # server code to print the rotated correlation coefficient for the given data and selected angle of rotation

  output$rotcor <- renderPrint({
    dat1<-as.data.frame(data())
    x1<-my.tilde(dat1[,toString(input$var1)],dat1[,toString(input$var2)],input$theta)$x1t
    x2<-my.tilde(dat1[,toString(input$var1)],dat1[,toString(input$var2)],input$theta)$x2t
    rotcore<-cor(x1,x2)
    cat("The sample correlation with the given angle of rotation is: ", rotcore)
  })

  # first quandrant value that zeros the rotated covariance

  output$thetsolve <- renderPrint({
    dat1<-as.data.frame(data())
    x1<-dat1[,toString(input$var1)]
    x2<-dat1[,toString(input$var2)]
    s11<-cov(x1,x1)
    s12<-cov(x1,x2)
    s22<-cov(x2,x2)
    s12t = function(x) {(s22-s11)*sin(x)*cos(x)+s12*(cos(x)^2-sin(x)^2)}
    soln<-uniroot(s12t,c(0,0.5*pi))
    solnroot<-soln$root
    cat("The first quadrant theta value (in radians) that will zero this rotated covariance for the given X and Y variables is: ", solnroot)
  })

  # output for plot

  output$Plot1 <- renderPlot({
    g <- ggplot(data(), aes(x = !!input$var1, y = !!input$var2)) + geom_point(aes(color = !!input$var3,size = !!input$var4))
    g
  })

  # second plot, significant extra code is to ensure appropriate scaling of ggplot so user can see axes rotating with change in input angle

  output$Plot2 <- renderPlot({
    dat1<-as.data.frame(data())
    var1<-toString(input$var1)
    var2<-toString(input$var2)
    xmin<-min(dat1[,var1])
    xmax<-max(dat1[,var1])
    ymin<-min(dat1[,var2])
    ymax<-max(dat1[,var2])
    xx<-abs(max(xmin,xmax))
    yy<-abs(max(ymin,ymax))

    ggplot(data(), aes(x = !!input$var1, y = !!input$var2)) + geom_point() +
      geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
      coord_cartesian(xlim=c(min(-1,xmin-1),max(1,xmax+1)),ylim = c(min(-1,ymin-1),max(1,ymax+1))) +
      geom_segment(aes(x=my.tilde(-2*xx,0,input$theta)$x1t,xend=my.tilde(2*xx,0,input$theta)$x1t,y=my.tilde(-2*xx,0,input$theta)$x2t,yend=my.tilde(2*xx,0,input$theta)$x2t,colour="X tilde")) +
      geom_segment(aes(x=my.tilde(0,-2*yy,input$theta)$x1t,xend=my.tilde(0,2*yy,input$theta)$x1t,y=my.tilde(0,-2*yy,input$theta)$x2t,yend=my.tilde(0,2*yy,input$theta)$x2t,colour="Y tilde"))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
