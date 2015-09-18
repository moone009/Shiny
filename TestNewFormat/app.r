library(shiny)
library(markdown)
library(ggplot2)
library(gridExtra)
library(data.table)

#Pre Processing______________________________________________________________________________
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears")) 
mtcars$am <- factor(mtcars$am,levels=c(0,1),
                    labels=c("Automatic","Manual")) 
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
                     labels=c("4cyl","6cyl","8cyl")) 
CallCenter = fread('CallCenter.csv',sep=",",header=T)
#____________________________________________________________________________________________

server =shinyServer(function(input, output, session) {
  
  output$GGplot <- renderPlot({
    p <- qplot(CallCenter[[input$variable]], data=CallCenter, fill=CallCenter[[input$variable2]])
    print(p)
  })
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderDataTable({
    summary(mtcars)
  })
  
  output$table <- renderDataTable({
    mtcars
  }, options=list(pageLength=10))
  
  
  output$plotThree <- renderPlot({
    qplot(mtcars$cyl)
  })
  
  output$Call_Center_Plot <- renderPlot({    
    graph1 = qplot(mtcars$cyl)
    graph2 = qplot(mtcars$vs)
    graph3 = qplot(mtcars$mpg)
    graph4 = qplot(mtcars$disp)
    grid.arrange( graph1, graph2,graph3,graph4, ncol=2)
        
  })
  output$Car_Center_Plot <- renderPlot({    
    
    # Kernel density plots for mpg
    # grouped by number of gears (indicated by color)
    graph1 = qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5), 
                   main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
                   ylab="Density")
    
    # Scatterplot of mpg vs. hp for each combination of gears and cylinders
    # in each facet, transmittion type is represented by shape and color
    graph2 = qplot(hp, mpg, data=mtcars, shape=am, color=am, 
                   facets=gear~cyl, size=I(3),
                   xlab="Horsepower", ylab="Miles per Gallon") 
    
    # Separate regressions of mpg on weight for each number of cylinders
    graph3 = qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), 
                   method="lm", formula=y~x, color=cyl, 
                   main="Regression of MPG on Weight", 
                   xlab="Weight", ylab="Miles per Gallon")
    
    # Boxplots of mpg by number of gears 
    # observations (points) are overlayed and jittered
    graph4 = qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"), 
                   fill=gear, main="Mileage by Gear Number",
                   xlab="", ylab="Miles per Gallon")
    
    grid.arrange( graph1, graph2,graph3,graph4, ncol=2)
    rm(mtcars)
    
  })
  
  #This plot is added to Car_Center_plot
  output$test = renderDataTable({
      mtcars
    }, options=list(pageLength=10))
  
  

  output$ContactCenterPlots <- renderPlot({    
  graph1 = qplot(CallType, data=CallCenter, fill=Territory, alpha=I(.7), 
                 main="Distribution of Calls", xlab="Call Type", 
                 ylab="Count")
  graph2 = qplot(CompanyType, data=CallCenter, fill=EstimatedCompanyRev, alpha=I(.7), 
                 main="", xlab="Company Type", 
                 ylab="Count")
  grid.arrange( graph1, graph2, ncol=1)
  
  })
  #This plot is added to ContactCenterPlots
  output$ContactCenterReport = renderDataTable({
    CallCenter
  }, options=list(pageLength=10))
  
  
})

ui <- shinyUI(navbarPage("Milwaukee Power and Light Company!",
                         tabPanel("Plot",
                                  sidebarLayout(
                                    
                                    sidebarPanel(
                                      
                                      radioButtons("plotType", "Plot type",
                                                   c("Scatter"="p", "Line"="l")),
                                      
                                      selectInput("man", 
                                                  "CompanyType:", 
                                                  c("All", 
                                                    unique(as.character(CallCenter$CompanyType)))),
                                      
                                      selectInput("trans", 
                                                  "CallType:", 
                                                  c("All", 
                                                    unique(as.character(CallCenter$CallType)))),
                                      
                                      
                                      selectInput("variable", "First variable:",
                                                  list(
                                                       "Call Type"  = "CallType",
                                                       "Current Customer" = "CurrentCustomer",
                                                       "Territory"  = "Territory")),
                                      
                                      selectInput("variable2", "Second variable:",
                                                  list(
                                                       "Call Type"  = "CallType",
                                                       "Current Customer" = "CurrentCustomer",
                                                       "Territory"  = "Territory")) 
                                      
                                      
                                    ),
                                    mainPanel(
                                      plotOutput("plot"),
                                      plotOutput("GGplot")
                                    )
                                  )
                         ),
                         
                         tabPanel("Health And Safety",
                                  dataTableOutput("summary")
                         ),
                         tabPanel("Credit And Collections",
                                  plotOutput("plotThree")
                          ),
                         
                         navbarMenu("Call Center",
                                    
                         tabPanel("Call Center Plot",
                                  plotOutput("Call_Center_Plot")
                         ),            
                         tabPanel("Car Center Plot",
                                  plotOutput("Car_Center_Plot"),
                                  dataTableOutput("test")
                         ),
                         tabPanel("Contact Center Reporting",
                                  plotOutput("ContactCenterPlots"),
                                  dataTableOutput("ContactCenterReport")
                         )),   
                         
                         navbarMenu("Meter-To-Bill",
                                    tabPanel("Table",
                                             dataTableOutput("table")
                                    ),
                                    tabPanel("About",
                                             fluidRow(
                                               column(6
                                                      
                                               ),
                                               column(3,
                                                      img(class="img-polaroid",
                                                          src=paste0("http://upload.wikimedia.org/",
                                                                     "wikipedia/commons/9/92/",
                                                                     "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                                      tags$small(
                                                        "Source: Photographed at the Bay State Antique ",
                                                        "Automobile Club's July 10, 2005 show at the ",
                                                        "Endicott Estate in Dedham, MA by ",
                                                        a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                                          "User:Sfoskett")
                                                      )
                                               )
                                             )
                                    )
                         )
))
shinyApp(ui = ui, server = server)