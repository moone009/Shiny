library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("MPG Filter"),
    
    
    navlistPanel(
      "Header A",
      tabPanel("Component 1"),
      tabPanel("Component 2"),
      "Header B",
      tabPanel("Component 3"),
      tabPanel("Component 4"),
      "-----",
      tabPanel("Component 5")
    ),
  
    # Create a new Row in the UI for selectInputs
    fluidRow(
      div(class="span1", 
          selectInput("man", 
                      "Manufacturer:", 
                      c("All", 
                        unique(as.character(mpg$manufacturer))))
      ),
      div(class="span1", 
          selectInput("trans", 
                      "Transmission:", 
                      c("All", 
                        unique(as.character(mpg$trans))))
      ),
      div(class="span1", 
          selectInput("cyl", 
                      "Cylinders:", 
                      c("All", 
                        unique(as.character(mpg$cyl))))
      ),      
      div(class="span1", 
          selectInput("Carmodel", 
                      "Car Model:", 
                      c("All", 
                        unique(as.character(mpg$model))))
      )    
    ),
    # Create a new row for the table.
    fluidRow(
      tableOutput(outputId="table")
    )    
  )  
)