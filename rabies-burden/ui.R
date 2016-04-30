#
# Shiny User interface to the rabies burden model.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# User interface
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Burden of Rabies"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       h1("Controls will go here"),
       actionButton("start", "Start Calculation")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h2("Results will go here"),
       textOutput("status")
    )
  )
))
