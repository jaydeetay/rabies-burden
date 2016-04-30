#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  print("Input changed")
   
  output$status<-renderText({paste("Running run number ", input$start)})
  
  # Do stuff
  print("Sourcing model")
  source('burden_model.R')
  
  output$status<-renderText({"Done"})
  
  
  print("Done")
})
