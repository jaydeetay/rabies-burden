#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source('load_countries.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  print("Input changed")
  output$countrySelector<-renderUI({selectInput("countryChoice", "Country", rownames(country_data))})
  output$country<-renderText({
    as.character(country_data[input$countryChoice, "Country"])})
  
  output$continent<-renderText({
    as.character(country_data[input$countryChoice, "Continent"])})
  
  output$cluster<-renderText({
    as.character(country_data[input$countryChoice, "Cluster"])})
  
  output$population<-renderText({
    as.character(country_data[input$countryChoice, "Human_population_2010"])})
  

  output$status<-renderText({paste("Running run number ", input$start)})
  
  # Do stuff
  #print("Sourcing model")
  #source('burden_model.R')
  
  output$status<-renderText({"Not started"})
  
  
  print("Done")
})
