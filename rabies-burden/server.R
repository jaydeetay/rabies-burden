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
  print("New User")
  output$countrySelector<-renderUI({selectInput("countryChoice", "Country", rownames(country_data))})
  output$country<-renderText({
    print("Country changed")
    as.character(country_data[input$countryChoice, "Country"])})
  
  output$continent<-renderText({
    as.character(country_data[input$countryChoice, "Continent"])})
  
  output$cluster<-renderText({
    as.character(country_data[input$countryChoice, "Cluster"])})
  
  output$population<-renderText({
    as.character(country_data[input$countryChoice, "Human_population_2010"])})
  

  # Running the rabies model is expensive - we don't want to redo it automatically
  # on every input change.  Instead only run it when the button is pressed.
  doCalculation <- eventReactive(input$start,{
    print("Sourcing burden_model")
    source('burden_model.R')
  })
  
  output$dummy <- renderText(withProgress(message="Calculating", {
    print("Do calculation")
    doCalculation()
    ""
  }))
  
  
  
  print("End server call")
})
