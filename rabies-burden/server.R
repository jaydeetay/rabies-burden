#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

print("ShinyServer: Server start up")
country_data = read.csv("vcountry2.csv", row.names = 2)
# This data is calculated by burden_1.R - we will assume this a given for
# now.
pPEPcountry = read.csv("pPEPcountry.csv", row.names = 2)
print("ShinyServer: sourcing burden model")
source('burden_model.R')
print("ShinyServer: Burden model sourced")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  print("ShinyServer: New User")
  output$countrySelector<-renderUI({selectInput("countryChoice", "Country", rownames(country_data))})
  output$country<-renderText({
    print("ShinyServer: Country changed")
    as.character(country_data[input$countryChoice, "Country"])})
  
  output$continent<-renderText({
    as.character(country_data[input$countryChoice, "Continent"])})
  
  output$cluster<-renderText({
    as.character(country_data[input$countryChoice, "Cluster"])})
  
  output$population<-renderText({
    as.character(country_data[input$countryChoice, "Human_population_2010"])})
  
  output$pPEP_table<-renderTable({t(pPEPcountry[input$countryChoice,])})
  

  # Running the rabies model is expensive - we don't want to redo it automatically
  # on every input change.  Instead only run it when the button is pressed.
  doCalculation <- eventReactive(input$start,{
    print("ShinyServer: Sourcing burden_model")
    return(calculate_burden(countryCode = input$countryChoice, provideProgress = TRUE))
  })
  
  output$burden_table <- renderTable(withProgress(message = "Calculating", detail = "Please wait...", {
    print("ShinyServer: Do calculation")
    doCalculation()
  }))
  
  print("ShinyServer: End server call")
})
