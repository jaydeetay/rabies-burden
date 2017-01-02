#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)

print("ShinyServer: Server start up")
country_data <- read.csv("vcountry2.csv", row.names = 2)
# This data is calculated by burden_1.R - we will assume this a given for
# now.
pPEPcountry <- read.csv("pPEPcountry.csv", row.names = 2)
# Strip non-numeric data from pPEPcountry as it buggers up the editable table types.
# Actually - don't do this just yet - we'll show the data horizontally for now pPEPcountry <- pPEPcountry[,!(colnames(pPEPcountry) %in% c("NUM","Country","Cluster","Continent","Code","CODE_GDP", "WHO"))]

print("ShinyServer: sourcing burden model")
source('burden_model.R')
print("ShinyServer: Burden model sourced")
source('helper.R')

shinyServer(function(input, output) {
  print("ShinyServer: New User")
  
  output$countrySelector<-renderUI({
    print("ShinyServer: Populating country selector")
    selectInput("countryChoice", "Country", rownames(country_data))})
  output$country<-renderText({
    print(paste("ShinyServer: Country changed to ", input$countryChoice))
    as.character(country_data[input$countryChoice, "Country"])})
  
  output$continent<-renderText({
    as.character(country_data[input$countryChoice, "Continent"])})
  
  output$cluster<-renderText({
    as.character(country_data[input$countryChoice, "Cluster"])})
  
  output$population<-renderText({
    as.character(country_data[input$countryChoice, "Human_population_2010"])})
  
  output$pPEP_table<-renderTable({row_slice_or_empty(pPEPcountry, input$countryChoice)})
  
  ## Editable table stuff
  pPep = reactiveValues(data = NULL)
  
  observeEvent(input$countryChoice, {
    print("ShinyServer: resetting pPep data from server")
    pPep$data <- row_slice_or_empty(pPEPcountry, input$countryChoice)
  })
  
  observeEvent(input$pPEP_table_editable, {
    print("ShinyServer: setting user edited pPep data")
    pPep$data <- hot_to_r(input$pPEP_table_editable)
  })
  
  
  output$pPEP_table_editable <- renderRHandsontable({
    input$countryChoice # Force a reaction
    DF = row_slice_or_empty(pPEPcountry, input$countryChoice) # Resets any user values
    if (!is.null(DF)) {
      rhandsontable(DF, useTypes = TRUE, stretchH = "all")
    }
  })
  # End editable table stuff
  
  resultData <- reactiveValues(data = NULL)
  
  # Running the rabies model is expensive - we don't want to redo it automatically
  # on every input change.  Instead only run it when the button is pressed.
  observeEvent(input$start, {
    print("ShinyServer: running the burden model")
    resultData$data <- calculate_burden(countryCode = input$countryChoice, provideProgress = TRUE,
                                        customPPEPdata = pPep$data)
  })
  
  observeEvent(input$countryChoice, {
    print("ShinyServer: resetting the output due to changed country")
    resultData$data <- NULL
  })
  
  observeEvent(input$pPEP_table_editable, {
    print("ShinyServer: resetting the output due to change params")
    resultData$data <- NULL
  })
  
  output$burden_table <- renderTable(withProgress(message = "Calculating", detail = "Please wait...", {
    print("ShinyServer: Maybe calculation")
    resultData$data
  }))
  
  print("ShinyServer: End server call")
})
