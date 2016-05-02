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
country_data = read.csv("vcountry2.csv", row.names = 2)
# This data is calculated by burden_1.R - we will assume this a given for
# now.
pPEPcountry = read.csv("pPEPcountry.csv", row.names = 2)
print("ShinyServer: sourcing burden model")
source('burden_model.R')
print("ShinyServer: Burden model sourced")
source('helper.R')

# Define server logic required to draw a histogram
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
  
  output$pPEP_table<-renderTable({t(row_slice_or_empty(pPEPcountry, input$countryChoice))})
  
  ## Editable table stuff
  values = reactiveValues()
  
  data = eventReactive(input$countryChoice, {
    if (!is.null(input$pPEP_table_editable)) {
      print("Getting data from UI")
      DF = hot_to_r(input$pPEP_table_editable)
    } else {
      print("Populating data from server...")
     # if (is.null(values[["DF"]])) {
        print("...dataframe")
        print(input$countryChoice)
        DF = t(row_slice_or_empty(pPEPcountry, input$countryChoice))
     # } else {
     #   print("...cache")
     #   DF = values[["DF"]]
     # }
    }
    
    
    values[["DF"]] = DF
    #print(DF)
    DF
  })
  
  output$pPEP_table_editable <- renderRHandsontable({
    input$countryChoice #Force a reaction
    DF = data()
    if (!is.null(DF)) {
      rhandsontable(DF, useTypes = TRUE, stretchH = "all")
    }
  })
  # End editable table stuff

  # Running the rabies model is expensive - we don't want to redo it automatically
  # on every input change.  Instead only run it when the button is pressed.
  doCalculation <- eventReactive(input$start,{
    print("ShinyServer: Sourcing burden_model")
    return(calculate_burden(countryCode = input$countryChoice, provideProgress = TRUE))
  })
  
  output$burden_table <- renderTable(withProgress(message = "Calculating", detail = "Please wait...", {
    print("ShinyServer: Maybe calculation")
    doCalculation()
  }))
  
  print("ShinyServer: End server call")
})
