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
if (!require("DT")) install.packages('DT')
library("DT")

print("ShinyServer: Server start up")
country_data <- read.csv("vcountry2.csv", row.names = 2)
# This data is calculated by burden_1.R - we will assume this a given for
# now.
pPEPcountry <- read.csv("pPEPcountry.csv", row.names = 2)

print("ShinyServer: sourcing burden model")
source('burden_model.R')
print("ShinyServer: Burden model sourced")
source('helper.R')

input_column_whitelist = c("Country", "HDI","Human_population_2010", "BI", "BI_L", "BI_U", "Coverage", "Cov_L", "Cov_U", "mRP", "lRP", "uRP", "PP", "PPl","PPu")
output_column_whitelist = c("deaths", "exp", "PEP", "prev_death", "YLL")

shinyServer(function(input, output) {
  print("ShinyServer: New User")
  
  output$countrySelector<-renderUI({
    print("ShinyServer: Populating country selector")
    selectInput("countryChoice",
                "Country",
                setNames(rownames(country_data), country_data[["Country"]]))})

  output$country<-renderText({
    print(paste("ShinyServer: Country changed to ", input$countryChoice))
    as.character(country_data[input$countryChoice, "Country"])})
  
  output$continent<-renderText({
    as.character(country_data[input$countryChoice, "Continent"])})
  
  output$cluster<-renderText({
    as.character(country_data[input$countryChoice, "Cluster"])})
  
  output$population<-renderText({
    as.character(country_data[input$countryChoice, "Human_population_2010"])})
  
  output$HDI<-renderText({
    as.character(pPEPcountry[input$countryChoice, "HDI"])})
  
  output$pPEP_table<-DT::renderDataTable(
    {row_slice_or_empty(pPEPcountry, input$countryChoice)}[input_column_whitelist],
     options = list(
       searching = FALSE,
       paging=FALSE,
       info=FALSE))
  
  ## Editable table stuff
  pPep = reactiveValues(data = NULL)
  
  observeEvent(input$countryChoice, {
    print("ShinyServer: resetting pPep data from server")
    pPep$data <- row_slice_or_empty(pPEPcountry, input$countryChoice)
  })
  
  observeEvent(input$pPEP_table_editable, {
    print("ShinyServer: setting user edited pPep data")
    new_data <- hot_to_r(input$pPEP_table_editable)
    updated <- merge_data(pPep$data, new_data, "Country")
    pPep$data <- updated
  })
  
  
  output$pPEP_table_editable <- renderRHandsontable({
    input$countryChoice # Force a reaction
    DF = row_slice_or_empty(pPEPcountry, input$countryChoice) # Resets any user values
    if (!is.null(DF)) {
      rhandsontable(DF[input_column_whitelist], useTypes = TRUE, stretchH = "all") %>%
        hot_cols(format = "0.0") %>%  # Default col format
        hot_col("HDI", format = "0.0000")  # Overridden per column
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
  
  output$burden_table <- DT::renderDataTable(
    withProgress(message = "Calculating", detail = "Please wait...", {
      print("ShinyServer: Maybe calculation")
      resultData$data[output_column_whitelist]
    }),
    caption = "Output",
    options = list(
      searching=FALSE,
      paging=FALSE,
      info=FALSE))
  
  print("ShinyServer: End server call")
})
