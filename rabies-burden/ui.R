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
       h1("Select:"),
       uiOutput("countrySelector"),  # Set dynamically
       actionButton("start", "Start Calculation")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h2("Output:"),
       h3("Country Data"),
       "Country: ", textOutput("country", inline=TRUE),br(),
       "Continent: ", textOutput("continent", inline=TRUE),br(),
       "Cluster: ", textOutput("cluster", inline=TRUE),br(),
       "Population (2010): ", textOutput("population", inline=TRUE),
       h3("Calculation status"),
       textOutput("status")
    )
  )
))
