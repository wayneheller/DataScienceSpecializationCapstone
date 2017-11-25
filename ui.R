#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        #h4("Instructions:"),
        #h5("Be Patient While Data Loads."),
        #textOutput("loadmsg"),
        #p("You should see a progress message in the lower right corner.  If not, you may need to reload this page."),
  
        uiOutput("phrase")

    ),
    # Show a plot of the generated distribution
    mainPanel(

            dataTableOutput('data')
                        
        
))))
