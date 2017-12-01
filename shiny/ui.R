################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# UI components of shiny application found at:                                 #
#   https://rougeone.shinyapps.io/nextword/                                    #                                                                  #
#                                                                              #
################################################################################

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
  
        uiOutput("phrase"),
        textOutput("instructions"),
        p(HTML("&nbsp")),
        htmlOutput("lblmodelinfo"),
        htmlOutput("labelngramcount", inline = TRUE),
        textOutput("ngramcount", inline = TRUE),
        HTML("</br>"),
        htmlOutput("labelquadgramcount", inline = TRUE),
        textOutput("quadgramcount", inline = TRUE),
        HTML("</br>"),
        htmlOutput("labeltrigramcount", inline = TRUE),
        textOutput("trigramcount", inline = TRUE),
        HTML("</br>"),
        htmlOutput("labelbigramcount",inline = TRUE),
        textOutput("bigramcount", inline = TRUE),
        HTML("</br>"),
        htmlOutput("labelunigramcount", inline = TRUE),
        textOutput("unigramcount", inline = TRUE),
        HTML("</br>"),
        htmlOutput("labelfilesize", inline = TRUE),
        textOutput("filesize", inline = TRUE),
        HTML("</br>"),
        htmlOutput("labelsmoothing", inline = TRUE),
        htmlOutput("smoothing", inline = TRUE),
        HTML("<br>"),
        HTML("<br>"),
        htmlOutput("labelspecialtokens", inline = FALSE),
        htmlOutput("labelunktoken", inline = FALSE),
        htmlOutput("labelstoken", inline = FALSE)
        

    ),
    # Show a plot of the generated distribution
    mainPanel( 
            htmlOutput("labeltopmatch", inline = TRUE),
            textOutput("topmatch", inline = TRUE), 
            p(HTML("&nbsp")),
            htmlOutput("labelpredictions", inline = FALSE),
            dataTableOutput('data')
              ))))
