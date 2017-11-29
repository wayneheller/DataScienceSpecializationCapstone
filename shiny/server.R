#
# 
# 
#
# 
# 
#    
#

library(shiny)
library(data.table)

source('queryModel.R')
model.filename <- 'ngram_model_21.rds'


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #output$loadmsg <- renderText({"Be Patient While Data Loads..."})
    withProgress(message = "Loading phrase dictionary...Please Wait",  {
        # Read summarized ngram information into object dt_model
        load(file = model.filename)
    } )
        

        output$phrase <- renderUI({textInput("type_phrase", "Enter a phrase:", width='100%', placeholder = 'Start typing...')})
        output$labeltopmatch <- renderText(HTML("<b>Predicted Next Word:&nbsp</b>"))
        output$labelpredictions <- renderText(HTML("<b>Extended List of Predicted Words:</b>"))
        output$instructions <- renderText("Start typing then hit space bar for next word prediction")
        
        # Wait for change in airline selection input to set flight numbers
        observeEvent(input$type_phrase, {
                last.char <- substr(input$type_phrase, nchar(input$type_phrase),nchar(input$type_phrase))
        
                if (last.char == ' ') {
                        dt <- queryModelNextWord(dt_model, input$type_phrase, topN = 3, verbose = TRUE)
                        output$topmatch <- renderText(unlist(dt[1, .(nextword)]))
                        output$data <- renderDataTable({
                                #head(dt_model)
                                #queryModelNextWord(dt_model, input$type_phrase, topN = 3, verbose = FALSE)
                                dt
                                 }, options = list(paging = FALSE, searching = FALSE, info = FALSE),
                                searchDelay = 1000)
                }
                
        }, ignoreInit = TRUE)
  
       
  
  

  
})
