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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #output$loadmsg <- renderText({"Be Patient While Data Loads..."})
    withProgress(message = "Loading phrase dictionary...Please Wait",  {
        # Read summarized ngram information into object dt_model
        load("ngram_model_shiny.rds")
    } )
        

        output$phrase <- renderUI({textInput("type_phrase", "Enter a phrase:", width='100%', placeholder = 'Start typing...')})
  
        # Wait for change in airline selection input to set flight numbers
        observeEvent(input$type_phrase, {
                last.char <- substr(input$type_phrase, nchar(input$type_phrase),nchar(input$type_phrase))
        
                if (last.char == ' ') {
                        
                        output$data <- renderDataTable({
                                queryModelNextWord(input$type_phrase, topN = 3, verbose = FALSE)
                                
                                 }, options = list(paging = FALSE, searching = FALSE, info = FALSE),
                                searchDelay = 1000)
                }
                
        }, ignoreInit = TRUE)
  
       
  
  

  
})
