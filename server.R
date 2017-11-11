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
    withProgress(message = "Loading phrase dictionary...Please Wait",  {
        # Read summarized ngram information into object dt_model
        load("ngram_model.rds")
    } )
        

        output$phrase <- renderUI({textInput("type_phrase", "Enter a phrase:", width='100%', placeholder = 'Start typing...')})
  
        # Wait for change in airline selection input to set flight numbers
        observeEvent(input$type_phrase, {
                last.char <- substr(input$type_phrase, nchar(input$type_phrase),nchar(input$type_phrase))
                if (last.char == ' ') {
                        i = runif(n=1, min=1, max = 5)
                        output$data <- renderDataTable({
                                queryModelNextWord(input$type_phrase, topN = 3)
                                
                                 })
                }
        }, ignoreInit = TRUE)
  
       
  
  

  
})
