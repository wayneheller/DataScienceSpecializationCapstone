################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# Server components of shiny application found at:                             #
#   https://rougeone.shinyapps.io/nextword/                                    #                                                                  #
#                                                                              #
################################################################################

library(shiny)
library(data.table)

source('queryModel.R')
model.filename <- 'ngram_model_22.rds'
model.documentation.filename <- 'model_documenation_copy.csv'

df <- read.csv(model.documentation.filename, stringsAsFactors = FALSE)
df <- df[df$model.filename == model.filename, ]


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
        output$lblmodelinfo <- renderText(HTML("<b>Model Info:</b>"))
        output$labelngramcount <- renderText(HTML("Total Ngrams:&nbsp"))
        output$ngramcount <- renderText(format(df$model.ngramcount.total, big.mark = ","))
        output$labelquadgramcount <- renderText(HTML("Quadgrams:&nbsp"))
        output$quadgramcount <- renderText(format(df$model.ngramcount.quadgrams, big.mark = ","))
        output$labeltrigramcount <- renderText(HTML("Trigrams:&nbsp"))
        output$trigramcount <- renderText(format(df$model.ngramcount.trigrams, big.mark = ","))
        output$labelbigramcount <- renderText(HTML("Bigrams:&nbsp"))
        output$bigramcount <- renderText(format(df$model.ngramcount.bigrams, big.mark = ","))
        output$labelunigramcount <- renderText(HTML("Unigrams:&nbsp"))
        output$unigramcount <- renderText(format(df$model.ngramcount.unigrams, big.mark = ","))
        output$labelfilesize <- renderText(HTML("Model Size (MB):&nbsp"))
        output$filesize <- renderText(format(df$model.filesize, big.mark = ","))
        output$labelsmoothing <- renderText(HTML("Smoothing:&nbsp"))
        output$smoothing <- renderText(unlist(df$model.smoothing))
        output$labelspecialtokens <- renderText(HTML("<b>Special Tokens:</b>"))
        output$labelunktoken <- renderText("unk = unknown word")
        output$labelstoken <- renderText("s = start of sentence")
        print(df$model.smoothing)
        # Wait for change in airline selection input to set flight numbers
        observeEvent(input$type_phrase, {
                last.char <- substr(input$type_phrase, nchar(input$type_phrase),nchar(input$type_phrase))
                #output$topmatch <- renderText('None.')
                if (last.char == ' ' | nchar(input$type_phrase) == 0) {
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
