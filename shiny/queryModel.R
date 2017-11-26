
library(dplyr)
library(data.table)
library(quanteda)

# Assumes dt_model has been build
# Queries model for a phrase
# returns data.table of top n next work predictions
queryModelNextWord <- function(dt_model, phrase, topN=3, nextWord = NULL, verbose = TRUE) {
        # handles case where phrase is blank
        if (nchar(phrase) == 0) {phrase <- 'the'}
        # get the number of words in the phrase
        #ntoks <- sapply(gregexpr("[A-z]\\W+", phrase), length) + 1L
        
        # tokeninze the phrase
        toks <- tokens(char_tolower(phrase), what ='word')
        #print(toks)
        # get the number of words in the phrase
        ntoks <- length(unlist(toks))
        # print(ntoks)
        # get last token in order to filter down the ngram list
        toks.last <- tail(toks[[1]],1)
        # print(toks.last)
        
        
        # construct ngrams from the tokens 
        ngrams <- tokens_ngrams(toks, n = 1:ntoks, concatenator = " ")
        # print(ngrams)
        
        # filter the ngram list to those tokens ending in the last word of the phrase
        # this has an error in that if the next to last token ends in the last token it
        # will return extra tokens, e.g. 'the rain in' will return 'the rain' as an n-gram
        toks.pattern <- paste0(toks.last, "$")
        #print(toks.pattern)
        ngrams <- tokens_select(ngrams, pattern = toks.pattern, selection='keep', valuetype="regex")
        #print(ngrams)
        
        if (is.null(nextWord)) {
                # subset the data and return the topN from each ngram type
                if (verbose == TRUE){
                        #return(dt_model[prefix %in% ngrams, head(.SD, topN), by=.(ngramlength, prefix)])  
                        dt <-  dt_model[prefix %in% ngrams, head(.SD, topN), by=.(ngramlength, prefix)]
                        return(dt[order(-ngramlength, -Pkn)])
                }
                else { # just return the next word
                        dt <- dt_model[prefix %in% ngrams, head(.SD, topN), by=.(ngramlength, prefix)]
                        dt <- dt[order(-ngramlength, -Pkn)]
                        return(unique(dt[, .(nextword)]))
                }
        }
        else {
                # subset the data and return the topN from reach ngram type for a list of predicted words
                # used this version to evaluate quiz questions where the next word options were provided
                # in the context of a multiple choice question.
                #return(dt_model[nextword %in% nextWord & prefix %in% unlist(ngrams), head(.SD, topN), by=.(ngramlength, prefix)])
                dt <- dt_model[nextword %in% nextWord & prefix %in% unlist(ngrams), head(.SD, topN), by=.(ngramlength, prefix)]
                return(dt[order(-ngramlength, -Pkn)])
        }
        
        
        
}