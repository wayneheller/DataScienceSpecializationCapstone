#library(ngram) # 11/1/17 Revised to use Dfm and not Corpus text
library(dplyr)
library(data.table)

# Create a character vector for the corpus content
#str <- c(lapply(myCorpus, "[", 1))
#str <- paste( unlist(str), collapse=' ')

# returns a dataframe of ngram conditional probabilities
# input is the corpus text in its entiretly and the length of the ngram
# e.g. ngramLength = 2 is a bigram
# Much of the complexity in this code has to do with handing variable column
# names in dplyr
# NOTE: A how to work with dplyr and variable names is here:
# https://datascience.blog.wzb.eu/2016/09/27/dynamic-columnvariable-names-with-dplyr-using-standard-evaluation-functions/
calcNgramProb <- function(myDfm, ngramLength = 2) {
        # create ngram table
        #ng <- ngram(str, n=ngramLength)   
        # create frequency table 
        #df <- get.phrasetable(ng)
        
        # Transpose Dfm into a dataframe of features and frequencies
        df <- as.data.frame(t(myDfm))  
        
        names(df) <-  "freq"
        
        # separate each term in the ngram into a separate column
        for (i in 1:ngramLength) {
                #df[[LETTERS[i]]] <- vapply(strsplit(df[,1]," "), `[`, i, FUN.VALUE=character(1))
                df[[LETTERS[i]]] <- vapply(strsplit(rownames(df)," "), `[`, i, FUN.VALUE=character(1))
        }
        
        # create strings for the various dplyr actions
        # name of the condictional probability column
        #NgramProb.colName <- paste0("P", LETTERS[ngramLength], "given", paste0(LETTERS[1:ngramLength -1], collapse = ''))
        #print(NgramProb.colName)
        # formula to calculate the additional probability column
        #dots.mutate <- paste0(NgramProb.colName, " = freq/sum(freq)" )
        #print(dots.mutate)
        # string to sort descending on the conditional probability column
        #NgramProb.colName.desc <- paste0("desc(", NgramProb.colName, ")")
        #print(NgramProb.colName.desc)
        
        if (ngramLength == 1) {
                df <- df %>% mutate(prefix = NA)
        }
        else {
                df <- df %>% mutate(prefix = do.call(paste, .[LETTERS[1:ngramLength - 1]]))      
        }
        

        # group rows by the condition, add conditional probability column, then sort
        #df <- df %>% group_by_(.dots = LETTERS[1:ngramLength - 1]) %>% mutate_(.dots = setNames(dots.mutate, NgramProb.colName)) %>% arrange_(.dots = c(LETTERS[1:ngramLength - 1], NgramProb.colName.desc))
        #df <- df %>% group_by(prefix) %>% mutate_(.dots = setNames(dots.mutate, "probability")) 
        df <- df %>% group_by(prefix) %>% mutate(probability = freq/sum(freq), ngramlength=ngramLength) %>% 
                arrange(prefix, desc(probability)) %>% rename_(.dots = setNames(LETTERS[ngramLength], "nextword")) %>%
                select(prefix, nextword, probability, ngramlength)
        View(df)
        dt <- as.data.table(df)
        View(dt)
        
        
        return(dt)
}

# Assumes dt_model has been build
# Queries model for a phrase
# returns data.table of top n next work predictions
queryModelNextWord <- function(phrase, topN=3, nextWord = NULL) {
        # get the number of words in the phrase
        ntoks <- sapply(gregexpr("[A-z]\\W+", phrase), length) + 1L
        
        # tokeninze the phrase
        toks <- tokens(phrase)
        # get last token in order to filter down the ngram list
        toks.last <- tail(toks[[1]],1)

        
        # construct ngrams from the tokens 
        ngrams <- tokens_ngrams(toks, n = 1:ntoks, concatenator = " ")
        
        # filter the ngram list to those tokens ending in the last word of the phrase
        toks.pattern <- paste0(toks.last, "$")
        ngrams <- tokens_select(ngrams, pattern = toks.pattern, valuetype="regex")
        
        if (is.null(nextWord)) {
                # subset the data and return the topN from reach ngram type
                return(dt_model[prefix %in% ngrams, head(.SD, topN), by=.(ngramlength, prefix)])              
        }
        else {
                # subset the data and return the topN from reach ngram type
                return(dt_model[nextword %in% nextWord & prefix %in% unlist(ngrams), head(.SD, topN), by=.(ngramlength, prefix)])
        }

        
        
}




