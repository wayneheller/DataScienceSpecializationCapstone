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
        df <- df %>% group_by(prefix) %>% mutate(probability = freq/sum(freq), nwordtypes = n(), ngramlength=ngramLength) %>% 
                arrange(prefix, desc(probability)) %>% rename_(.dots = setNames(LETTERS[ngramLength], "nextword")) %>%
                select(prefix, nextword, probability, ngramlength, freq, nwordtypes)
        View(df)
        dt <- as.data.table(df)
        View(dt)
        
        
        return(dt)
}

# Assumes dt_model has been build
# Queries model for a phrase
# returns data.table of top n next work predictions
queryModelNextWord <- function(phrase, topN=3, nextWord = NULL, verbose = TRUE) {
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
        toks.pattern <- paste0(toks.last, "$")
        # print(toks.pattern)
        ngrams <- tokens_select(ngrams, pattern = toks.pattern, valuetype="regex")
        # print(ngrams)
        
        if (is.null(nextWord)) {
                # subset the data and return the topN from each ngram type
                if (verbose == TRUE){
                        return(dt_model[prefix %in% ngrams, head(.SD, topN), by=.(ngramlength, prefix)])   
                }
                else { # just return the next word
                        dt <- dt_model[prefix %in% ngrams, head(.SD, topN), by=.(ngramlength, prefix)]
                        
                        return(unique(dt[, .(nextword)]))
                }
        }
        else {
                # subset the data and return the topN from reach ngram type for a list of predicted words
                # used this version to evaluate quiz questions where the next word options were provided
                # in the context of a multiple choice question.
                return(dt_model[nextword %in% nextWord & prefix %in% unlist(ngrams), head(.SD, topN), by=.(ngramlength, prefix)])
        }

        
        
}

# Apply Modified Kneser-Ney smoothing to dt_model object
# Assumes model is built and loaded into the environment
applyKneserNeySmoothing <- function() {
        
        # Get discounting factors - will build out true calculations later
        # for now using these made up constants
        D1 <- .4
        #D.2 == .5
        #D.3.plus == .75
        
        # Step 1: Calculate lowest order unigram probabilities
        # Pkn(w) = number of bigrams w completes / total number of bigrams
        
        # total number of bigrams in the model
        num_bigrams <- nrow(dt_model[ngramlength == 2])
        # conditional probability
        dt_model_unigram <- dt_model[ngramlength == 2, .(completes = .N/num_bigrams, ngramlength = 1) , by=.(nextword)]
        # update model
        dt_model[dt_model_unigram, Pkn:=i.completes, on = c('ngramlength', 'nextword')]
        
        # Step 2: Calucate bigram probabilities
        # Pkn(wi|wi-1) = max()....
        #setkey(df1, lsr, ppr)
        #setkey(df2, li, pro)
        #df1[df2, alpha := i.alpha]
        dt_model_unigram <- dt_model[ngramlength == 1, .(nextword, freq, ngramlength = ngramlength + 1)]
        print(head(dt_model_unigram))
        setkey(dt_model_unigram, nextword, ngramlength)
        setkey(dt_model, prefix, ngramlength)
        dt_model[dt_model_unigram, Pkn:=(freq-D1)/i.freq]
        print(head(dt_model, 15))
        
        
}



