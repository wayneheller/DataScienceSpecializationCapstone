################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# Used by buildModel script to calculate and smooth probabilities              #
#                                                                              # 
#                                                                              #
#                                                                              #
################################################################################
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
# This function has been modified several times over the course of thie project
# and it might make sense to rethink the approach and rewrite.
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
        #View(df)
        dt <- as.data.table(df)
        #View(dt)
        
        
        return(dt)
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
        #dt_model_unigram <- dt_model_unigram[ , .(completes = ifelse(is.na(completes), 0, completes), ngramlength, nextword)]
        #print(dt_model_unigram[nextword=='0.5mm'])
        # update model
        dt_model[dt_model_unigram, Pkn:=i.completes, on = c('ngramlength', 'nextword')]
        dt_model[ngramlength == 1 & is.na(Pkn), Pkn:=0]
        
        # Step 2: Calucate bigram probabilities
        # Pkn(wi|wi-1) = max()....
        # create a data table of unigrams to join with model table
        # for convenience rename the nextword column to prefix because we will be
        # joining on prefix. increment ngramlength for the same reason--to make the join easier
        dt_model_unigram <- dt_model[ngramlength == 1 , .(prefix = nextword, freq, ngramlength = ngramlength + 1, nwordtypes, Pkn)]
        # perform the join and then calculate the Kneser-Nye probability
        dt_model[dt_model_unigram, Pkn:=calcKneserNye(freq, D1, i.freq, nwordtypes, i.Pkn), on = c('ngramlength', 'prefix') ]
        dt_model_unigram <- NULL # Clean up
        
        # Step 3: Trigrams
        dt_model_bigram <- dt_model[ngramlength == 2 , .(prefix = paste(prefix, nextword), freq, ngramlength = ngramlength + 1, nwordtypes, Pkn)]
        #print(head(dt_model_bigram[is.na(Pkn)]))
        dt_model[dt_model_bigram, Pkn:=calcKneserNye(freq, D1, i.freq, nwordtypes, i.Pkn), on = c('ngramlength', 'prefix') ]
        dt_model_bigram <- NULL # Clean up
        
        # Step 4: Quadgrams
        dt_model_trigram <- dt_model[ngramlength == 3 , .(prefix = paste(prefix, nextword), freq, ngramlength = ngramlength + 1, nwordtypes, Pkn)]
        #print(head(dt_model_bigram[is.na(Pkn)]))
        dt_model[dt_model_trigram, Pkn:=calcKneserNye(freq, D1, i.freq, nwordtypes, i.Pkn), on = c('ngramlength', 'prefix') ]
        dt_model_trigram <- NULL # Clean up
        

        
        
}

calcKneserNye <- function(freq, D, i.freq, nwordtypes, i.Pkn) {
        #print(paste(freq, D, i.freq, nwordtypes, i.Pkn))  
        # lapply(num, function(z) {z[3]})
        D <- unlist(lapply(freq, function(z) {getKneserNyeDiscount(z)}))
        # print(D)
        max_discount <- lapply((freq - D) / i.freq, function(z) {ifelse(is.na(z), 0, max(z,0))})
        continuation_prob <- lapply(D/i.freq*nwordtypes*i.Pkn, function(z) {ifelse(is.na(z), 0, max(z,0))})

        return(list(unlist(max_discount) + unlist(continuation_prob)))
}

getKneserNyeDiscount <- function(freq) {
        if (is.na(freq)) {
                return(0)
        }
        if (freq >= 3) {
                return(D.3)
                
                }
        if (freq == 2) {
                return(D.2)
                }
        
        return(D.1)
}


