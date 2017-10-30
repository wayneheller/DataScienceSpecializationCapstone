library(ngram)
library(dplyr)

# Create a character vector for the corpus content
str <- c(lapply(myCorpus, "[", 1))
str <- paste( unlist(str), collapse=' ')

# returns a dataframe of ngram conditional probabilities
# input is the corpus text in its entiretly and the length of the ngram
# e.g. ngramLength = 2 is a bigram
# Much of the complexity in this code has to do with handing variable column
# names in dplyr
# NOTE: A how to work with dplyr and variable names is here:
# https://datascience.blog.wzb.eu/2016/09/27/dynamic-columnvariable-names-with-dplyr-using-standard-evaluation-functions/
calcNgramProb <- function(myCorpus.text, ngramLength=2) {
        # create ngram table
        ng <- ngram(str, n=ngramLength)   
        # create frequency table 
        df <- get.phrasetable(ng)
        # separate each term in the ngram into a separate column
        for (i in 1:ngramLength) {
                df[[LETTERS[i]]] <- vapply(strsplit(df[,1]," "), `[`, i, FUN.VALUE=character(1))
        }
        
        # create strings for the various dplyr actions
        # name of the condictional probability column
        NgramProb.colName <- paste0("P", LETTERS[ngramLength], "given", paste0(LETTERS[1:ngramLength -1], collapse = ''))
        print(NgramProb.colName)
        # formula to calculate the additional probability column
        dots.mutate <- paste0(NgramProb.colName, " = freq/sum(freq)" )
        print(dots.mutate)
        # string to sort descending on the conditional probability column
        NgramProb.colName.desc <- paste0("desc(", NgramProb.colName, ")")
        print(NgramProb.colName.desc)
        
        # group rows by the condition, add conditional probability column, then sort
        df <- df %>% group_by_(.dots = LETTERS[1:ngramLength - 1]) %>% mutate_(.dots = setNames(dots.mutate, NgramProb.colName)) %>% arrange_(.dots = c(LETTERS[1:ngramLength - 1], NgramProb.colName.desc))
        
        return(df)
}



#df <- df %>% group_by(A) %>% mutate(PBA = freq/sum(freq)) %>% arrange(A, desc(PBA))
#print(head(df))
