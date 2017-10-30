### 10/30/17 switching to Quanteda package
### Also installed readtext companion package

library(quanteda)
library(readtext)

samplefilename <- "sampledata_samplesize_0.05.txt"

# loads the sample file and creates a corpus from it
loadCorpus <- function() {
        mySampleFile <- readtext(file.path(sampledir, samplefilename))
        myCorpus <- corpus(mySampleFile)
        metadoc(myCorpus, "SampleSize") <- getSampleSize(sampledir)
        return(myCorpus)
        
}

# preprocesses and returns a document feature matrix
getDfm <- function(myCorpus, ngram=1) {
        
        # Remove Profane words from a list originally published by Google
        BannedWordFileName <- "full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
        pathToBannedWordList <- file.path(metadatadir, BannedWordFileName)
        BandWords <- read.csv(pathToBannedWordList, sep="\t", strip.white = TRUE)
        
        # This file contains non-breaking whitespace that needs to be removed
        chrBandWords <- sapply(BandWords[, 1], removenbsp)
        
        wordstoremove <- c(stopwords("english"), chrBandWords)
        
        if (ngram == 1) {
                # to lower case is TRUE by default
                myDfm <- dfm(myCorpus, remove = wordstoremove, remove_numbers = TRUE, remove_separators = TRUE, remove_punct = TRUE)
        }
        else {
                myTokens <- tokens(myCorpus, remove_numbers = TRUE, remove_separators = TRUE, remove_punct = TRUE)
                myTokens <- tokens_remove(myTokens, wordstoremove, valuetype='fixed')
                myDfm <- dfm(myTokens, ngrams = ngram, concatenator =' ')
        }
        
        return(myDfm)
}