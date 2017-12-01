################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# Loads the sample file into a corpus object and managers metadata for it      #
# Creates a document feature matrix                                            # 
# Includes pruning and cleaning functions                                      #                                                                     #
#                                                                              #
################################################################################

### 10/30/17 switching to Quanteda package
### Also installed readtext companion package

## ---- preprocessingqanteda

library(quanteda)
library(readtext)
library(dplyr)
library(tidyverse)

# loads the sample file and creates a corpus from it

loadCorpus <- function(samplefiletype = 'training', sample.size = 0.05) {
        samplefilename <- switch(samplefiletype,
                                 training = paste0("sampledata_samplesize_", as.character(sample.size), ".txt"),
                                 testing =  paste0("testdata_samplesize_", as.character(sample.size), ".txt"),
                                 development =  paste0("devdata_samplesize_", as.character(sample.size), ".txt")
        )
        
        samplefilename <- switch(samplefiletype,
                                 training = file.path(sampledir, samplefilename),
                                 testing =  file.path(testingdatadir, samplefilename),
                                 development =  file.path(devdatadir, samplefilename)
        )
        # print(samplefilename)
        
        mySampleFile <- readtext(samplefilename)
        
        myCorpus <- corpus(mySampleFile)
        metadoc(myCorpus, "SampleSize") <- getSampleSize(sampledir)
        
        return(myCorpus)
        
}

# preprocesses and returns a document feature matrix
# if unkWords is a character vector of length > 1, then will convert those tokens to 'unk'
getDfm <- function(myCorpus, ngram=1, unkWords="") {
        
        # Remove Profane words from a list originally published by Google
        BannedWordFileName <- "full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
        pathToBannedWordList <- file.path(metadatadir, BannedWordFileName)
        BandWords <- read.csv(pathToBannedWordList, sep="\t", strip.white = TRUE)
        
        # This file contains non-breaking whitespace that needs to be removed
        wordstoremove <- sapply(BandWords[, 1], removenbsp)
        
        # leave in the stop words
        #wordstoremove <- c(stopwords("english"), wordstoremove)
        
        myTokens <- tokens(myCorpus, remove_numbers = TRUE, remove_separators = TRUE, remove_punct = TRUE, remove_twitter = TRUE)
        myTokens <- tokens_remove(myTokens, wordstoremove, valuetype='fixed')
        
        # replace low frequency words with "unk", in the next version of quanteda there will be a token_replace function
        if (length(unkWords) > 1){
                dict_list <- list( "unk" = unkWords)
                dict <- quanteda::dictionary(dict_list)
                myTokens <- tokens_lookup(myTokens, dictionary=dict, exclusive = FALSE, capkeys = FALSE)      
        }

        myDfm <- dfm(myTokens, ngrams = ngram, concatenator =' ')
   
        return(myDfm)
}

# returns the Dfm filter down to just terms with frequency density greater than threshold
pruneDfm <- function(myDfm, densityThreshold = 0, frequencyThreshold = 0)  {
        if (densityThreshold > 0) {
                # Transpose and convert to dataframe
                df <- as.data.frame(t(myDfm))
                df <- rownames_to_column(df)
                names(df) <- c("term", "freq")
                
                df_sum <- df %>% group_by(freq) %>% summarise(group_total = sum(freq)) %>% arrange(desc(freq)) %>%
                         mutate(cumsum_group_total = cumsum(group_total))
                
                # Find total cummulative sum
                cummulative_sum <- df_sum[nrow(df_sum), 3][[1]]
        
                # create density column
                df_sum <- mutate(df_sum, density_group_total = cumsum_group_total / cummulative_sum)
          
                View(df_sum)
        
        
                # find the freq value that is just above the threadhold value
                minFreq <- df_sum[max(which(df_sum$density_group_total <= densityThreshold)), ]$freq         
        }
        
        else {
                minFreq <- frequencyThreshold
        }

        print(minFreq)
        # trim the dfm to the minimum frequency count
        return(dfm_trim(myDfm, min_count = minFreq))
        
}

# Reterns list of low frequency words based on threshold (lowfreq) 
identifyLowFreqTerms <- function(myCorpus, lowfreq=1){
        # Get Unigram counts
        myDfm <- getDfm(myCorpus, ngram=1)
        # Transpose and convert to dataframe
        df <- as.data.frame(t(myDfm))
        df <- rownames_to_column(df)
        names(df) <- c("term", "freq")
        # Build a vector of low frequency terms
        df <- filter(df, freq <= lowfreq)
        lowfreqterms <- df$term
        return(lowfreqterms)
}

getSampleSize <- function(sampleDir) {
        if (file.exists(file.path(sampleDir, "sampledata_samplesize_0.01.txt"))) {
                sample.size <- "1%"
        }
        if (file.exists(file.path(sampleDir, "sampledata_samplesize_0.1.txt"))) {
                sample.size <- "10%"
        }
        if (file.exists(file.path(sampleDir, "sampledata_samplesize_0.05.txt"))) {
                sample.size <- "5%"
        }
        else sample.size <- "Unknown"
        
        return(sample.size)
}

# This function removes non-breaking spaces ascii character 160
# The trimws() function does not handle these
# Used by cleanCorpus() in the preparation of the banned words file
removenbsp <- function(x) {
        return(gsub(chr(160), "", x, fixed = TRUE))
}

# helper function to convert an ascii code to its character
#used by cleanCorpus()
chr <- function(n) { rawToChar(as.raw(n)) }
## ----end-preprocessingqanteda