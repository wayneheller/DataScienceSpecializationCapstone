# Data Science Capstone Project
# August 2017
#
#
# Read and sample text files
# Row counts were determined by opening file in a text editor

library(tm)

datadir <- file.path(getwd(), "data")
sampledir <- file.path(datadir, "sampleddata")
metadatadir <- file.path(datadir, "metadata")

chr <- function(n) { rawToChar(as.raw(n)) }

sampleText <- function (filename, totalrows, sampleSize) {  
        set.seed(12345)
        
        # generate line numbers of the file to include in the sample
        lineNumber = sample(totalrows, sampleSize * totalrows)

        sample <- list() # initialize the sample
        
        conn <- file(filename, open = "rb") # open connection to the file using rb to deal with Cntl-Z in news file

        row <- 0 # initialize row counter
        
        #read file 1 line at a time 
        while (length(oneLine <- readLines(conn, n = 1, warn = TRUE, skipNul = TRUE)) > 0) {
                
                row <- row + 1
                        
                if(row %in% lineNumber) {
                        sample <- list(sample, oneLine)   
                }
        }
        close(conn)
        return(unlist(sample))
}

createSampleData <- function(datadir) {
        sample.size <- 0.00001
        samplefilename <- "sampledata.txt"
        
        file.news <- "en_US.news.txt"
        file.news.rows <- 1010242
        
        file.blogs <- "en_US.blogs.txt"
        file.blogs.rows <- 899289
        
        file.twitter <- "en_US.twitter.txt"
        file.twitter.rows <- 2360148
        
        # Create sample for each source file
        file.blogs.sample <- sampleText(file.path(datadir, file.blogs), file.blogs.rows, sample.size)
        file.news.sample <- sampleText(file.path(datadir, file.news), file.news.rows, sample.size)
        file.twitter.sample <- sampleText(file.path(datadir, file.twitter), file.twitter.rows, sample.size)
        
        # Concatenate samples
        samples <- c(file.blogs.sample, file.news.sample, file.twitter.sample)
        
        
        # Remove existing sample file if it exists
        if(file.exists(file.path(datadir, samplefilename))) {
                file.remove(file.path(datadir, samplefilename))
        }
        
        # Create new sample file
        conn <- file(file.path(datadir, "sampleddate", samplefilename), open = "w", encoding = "UTF-8" )
        write(samples, conn)
        close(conn)
}

# open sample data as a corpus
openCorpus <- function(sampleDir) {
        #txt <- system.file(sampleDir, "txt", package = "tm")
        #print(txt)
        myCorpus <- Corpus(DirSource(sampleDir, encoding = "UTF-8"),
                           readerControl = list(language = "lat"))

        return(myCorpus)
}

# This function removes non-breaking spaces ascii character 160
# The trimws() function does not handle these
removenbsp <- function(x) {
        return(gsub(chr(160), "", x, fixed = TRUE))
}

removePattern <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x, fixed = FALSE))})

# Removes Profanity, Whitespace, and Punctuation
cleanCorpus <- function(myCorpus, metadataDir) {
        # Step 1: Remove Profane words from a list originally published by Google
        BannedWordFileName <- "full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
        pathToBannedWordList <- file.path(metadataDir, BannedWordFileName)
        BandWords <- read.csv(pathToBannedWordList, sep="\t", strip.white = TRUE)
        # This file contains non-breaking whitespace that needs to be removed
        myCorpus <- tm_map(myCorpus, removeWords, sapply(BandWords[, 1], removenbsp))
        
        # Step 2: Remove special characters of the form <U+9999>"
        # gsub("<U\\+\\d+>", "", x, fixed=FALSE)
        myCorpus <- tm_map(myCorpus, removePattern, "<U") # not working yet
        
        # Step 3: Remove punctuation
        myCorpus <- tm_map(myCorpus, removePunctuation)

}

