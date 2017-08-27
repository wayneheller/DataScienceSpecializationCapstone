################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# To create the sample data file:                                              #
# > createSampleData(datadir)                                                  #
#                                                                              #
# To open the corpus:                                                          #
# > myCorpus <- openCorpus(sampledir)                                          #
#                                                                              #
# To remove profanity, puncuation, whitespace, etc.                            #
# > myCorpus <- cleanCorpus(myCorpus, metadatadir)                             #
#                                                                              #
################################################################################

library(tm)

# path variable names
datadir <- file.path(getwd(), "data")
sampledir <- file.path(datadir, "sampleddata")
metadatadir <- file.path(datadir, "metadata")

# helper function to convert an ascii code to its character
#used by cleanCorpus()
chr <- function(n) { rawToChar(as.raw(n)) }

# creates a sample dataset from a file
# as inputs: the full path to the file, total number of rows in the file, 
# and the sample size
# expressed as a decimal between 0.0 and 1.0
sampleText <- function (filename, totalrows, sampleSize) {  
        # for reproducibility
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
                        # Add 1 line to the sample row set, stripping out the non-ASCII characters
                        sample <- list(sample, iconv(oneLine, from = "latin1", to="ASCII", sub=""))   
                }
        }
        close(conn)
        return(unlist(sample))
}

# Read and sample text files
# Row counts were determined by opening file in a text editor
# input is the full path to the directory containing the original corpus
# output: a Corpus object
createSampleData <- function(datadir) {
        sample.size <- 0.00001
        samplefilename <- paste0("sampledata_samplesize_", as.character(sample.size), ".txt")
        
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
        if(file.exists(file.path(sampledir, samplefilename))) {
                file.remove(file.path(sampledir, samplefilename))
        }
        
        # Create new sample file
        conn <- file(file.path(sampledir, samplefilename), open = "w", encoding = "UTF-8" )
        write(samples, conn)
        close(conn)
}

# open sample data as a corpus
# input: full path to the directory containing the sampled data file
# output: a Corpus object
openCorpus <- function(sampleDir) {
        #txt <- system.file(sampleDir, "txt", package = "tm")
        #print(txt)
        myCorpus <- Corpus(DirSource(sampleDir, encoding = "UTF-8"),
                           readerControl = list(language = "lat"))

        return(myCorpus)
}

# This function removes non-breaking spaces ascii character 160
# The trimws() function does not handle these
# Used by cleanCorpus() in the preparation of the banned words file
removenbsp <- function(x) {
        return(gsub(chr(160), "", x, fixed = TRUE))
}

# This function is a generic content transformer used by function cleanCorpus to replace special characters with ""
# Currently not used by cleanCorpus but may need it later, so retaining it.
removePattern <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed = TRUE))

# Removes Profanity, Whitespace, and Punctuation
cleanCorpus <- function(myCorpus, metadataDir) {
        # Step 1: Remove Profane words from a list originally published by Google
        BannedWordFileName <- "full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
        pathToBannedWordList <- file.path(metadataDir, BannedWordFileName)
        BandWords <- read.csv(pathToBannedWordList, sep="\t", strip.white = TRUE)
        # This file contains non-breaking whitespace that needs to be removed
        myCorpus <- tm_map(myCorpus, removeWords, sapply(BandWords[, 1], removenbsp))
        
        # This step is no longer needed since we remove all non-ASCII characters
        # when creating this file
        # Step 2: Remove special characters of the form <U+9999>"
        # gsub("<U\\+\\d+>", "", x, fixed=FALSE)
        #myCorpus <- tm_map(myCorpus, removePattern, "<") 
        
        # Step 3: Remove punctuation
        myCorpus <- tm_map(myCorpus, removePunctuation)

}

