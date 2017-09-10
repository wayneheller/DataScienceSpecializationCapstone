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
# To create a term document matrix of Bigrams                                  #
# > myDtm <- tokenizeCorpus(myCorpus, ngram=2)                                 #
################################################################################

library(tm)
library("RWeka")

# path variable names
datadir <- file.path(getwd(), "data")

sampledir <- file.path(datadir, "sampleddata")
sampledirarchive <- file.path(datadir, "sampleddatearchive")
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
# NOTE: will archive existing sample file first
createSampleData <- function(datadir) {
        sample.size <- 0.01
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
        #if(file.exists(file.path(sampledir, samplefilename))) {
        #        file.remove(file.path(sampledir, samplefilename))
        #}
        
        # Archive old sample file(s) if present
        archive.sample(sampledir)
        
        # Create new sample file
        conn <- file(file.path(sampledir, samplefilename), open = "w", encoding = "UTF-8" )
        write(samples, conn)
        close(conn)
}

# open sample data as a corpus
# input: full path to the directory containing the sampled data file
# output: a Corpus object
openCorpus <- function(sampleDir) {
        # openning as a VCorpus vs. a simple one - there is an issue with the 
        # ngramtokenizer not workings as desired.  See function tokenizeCorpus
        # for more info
        myCorpus <- VCorpus(DirSource(sampleDir, encoding = "UTF-8"),
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
        
        print("Removing Banned Words...")
        # This file contains non-breaking whitespace that needs to be removed
        chrBandWords <- sapply(BandWords[, 1], removenbsp)
        myCorpus <- tm_map(myCorpus, removeWords, chrBandWords)
        
        # This step is no longer needed since we remove all non-ASCII characters
        # when creating this file
        # Step 2: Remove special characters of the form <U+9999>"
        # gsub("<U\\+\\d+>", "", x, fixed=FALSE)
        #myCorpus <- tm_map(myCorpus, removePattern, "<") 
        
        # Step 3: Remove punctuation and numbers
        print("Remvoing punctuation...")
        myCorpus <- tm_map(myCorpus, removePunctuation)
        print("Remvoing numbers")
        myCorpus <- tm_map(myCorpus, removeNumbers)
        
        # Step 4: Remove english stop words (Not sure we want to do this for the final project)
        print("Remvoing stop words")
        myCorpus <- tm_map(myCorpus, removeWords, stopwords(kind = "en"))
}




# This fuction returns a term document matrix for unigrams, bigrams, trigrams or some combination of these
# inputs are a corpus, the desired ngram OR the min and max of the ngrams to return
# in the case where min and max are specified, ngram term is ignored
# default is to return unigrams as a Document Term Matrix
tokenizeCorpus <- function(myCorpus, ngram= 1, minWords=0, maxWords=0) {
        # There is an issue with verion 0.7 of tm that prevents bigram tokenization
        # I currently have tm_0.7-1 installed
        # https://stackoverflow.com/questions/42538223/ngramtokenizer-not-working-as-expected
        # one solution it to revert tm to prior version, another solution is to
        # open the corpus using VCorpus - I went with the later option
        # https://stackoverflow.com/questions/43410491/2-gram-and-3-gram-instead-of-1-gram-using-rweka
        
        UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
        BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
        TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
        
        if ((minWords > 0) & (maxWords > minWords)) {
                MultigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = minWords, max = maxWords))
                dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = MultigramTokenizer)) 
                }
        else if (ngram == 1) {
                dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = UnigramTokenizer))    
        }
        else if (ngram == 2){
                dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = BigramTokenizer))
        }
        else if (ngram == 3){
                dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = TrigramTokenizer))
        }
        
        
        
        
        #plot(tdm, terms = findFreqTerms(tdm, lowfreq = 2)[1:50], corThreshold = 0.5)  
        
        return(dtm)
        
}

# Utility function to archive sampled files
utility.file.rename <- function(from, to) {
        todir <- dirname(to)
        if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
        file.rename(from = from,  to = to)
}

# clean out sampledir in prep for creating a new one
archive.sample <- function(sampleDir){
        samplefiles <- list.files(sampleDir)
        for (f in samplefiles) {
                print(f)
                utility.file.rename(file.path(sampleDir, f), file.path(sampledirarchive,f))
        }
}
