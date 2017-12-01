################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# As of 11/23/2017 only used to create sample data files                       #
# To create a 10% sample for training the model use :                          #
# > createSampleData(datadir, 0.10, 'training'                                 #
#                                                                              #
# EVERYTHING ELSE BELOW IS DEPRECATED                                          #
# To open the corpus:                                                          #
# > myCorpus <- openCorpus(sampledir)                                          #
# To see current sample size                                                   #
# > meta(myCorpus)                                                             #
#                                                                              #
# To remove profanity, puncuation, whitespace, etc.                            #
# > myCorpus <- cleanCorpus(myCorpus, metadatadir)                             #
#                                                                              #
# To split each entry into a separate document                                 #
# > myCorpus <- splitOutDocsinCorpus(myCorpus)                                 #
#                                                                              #
# To create a term document matrix of Bigrams                                  #
# > myDtm <- tokenizeCorpus(myCorpus, ngram=2)                                 #
#                                                                              #
# To start fresh with a new Corpus                                             #
# > myCorpus <- newCorpus()                                                    #
################################################################################

## ---- preprocessing

#library(tm)
#library("RWeka")
#library("SnowballC")

# path variable names
#datadir <- file.path(getwd(), "data")

#sampledir <- file.path(datadir, "trainingdata")
#sampledirarchive <- file.path(datadir, "sampleddatearchive")
#metadatadir <- file.path(datadir, "metadata")
#devdatadir <- file.path(datadir, "devdata")
#testingdatadir <- file.path(datadir, "testingdata")

# helper function to convert an ascii code to its character
#used by cleanCorpus()
#chr <- function(n) { rawToChar(as.raw(n)) }

# Append chunk of sample to sample file
appendChunk <- function(samplefilename, chunck) {
        if (file.exists(samplefilename)) {
                openmode = "a"
        }
        else openmode = "w"
        
        conn <- file(samplefilename, open = openmode, encoding = "UTF-8" )
        write(chunck, conn)
        close(conn)
}

# creates a training, testing and dev datasets from a source file in the corpus
# as inputs: the full path to the file, total number of rows in the file, 
# and the sample size expressed as a decimal between 0.0 and 1.0
# sammplefiletype = training|testing|development
sampleText <- function (sourcefilename, samplefiletype, totalrows, sample.size) {  
        
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
        
        seed.value <- switch(samplefiletype,
                                 training = 12345,
                                 testing =  23456,
                                 development =  34567
        )
        
        
        
        # for reproducibility
        set.seed(seed.value)
        
        # generate line numbers of the file to include in the sample
        lineNumber <-  sample(totalrows, sample.size * totalrows)

        sample <- list() # initialize the sample
        
        conn <- file(sourcefilename, open = "rb") # open connection to the file using rb to deal with Cntl-Z in news file

        row <- 0 # initialize row counter
        chunkcount <- 0 # initialize chunk counter
        
        #read file 1 line at a time 
        while (length(oneLine <- readLines(conn, n = 1, warn = TRUE, skipNul = TRUE)) > 0) {
                
                row <- row + 1
                        
                if(row %in% lineNumber) {
                        # Add 1 line to the sample row set, stripping out the non-ASCII characters
                        # Add begining of line token <s> to each sample row.
                        oneLine <- paste('<s>', oneLine)
                        sample <- list(sample, iconv(oneLine, from = "latin1", to="ASCII", sub="")) 
                        print(paste(sourcefilename, row))
                        # increment chunk count
                        chunkcount <- chunkcount + 1
                        if(chunkcount == 10000) {
                                print("Appending sample to file...")
                                appendChunk(samplefilename, unlist(sample))
                                chunkcount <- 0
                                sample <- list()
                        }
                        
                }
        }
        # write out rest of sample
        print("Appending last sample to file...")
        appendChunk(samplefilename, unlist(sample))
        chunkcount <- 0
        sample <- list()
        
        close(conn)
}

# Read and sample text files
# Row counts were determined by opening file in a text editor
# input is the full path to the directory containing the original corpus
# output: a Corpus object
# NOTE: will archive existing sample file first
# sample.size 0 < x < 1
# samplefiletype = 'training' 'testing' 'development'
createSampleData <- function(datadir, sample.size, samplefiletype) {
        
        file.news <- "en_US.news.txt"
        file.news.rows <- 1010242
        
        file.blogs <- "en_US.blogs.txt"
        file.blogs.rows <- 899289
        
        file.twitter <- "en_US.twitter.txt"
        file.twitter.rows <- 2360148
        
        # Archive old  file(s) if present
        dirtoarchive <- switch(samplefiletype,
                                 training = sampledir,
                                 testing =  testingdatadir,
                                 development =  devdatadir
        )
        
        #archive.sample(dirtoarchive)
        
        # Create sample for each source file
        print("Creating blog sample...")
        sampleText(file.path(datadir, file.blogs), samplefiletype, file.blogs.rows, sample.size)

        print("Creating news sample...")
        sampleText(file.path(datadir, file.news), samplefiletype, file.news.rows, sample.size)
        
        print("Creating twitter sample...")
        sampleText(file.path(datadir, file.twitter), samplefiletype, file.twitter.rows, sample.size)
        
        print("Done.")
}

# open sample data as a corpus
# input: full path to the directory containing the sampled data file
# output: a Corpus object
#openCorpus <- function(sampleDir) {
        # openning as a VCorpus vs. a simple one - there is an issue with the 
        # ngramtokenizer not workings as desired.  See function tokenizeCorpus
        # for more info
#        print("Opening Sample Directory as VCorpus...")
#        myCorpus <- VCorpus(DirSource(sampleDir, encoding = "UTF-8", mode="binary"),
#                           readerControl = list(language = "lat"))

        # add meta data tag
#        print("Adding meta data tag...")
#        sample.size <- getSampleSize(sampleDir)
#        print(paste("Sample Size =", sample.size))
        
#        meta(myCorpus, "SampleSize") <- sample.size
        
#        return(myCorpus)
#}

# This function removes non-breaking spaces ascii character 160
# The trimws() function does not handle these
# Used by cleanCorpus() in the preparation of the banned words file
#removenbsp <- function(x) {
#        return(gsub(chr(160), "", x, fixed = TRUE))
#}

# This function is a generic content transformer used by function cleanCorpus to replace special characters with ""
# Currently not used by cleanCorpus but may need it later, so retaining it.
#removePattern <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed = TRUE))

# Removes Profanity, Whitespace, and Punctuation
#cleanCorpus <- function(myCorpus, metadataDir) {
#        print("Cleaning corpus content...")
#        
        # Step 1: Convert to lower case
#        print("Converting to lower case...")
#        myCorpus <- tm_map(myCorpus, content_transformer(tolower))
        
        # Step 2: Remove Profane words from a list originally published by Google
#        BannedWordFileName <- "full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt"
#        pathToBannedWordList <- file.path(metadataDir, BannedWordFileName)
#        BandWords <- read.csv(pathToBannedWordList, sep="\t", strip.white = TRUE)
#        
#        print("Removing Banned Words...")
        # This file contains non-breaking whitespace that needs to be removed
#        chrBandWords <- sapply(BandWords[, 1], removenbsp)
#        myCorpus <- tm_map(myCorpus, removeWords, chrBandWords)
#        
        # This step is no longer needed since we remove all non-ASCII characters
        # when creating this file
        # Step 3: Remove special characters of the form <U+9999>"
        # gsub("<U\\+\\d+>", "", x, fixed=FALSE)
        #myCorpus <- tm_map(myCorpus, removePattern, "<") 
        
        # Step 3: Remove punctuation and numbers
#        print("Removing punctuation...")
#        myCorpus <- tm_map(myCorpus, removePunctuation)
#        print("Removing numbers...")
#        myCorpus <- tm_map(myCorpus, removeNumbers)
        
        # Step 4: Remove english stop words (Not sure we want to do this for the final project)
#        print("Removing stop words...")
#        myCorpus <- tm_map(myCorpus, removeWords, stopwords(kind = "en"))
        
        # Step 5: Convert to Plain Text Document and apply Stemmming
        #print("Converting to plain text document...")
        #myCorpus <- tm_map(myCorpus, PlainTextDocument)  # needs to come before stemming
#       print("Stemming...")
#        myCorpus <- tm_map(myCorpus, stemDocument, "english")
        
#        return(myCorpus)
#}

# converts each line of the corpus into a separate document
#splitOutDocsinCorpus <- function(myCorpus) {
#        print("Splitting content into individual documents...")
#        sample.size <- meta(myCorpus)$SampleSize
#        print(sample.size)
#        myCorpus <- VCorpus(VectorSource(myCorpus[[1]]$content))
#        meta(myCorpus, "SampleSize") <- sample.size
        #print(meta(myCorpus)$SampleSize)
#        return(myCorpus)
#}

# This function returns a cleaned Corpus of individual documents from the sample in the sampledir
#newCorpus <- function() {
#        myCorpus <- openCorpus(sampledir)
#        myCorpus <- cleanCorpus(myCorpus, metadatadir)
        # splitting out individual entries into documents makes working with the DTM computationally
        # complex and in some cases isn't necessary
        #myCorpus <- splitOutDocsinCorpus(myCorpus)
#        return(myCorpus)
#}
                                 

# This fuction returns a term document matrix for unigrams, bigrams, trigrams or some combination of these
# inputs are a corpus, the desired ngram OR the min and max of the ngrams to return
# in the case where min and max are specified, ngram term is ignored
# default is to return unigrams as a Document Term Matrix
#tokenizeCorpus <- function(myCorpus, ngram= 1, minWords=0, maxWords=0) {
        # There is an issue with verion 0.7 of tm that prevents bigram tokenization
        # I currently have tm_0.7-1 installed
        # https://stackoverflow.com/questions/42538223/ngramtokenizer-not-working-as-expected
        # one solution it to revert tm to prior version, another solution is to
        # open the corpus using VCorpus - I went with the later option
        # https://stackoverflow.com/questions/43410491/2-gram-and-3-gram-instead-of-1-gram-using-rweka
        
#        UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
#        BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#        TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
        
#        if ((minWords > 0) & (maxWords > minWords)) {
#                MultigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = minWords, max = maxWords))
#                dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = MultigramTokenizer)) 
#                }
#        else if (ngram == 1) {
                #print(class(myCorpus))
#                dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = UnigramTokenizer))    
#        }
#        else if (ngram == 2){
#               dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = BigramTokenizer))
#       }
#       else if (ngram == 3){
#               dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize = TrigramTokenizer))
#       }
        
        
#       return(dtm)
        
#}

# Utility function to archive sampled files
#utility.file.rename <- function(from, to) {
#        todir <- dirname(to)
#        if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
#        file.rename(from = from,  to = to)
#}

# clean out sampledir in prep for creating a new one
#archive.sample <- function(sampleDir){
#        samplefiles <- list.files(sampleDir)
#        for (f in samplefiles) {
#                print(f)
#                utility.file.rename(file.path(sampleDir, f), file.path(sampledirarchive,f))
#        }
#}


## ---- end-preprocessing

