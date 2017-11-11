# Scripts to build the prediction model

# Construct simple backoff model

# Load Corpus from sample file
myCorpus <- loadCorpus()
# Replace Unigrams with frequency 1 with UNK
unkWords <- identifyLowFreqTerms(myCorpus, lowfreq = 1)

# Build individual models and then combine


################################ QUADRAGRAMS ###################################

# Get document feature matrix of Ngrams
myDfm <- getDfm(myCorpus, ngram = 4, unkWords)
# Threshold parameter set finding natural cutoff point in data, see function 
# description for more details
#myDfm <- pruneDfm(myDfm, freqThreshold = .09)  

# get dataframe of ngram probabilities
dt.4 <- calcNgramProb(myDfm, ngramLength = 4)

################################### TRIGRAMS ###################################

# Get document feature matrix of Trigrams
myDfm <- getDfm(myCorpus, ngram = 3, unkWords)
# Threshold parameter set finding natural cutoff point in data, see function 
# description for more details
#myDfm <- pruneDfm(myDfm, freqThreshold = .30)  

# get dataframe of ngram probabilities
dt.3 <- calcNgramProb(myDfm, ngramLength = 3)

################################### BIGRAMS ####################################

myDfm <- getDfm(myCorpus, ngram = 2, unkWords)
# Threshold parameter set finding natural cutoff point in data, see function 
# description for more details
myDfm <- pruneDfm(myDfm, freqThreshold = .73)  

# get dataframe of ngram probabilities
dt.2 <- calcNgramProb(myDfm, ngramLength = 2)

################################## UNIGRAMS ####################################

myDfm <- getDfm(myCorpus, ngram = 1, unkWords)
# Threshold parameter set finding natural cutoff point in data, see function 
# description for more details
myDfm <- pruneDfm(myDfm, freqThreshold = .96)  

# get dataframe of ngram probabilities
dt.1 <- calcNgramProb(myDfm, ngramLength = 1)

################################## COMBINE #####################################

dt_model <- rbindlist(list(dt.4, dt.3, dt.2, dt.1), use.names = TRUE)

remove(dt.1, dt.2, dt.3, dt.4)

