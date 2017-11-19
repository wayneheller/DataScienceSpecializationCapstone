# Scripts to build the prediction model

# Construct simple backoff model

# Load Corpus from sample file
print("Loading corpus...")
myCorpus <- loadCorpus(samplefiletype = 'training', sample.size = 0.10)
# Replace Unigrams with frequency 1 with UNK
#unkWords <- identifyLowFreqTerms(myCorpus, lowfreq = 1)

unkWords <- ""

# Build individual models and then combine


################################ QUADRAGRAMS ###################################

# Get document feature matrix of Ngrams
print("getting Dfm for quadgrams...")
myDfm <- getDfm(myCorpus, ngram = 4, unkWords)
# Threshold parameter set finding natural cutoff point in data, see function 
# description for more details
# 11/19 preserving all words to provide basis for modified Kneser-Nye smoothing
#myDfm <- pruneDfm(myDfm, frequencyThreshold = 2)  

# get dataframe of ngram probabilities
dt.4 <- calcNgramProb(myDfm, ngramLength = 4)

################################### TRIGRAMS ###################################

# Get document feature matrix of Trigrams
print("getting Dfm for trigrams...")
myDfm <- getDfm(myCorpus, ngram = 3, unkWords)
# Threshold parameter set finding natural cutoff point in data, see function 
# description for more details
# 11/19 preserving all words to provide basis for modified Kneser-Nye smoothing
#myDfm <- pruneDfm(myDfm, frequencyThreshold = 2)  

# get dataframe of ngram probabilities
dt.3 <- calcNgramProb(myDfm, ngramLength = 3)

################################### BIGRAMS ####################################
print("getting Dfm for bigrams...")

myDfm <- getDfm(myCorpus, ngram = 2, unkWords)
# Threshold parameter set finding natural cutoff point in data, see function 
# description for more details
# 11/19 preserving all words to provide basis for modified Kneser-Nye smoothing
#myDfm <- pruneDfm(myDfm, frequencyThreshold = 2)  

# get dataframe of ngram probabilities
dt.2 <- calcNgramProb(myDfm, ngramLength = 2)

################################## UNIGRAMS ####################################
print("getting Dfm for unigrams...")
myDfm <- getDfm(myCorpus, ngram = 1, unkWords)
# Threshold parameter set finding natural cutoff point in data, see function 
# description for more details
# 11/18 preserving all words to provide basis for modified Kneser-Nye smoothing
#myDfm <- pruneDfm(myDfm, frequencyThreshold = 2)

# get dataframe of ngram probabilities
dt.1 <- calcNgramProb(myDfm, ngramLength = 1)

################################## COMBINE #####################################

print("Combining models...")
dt_model <- rbindlist(list(dt.4, dt.3, dt.2, dt.1), use.names = TRUE)

remove(dt.1, dt.2, dt.3, dt.4)

############################ CALCULATE DISCOUNTS ###############################

print("Calculating discounts...")
N.1 <- nrow(dt_model[freq == 1, .(freq)]) 
N.2 <- nrow(dt_model[freq == 2, .(freq)]) 
N.3 <- nrow(dt_model[freq == 3, .(freq)]) 
N.4 <- nrow(dt_model[freq == 4, .(freq)]) 

Y <- N.1 / (N.1 + 2 * N.2)
D.1 <- 1 - 2 * Y * N.2 / N.1
D.2 <- 2 - 3 * Y * N.3 / N.2
D.3 <- 3 - 4 * Y * N.4 / N.3

print(Y)
print(D.1)
print(D.2)
print(D.3)

########################## SMOOTH PROBABILITIES ################################

print("Smoothing...")
applyKneserNeySmoothing()

################################ PRUNE MODEL ###################################

print("Pruning...")

# remove unnecessary columns probability and nwordtypes
dt_model <- dt_model[, .(prefix, nextword, ngramlength, freq, Pkn)]


# prune quadgrams and trigrams, remove unigrams
dt_model <- dt_model[(ngramlength == 4 & freq >= 2) | (ngramlength == 3 & freq >= 2) | ngramlength == 2]


# Set the key to be prefix, and order by descending Pkn
dt_model <- dt_model[order(prefix, -ngramlength, -Pkn)]
setkey(dt_model, prefix)

############################## TEST ACCURACY ###################################

print("Testing...")
testModel()