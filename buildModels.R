################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# Script to build, evaluate and document the prediction model                  #
# Adjust build configuraiton parameters below before running                   #
#                                                                              #
# Be sure to do the following before sourcing:                                 #
# Update model.filename (increment value)                                      #
# Adjust model threshold parameters                                            #
# Validate sample size to draw from                                            #
################################################################################

# Build Configuration Parameters

model.date <- Sys.Date()
model.filename <- 'ngram_model_22.rds'     # not yet self incrementing
model.filesize <- 0                        # to be calculated
model.threshold.quadgram <- 2              # low frequency threshold
model.threshold.trigram <- 2               # low frequency threshold
model.threshold.bigram <- "Above Average continuation probability"                # low frequency threshold
#model.threshold.bigram <- 1
model.threshold.unigram <- NA              # low frequency threshold
model.ngramcount.total <- 0                # To be calculated
model.ngramcount.quadgrams <- 0            # To be calculated
model.ngramcount.trigrams <- 0             # To be calculated
model.ngramcount.bigrams <- 0              # To be calculated
model.ngramcount.unigrams <- 0             # To be calculated
model.smoothing <- 'Modified Kneser-Nye'
model.performance.avgoverallaccuracy <- 0  # to be calculated
model.performance.avgtop3accuracy <- 0     # to be calculated
model.performance.avgcrossentropy <- 0     # to be calculated
model.performance.avgtop3crossentropy <- 0 # to be calculated
model.performance.avgquerytime <- 0        # to be calculated

# sample.size - should make this drive sample file selection, is just a pass 
# through now
sample.size <- 0.20
fullmodelfilename <- paste0('ngram_model_full_', as.character(sample.size), '.rds')
model.full.exists <- file.exists(file.path(modellibrarydir, fullmodelfilename))
if (model.full.exists == TRUE) {
        load(file.path(modellibrarydir, fullmodelfilename))
}
if (model.full.exists == FALSE)  {
                
        
        ################################ LOAD CORPUS ###################################
        
        # Load Corpus from sample file
        print("Loading corpus...")
        myCorpus <- loadCorpus(samplefiletype = 'training', sample.size = sample.size)
        # Replace Unigrams with frequency 1 with UNK
        unkWords <- identifyLowFreqTerms(myCorpus, lowfreq = 1)
        
        #unkWords <- ""
        
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
        
        ################################ SAVE MODEL ############################
        
        # remove unnecessary columns probability and nwordtypes
        dt_model <- dt_model[, .(prefix, nextword, ngramlength, freq, Pkn)]
        
        # Save off a copy of the full model
        save(dt_model, file=file.path(modellibrarydir, fullmodelfilename))

}
####################################### PRUnE MODEL ############################
print("Pruning...")

# SPECIAL CASE: Using average continuation probability
avg.bigram.Pkn <- mean(unlist(dt_model[ngramlength == 2, .(Pkn)]))
dt_model <- dt_model[(ngramlength == 4 & freq >= model.threshold.quadgram) | (ngramlength == 3 & freq >= model.threshold.trigram) | (ngramlength == 2 & Pkn >= avg.bigram.Pkn)]

# DEFAULT CASE: Using ngram threasholds only
#dt_model <- dt_model[(ngramlength == 4 & freq >= model.threshold.quadgram) | (ngramlength == 3 & freq >= model.threshold.trigram) | (ngramlength == 2 & freq >= model.threshold.bigram)]

# Set the key to be prefix, and order by descending Pkn
dt_model <- dt_model[order(prefix, -ngramlength, -Pkn)]
setkey(dt_model, prefix)

############################## EVALUATE ACCURACY ###############################

print("Evaluating Accuracy and Calculating Cross Entropy...")
testResults.1 <- testModel(12345)
testResults.2 <- testModel(23456)

number.tested.total <- testResults.1["number.tested"] + testResults.2["number.tested"]
number.correct.total <- testResults.1["number.correct"] + testResults.2["number.correct"]
number.correct.top3.total <- testResults.1["number.correct.top3"] + testResults.2["number.correct.top3"]

model.performance.avgoverallaccuracy <- number.correct.total / number.tested.total
model.performance.avgtop3accuracy <- number.correct.top3.total / number.tested.total

model.performance.avgcrossentropy <- (testResults.1["cross.entropy.tally"] +
                                   testResults.2["cross.entropy.tally"] ) /
                                   number.tested.total

model.performance.avgtop3crossentropy <- (testResults.1["cross.entropy.tally.top3"] +
                                              testResults.2["cross.entropy.tally.top3"] ) /
                                              number.tested.total

model.performance.avgquerytime = (testResults.1["querytime.cum"] + testResults.2["querytime.cum"]) / number.tested.total

############################ GATHER NGRAM COUNTS ###############################

print("Gathering Ngram Counts...")
model.ngramcount.total <- nrow(dt_model)
model.ngramcount.unigrams <- nrow(dt_model[ngramlength == 1])
model.ngramcount.bigrams <- nrow(dt_model[ngramlength == 2])
model.ngramcount.trigrams <- nrow(dt_model[ngramlength == 3])
model.ngramcount.quadgrams <- nrow(dt_model[ngramlength == 4])

############################## SAVE & DOCUMENT MODEL ###########################

print("Saving model to disk...")
save(dt_model, file=file.path(modellibrarydir, model.filename))
model.filesize <- round(file.size(file.path(modellibrarydir, model.filename)) / 1000000, digits = 1)

print("Documenting the model...")
model.data <-  data.frame(model.date = model.date, 
                          model.filename = model.filename, 
                          model.filesize = model.filesize, 
                          sample.size = sample.size, 
                          model.threshold.quadgram = model.threshold.quadgram, 
                          model.threshold.trigram = model.threshold.trigram,
                          model.threshold.bigram = model.threshold.bigram, 
                          model.threshold.unigram = model.threshold.unigram,
                          model.ngramcount.total = model.ngramcount.total,
                          model.ngramcount.quadgrams = model.ngramcount.quadgrams,
                          model.ngramcount.trigrams = model.ngramcount.trigrams,
                          model.ngramcount.bigrams = model.ngramcount.bigrams, 
                          model.ngramcount.unigrams = model.ngramcount.unigrams,
                          model.smoothing = model.smoothing,
                          model.performance.avgoverallaccuracy = model.performance.avgoverallaccuracy,
                          model.performance.avgcrossentropy = model.performance.avgcrossentropy,
                          model.performance.avgquerytime = model.performance.avgquerytime,
                          model.performance.avgtop3accuracy = model.performance.avgtop3accuracy,
                          model.performance.avgtop3crossentropy = model.performance.avgtop3crossentropy)

write.table(model.data, file = file.path(modellibrarydir, modeldocfilename), row.names = FALSE, append = TRUE, sep = ",", col.names = FALSE)
