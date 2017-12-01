################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# Script by buildModel script to Test Accuracy and compute Cross Entropy of    #
# Models                                                                       #
#                                                                              #
#                                                                              #
################################################################################



library(tm)  # used for some helper functions only.  

# Performs calcuation on 1 sentence
# assumes dt_model has been loaded into the environment
calcAccuracyEntropy <- function(test.phrase, max.ngram.length = 4) {
        
        lambda.error <- .000001 # a error term for 0 count predictions
        
        correct.tally <- 0
        correct.tally.top3 <-0          # correct word in top 3 predictions
        cross.entropy.tally <- 0
        cross.entropy.tally.top3 <- 0
        querytime.cum <- 0
        
        test.ngrams <- tokens(test.phrase, what = 'word', ngrams = 2:max.ngram.length, concatenator = " ")
        test.ngrams <- unlist(test.ngrams)
        test.ngrams.length <- length(test.ngrams)
        
        for (each.ngram in test.ngrams) {
                words.in.each.ngram <- unlist(tokens(each.ngram, what = 'word', ngram = 1))
                words.prefix <- paste(words.in.each.ngram[1:length(words.in.each.ngram) - 1],  collapse = " ")
                # print(words.prefix)
                # print(words.in.each.ngram[length(words.in.each.ngram)])
                
                query.start <- Sys.time()
                dt_result <- queryModelNextWord(dt_model, words.prefix, topN = 1)
                query.end <- Sys.time()
                querytime.cum <- querytime.cum + difftime(query.end, query.start, units = "sec")
                
                #print(dt_result)
                if (nrow(dt_result) == 0){
                        # no predictions returned
                        # correct.tally remains unchanged
                        cross.entropy.tally <- cross.entropy.tally - log(lambda.error)
                }
                else {
                        # NEXT STEP: Modify This Section for Top 3 correct
                        predicted.nextword <- dt_result[1, nextword]
                        cross.entropy.tally <- cross.entropy.tally - log(dt_result[1, Pkn])
                        # print(predicted.nextword)
                        correct.nextword <- words.in.each.ngram[length(words.in.each.ngram)]
                        # first predicted word is correct
                        if (predicted.nextword == correct.nextword) {
                                # correct prediction
                                correct.tally <- correct.tally + 1
                                
                                
                        }
                        
                        # test for accuracy in top 3, handles case where there are only 1 or 2 predictions
                        predicted.nextword.count <- min(3, nrow(dt_result))
                        predicted.nextwords <- dt_result[1:predicted.nextword.count, nextword]
                        if (correct.nextword %in% predicted.nextwords) {
                                correct.tally.top3 <- correct.tally.top3 + 1

                                # modified the which statement to handle cases where there are duplicate predictions
                                cross.entropy.tally.top3 <- cross.entropy.tally.top3 - log(dt_result[which(predicted.nextwords == correct.nextword)[1], Pkn])
                        }
                        else {
                                cross.entropy.tally.top3 <- cross.entropy.tally.top3 - log(lambda.error)
                        }
                        
                        
                        
                }
                
        }
        # print(correct.tally / test.ngrams.length)
        # print(cross.entropy.tally)
        
        return(c(number.correct = correct.tally, number.correct.top3 = correct.tally.top3, number.tested = test.ngrams.length, cross.entropy.tally = cross.entropy.tally, cross.entropy.tally.top3 = cross.entropy.tally.top3, querytime.cum = querytime.cum))
        
}

# assumes dt_model, path variables are loaded into the environment
# Samples testing data set
testModel <- function(seedValue) {
        
        set.seed(seedValue)
        #set.seed(12345)
        #set.seed(23456)
        
        # initialize counters to 0
        number.correct <- 0             # first word correct
        number.correct.top3 <- 0        # correct word in top 3
        number.tested <- 0
        cross.entropy.tally <- 0
        cross.entropy.tally.top3 <- 0
        querytime.cum <- 0              # total time spent querying the model
        
        # open testing data as a corpus
        myCorpus.test <- loadCorpus(samplefiletype = 'testing', sample.size = 0.05)
        
        # tokenize into sentences
        tokens.sentences <- unlist(tokens(myCorpus.test, what = 'sentence', 
                                   remove_numbers = TRUE, remove_separators = TRUE, remove_punct = TRUE, 
                                   remove_twitter = TRUE, remove_symbols = TRUE), use.names = FALSE)
        
        # sample sentences
        tokens.sample <- runif(100, min = 1, max = length(tokens.sentences))
        # For some reason remove_punct is not getting rid of all punctuation
        tokens.sentences.sample <- char_tolower(removePunctuation(tokens.sentences[tokens.sample]))
        # print(tokens.sentences.sample)
        
        
        # test each sentence and tally results
        for (sentence in tokens.sentences.sample) {
                
                test_results <- calcAccuracyEntropy(sentence, max.ngram.length = 4)
                number.tested <- number.tested + test_results["number.tested"]
                number.correct <- number.correct + test_results["number.correct"]
                number.correct.top3 <- number.correct.top3 + test_results["number.correct.top3"]
                cross.entropy.tally <- cross.entropy.tally + test_results["cross.entropy.tally"]
                cross.entropy.tally.top3 <- cross.entropy.tally.top3 + test_results["cross.entropy.tally.top3"]
                querytime.cum <- querytime.cum + test_results["querytime.cum"]
        }
        
        
        # print out results
        print(number.tested)
        
        print(number.correct)
        print(number.correct / number.tested)
        
        print(number.correct.top3)
        print(number.correct.top3 / number.tested)
        
        print(cross.entropy.tally)
        print(cross.entropy.tally / number.tested)
        
        print(cross.entropy.tally.top3)
        print(cross.entropy.tally.top3 / number.tested)
        
        print(querytime.cum / number.tested)
        
        return(c(number.tested,
               number.correct,
               number.correct.top3,
               cross.entropy.tally,
               cross.entropy.tally.top3,
               querytime.cum))

        
        
}