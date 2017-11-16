# To Test Accuracy and Cross Entropy of Models

# Performs calcuation on 1 sentence
# assumes dt_model has been loaded into the environment
calcAccuracyEntropy <- function(test.phrase, max.ngram.length = 4) {
        
        lambda.error <- .000001 # a error term for 0 count predictions
        
        correct.tally <- 0
        cross.entropy.tally <- 0
        
        test.ngrams <- tokens(test.phrase, what = 'word', ngrams = 2:max.ngram.length, concatenator = " ")
        test.ngrams <- unlist(test.ngrams)
        test.ngrams.length <- length(test.ngrams)
        
        for (each.ngram in test.ngrams) {
                words.in.each.ngram <- unlist(tokens(each.ngram, what = 'word', ngram = 1))
                words.prefix <- paste(words.in.each.ngram[1:length(words.in.each.ngram) - 1],  collapse = " ")
                # print(words.prefix)
                # print(words.in.each.ngram[length(words.in.each.ngram)])
                
                dt_result <- queryModelNextWord(words.prefix, topN = 1)
                #print(dt_result)
                if (nrow(dt_result) == 0){
                        # no predictions returned
                        # correct.tally remains unchanged
                        cross.entropy.tally <- cross.entropy.tally - log(lambda.error)
                }
                else {
                        predicted.nextword <- dt_result[1, nextword]
                        # print(predicted.nextword)
                        if (predicted.nextword == words.in.each.ngram[length(words.in.each.ngram)]) {
                                # correct prediction
                                correct.tally <- correct.tally + 1
                        }
                        
                        cross.entropy.tally <- cross.entropy.tally - log(dt_result[1, probability])
                }
                
        }
        # print(correct.tally / test.ngrams.length)
        # print(cross.entropy.tally)
        
        return(c(number.correct = correct.tally, number.tested = test.ngrams.length, cross.entropy.tally = cross.entropy.tally))
        
}

# assumes dt_model, path variables are loaded into the environment
# Samples testing data set
testModel <- function() {
        
        #set.seed(12345)
        set.seed(23456)
        
        # initialize counters to 0
        number.correct <- 0
        number.tested <- 0
        cross.entropy.tally <- 0
        
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
                cross.entropy.tally <- cross.entropy.tally + test_results["cross.entropy.tally"]
        }
        
        
        # print out results
        print(number.tested)
        print(number.correct)
        print(number.correct / number.tested)
        print(cross.entropy.tally)
        print(cross.entropy.tally / number.tested)

        
        
}