################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# To create a bar plot of top 10 words                                         #
# > barPlotFreq(myDtm, topN = 10, sample.size = meta(myCorpus)$SampleSize)     #
#                                                                              #
# To create a graph network diagram of top 5 words assciated to "one"          #
# > terms <- findAssociatedTerms(myDtm, "one")                                 #
# > graphDtm(myDtm, c("one", terms), corThresh = 0.07)                         #                                                 #
################################################################################

## ---- exploringvisualizing

# Data Exploration and Visualization
# instructions to install Rgraphviz package
# http://www.bioconductor.org/install/
# source("https://bioconductor.org/biocLite.R"); bioLite("Rgraphviz")
library(Rgraphviz)
library(ggplot2)
library(Matrix)

# returns a sorted descending vector of topN most frequent terms based on low frequency threshold
# INPUT: a document term matrix, low frequency threshold, number of terms to return
# NOTE: Set lowFreq sufficiently large in order to avoid memory errors
findTopNFreqTerms <- function(myDtm, lowFreq=2000, topN=5){
        
        
        print("Finding frequent terms based on lowFreq...")
        myDtm <- myDtm[, findFreqTerms(myDtm, lowfreq = lowFreq)]
        
        print("Convering Dtm to matrix...")
        m <- as.matrix(myDtm)
        
        print("Sorting columns...")
        v <- sort(colSums(m), decreasing=TRUE)
        
        return(v)
}

# Returns a sorted descending vector of topN most correlated terms to the input term
# INPUTS are a document term matrix, the root term, the number of associates to return
# and the correlation threshold
# OUTPUT is a sorted vector of top terms
findAssociatedTerms <- function(myDtm, term, topN = 5, corLimit = 0.025){
        print(paste("Finding terms associated with", term, "..."))
        v <- findAssocs(myDtm, term, corlimit=corLimit)
        u <- unlist(v)
        names(u) <- sapply(names(u), function(x) substring(x, nchar(term) + 2))
        print(u[1:topN])
        return(names(u)[1:topN])
}

# Creates a graph of terms from a dtm
# INPUTS: a document term matrix (or term document matrix), and the subset of terms to graph
# OUTPUT is a network diagram of related terms
# NOTE:  Need to fine tune corThresh to get interesting results, if corThresh is too low
# then all notes are connected
graphDtm <- function(myDtm, myTerms, corThresh = 0) {
        
        print("Plotting network graph of terms...")
        plot(myDtm, terms = myTerms, corThreshold = corThresh)
        
}

# Creates a horizontal bar chart of top terms
# INPUT: is a document term matrix, the low frequency threshold, the number of terms to plot
# and a string for the sample size from which the dtm was created
# OUTPUT: is a bar chart
# NOTE:# set lowFreq sufficiently large in order to avoid memory errors
# NOTE: For 2-gram on 5% sample, this works well barPlotFreq(myDtm, sample.size = meta(myCorpus)$SampleSize, lowFreq = 100)
barPlotFreq <- function(myDtm, lowFreq = 2000, topN = 10, sample.size) {
        
        # get vector of top frequency terms
        v <- findTopNFreqTerms(myDtm, lowFreq, topN)
        
        # convert to a dataframe for plotting
        df <- data.frame(v)
        # convert rownames to first column
        names <- rownames(df)
        rownames(df) <- NULL
        df <- cbind(names,df)
        
        # rename columns and then plot as barchart
        names(df) <- c("terms", "frequency")
        ggplot(df[1:topN, ], aes(x=reorder(terms, frequency),y = frequency)) +
                xlab("terms") + geom_bar(stat = "identity") + coord_flip() +
                ggtitle(paste("Frequency for Top", topN, "Terms For Sample Size =", sample.size))
}
# Example Usage: barPlotFreqDfm(myDfm, sample.size = metadoc(myCorpus, "SampleSize"), topN=15 )
barPlotFreqDfm <- function(myDfm, topN = 10, sample.size) {
        
        # get vector of top frequency terms
        v <- topfeatures(myDfm, topN)
        
        # convert to a dataframe for plotting
        df <- data.frame(v)
        # convert rownames to first column
        names <- rownames(df)
        rownames(df) <- NULL
        df <- cbind(names,df)
        
        # rename columns and then plot as barchart
        names(df) <- c("terms", "frequency")
        ggplot(df[1:topN, ], aes(x=reorder(terms, frequency),y = frequency)) +
                xlab("terms") + geom_bar(stat = "identity") + coord_flip() +
                ggtitle(paste("Frequency for Top", topN, "Terms For Sample Size =", sample.size))
}

# Creates distribution of frequencies
histDistFreq <- function(myDtm, sample.size){
        #v <- findFreqTerms(myDtm)
        #v <- findTopNFreqTerms(myDtm, 2000, 200)
        #myDtm <- myDtm[, findFreqTerms(myDtm, lowfreq = 1)]
        # convert dtm to matrix
        m <- Matrix(myDtm)
        print(m)
        # create a df with terms and frequencies
        v <- rowSums(m)
        print(v)
        # convert to a datafMame for plotting
        print("converting to dataframe...")
        df <- data.frame(v)
        # convert rownames to first column
        #names <- rownames(df)
        #rownames(df) <- NULL
        #df <- cbind(names,df)
        
        # rename columns and then plot as barchart
        names(df) <- "frequency"
        print(head(df))
        print("plotting...")
        # plot using ggplot
        g <- ggplot(df, aes(x=frequency)) +
                stat_bin(bins = 20)
        print(g)
        
}

## ---- end-exploringvisualizing