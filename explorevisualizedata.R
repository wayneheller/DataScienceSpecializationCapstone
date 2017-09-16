################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# To create a bar plot of top 10 words
# > barPlotFreq(myDtm, topN = 10, meta(myCorpus)$SampleSize)
#
# To create a graph network diagram of top 5 words
# > 
################################################################################

# Data Exploration and Visualization
# instructions to install Rgraphviz package
# http://www.bioconductor.org/install/
# source("https://bioconductor.org/biocLite.R"); bioLite("Rgraphviz")
library(Rgraphviz)
library(ggplot2)

# returns a sorted descending vector of topN most frequent terms based on low frequency threshold
findTopNFreqTerms <- function(myDtm, lowFreq=2, topN=5){
        # must set lowFreq sufficiently large in order to avoid memory errors
        
        print("Finding frequent terms based on lowFreq...")
        myDtm <- myDtm[, findFreqTerms(myDtm, lowfreq = lowFreq)]
        
        print("Convering Dtm to matrix...")
        m <- as.matrix(myDtm)
        
        print("Sorting columns...")
        v <- sort(colSums(m), decreasing=TRUE)
        
        return(v)
}

# returns a sorted descending vector of topN most correlated terms to the input term
findAssociatedTerms <- function(myDtm, term, topN = 5, corLimit = 0.25){
        print(paste("Finding terms associated with", term, "..."))
        v <- findAssocs(myDtm, term, corlimit=0.025)
        u <- unlist(v)
        names(u) <- sapply(names(u), function(x) substring(x, nchar(term) + 2))
        print(u[1:topN])
        return(names(u)[1:topN])
}

# creates a graph of terms from a dtm

graphDtm <- function(myDtm, myTerms) {
        
        print("Plotting...")
        plot(myDtm, terms = myTerms)
        
}

graphTdm <- function(myTdm, lowFreq = 2, topN = 5, corthreshold = 0.0) {
        # must set lowFreq sufficiently large in order to avoid memory errors
        
        print("Finding frequent terms based on lowFreq...")
        myTdm <- myTdm[findFreqTerms(myTdm, lowfreq = lowFreq) , ]
        
        print("Convering Dtm to matrix...")
        m <- as.matrix(myTdm)
        print("Sorting rows")
        v <- sort(rowSums(m), decreasing=TRUE)
        print("Plotting...")
        plot(myTdm, term = names(v)[1:topN], corThreshold = corthreshold)
        
        print(v[1:topN])
}

barPlotFreq <- function(myDtm, lowFreq = 2, topN = 10, sample.size) {
        # must set lowFreq sufficiently large in order to avoid memory errors
        
        print("Finding frequent terms based on lowFreq...")
        myDtm <- myDtm[, findFreqTerms(myDtm, lowfreq = lowFreq)]
        
        # sort terms by decreasing frequency
        m <- as.matrix(myDtm)
        v <- sort(colSums(m), decreasing=TRUE)
        
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