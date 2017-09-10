################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
################################################################################

# Data Exploration and Visualization
# instructions to install Rgraphviz package
# http://www.bioconductor.org/install/
# source("https://bioconductor.org/biocLite.R"); bioLite("Rgraphviz")
library(Rgraphviz)
library(ggplot2)

graphDtm <- function(myDtm, lowFreq = 2, topN = 50, corthreshold = 0.5) {
        
        m <- as.matrix(myDtm)
        v <- sort(colSums(m), decreasing=TRUE)
        plot(myDtm, terms = names(v)[1:topN], corThreshold = corthreshold)
}

barPlotFreq <- function(myDtm, topN = 10, sample.size) {
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