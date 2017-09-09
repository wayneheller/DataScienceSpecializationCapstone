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

graphDtm <- function(myDtm, lowFreq = 2, topN = 50, corthreshold = 0.5) {
        
        m <- as.matrix(myDtm)
        v <- sort(colSums(m), decreasing=TRUE)
        #print(names(v))
        #plot(myDtm, terms = findFreqTerms(myDtm, lowfreq = lowFreq)[1:topN], corThreshold = corthreshold)
        plot(myDtm, terms = names(v)[1:topN], corThreshold = corthreshold)
        }