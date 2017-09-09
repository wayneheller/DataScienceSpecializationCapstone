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

graphTdm <- function(myTdm, lowFreq = 2, topN = 50, corthreshold = 0.5) {
        plot(myTdm, terms = findFreqTerms(myTdm, lowfreq = lowFreq)[1:topN], corThreshold = corthreshold)
}