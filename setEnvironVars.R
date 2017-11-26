################################################################################
# Coursera|Johns Hopkins Data Science Science Specializaiton|Capstone Project  #
# October - December 2017                                                      #
# Wayne Heller                                                                 #
#                                                                              #
# Script to set environment variables                                          #
# Must be run first before any other functions                                 #
#                                                                              #
################################################################################

# path and other environment variable names
datadir <- file.path(getwd(), "data")
sampledir <- file.path(datadir, "trainingdata")
sampledirarchive <- file.path(datadir, "sampleddatearchive")
metadatadir <- file.path(datadir, "metadata")
devdatadir <- file.path(datadir, "devdata")
testingdatadir <- file.path(datadir, "testingdata")
modellibrarydir <- file.path(getwd(), "modellibrary")
modeldocfilename <- 'model_documenation.csv'



source('testModels.R')
source('predictprobabilities.R')
source('loadandcleandata.R')
source('shiny/queryModel.R')