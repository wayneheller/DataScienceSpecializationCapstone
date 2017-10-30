# Quiz 1 Workspace
# Question #3
file.news <- "en_US.news.txt"
file.blogs <- "en_US.blogs.txt"
file.twitter <- "en_US.twitter.txt"

quizQuestion3 <- function(sourcefilename) {
        longestLine <-0
        conn <- file(sourcefilename, open = "rb") # open connection to the file using rb to deal with Cntl-Z in news file
        while (length(oneLine <- readLines(conn, n = 1, warn = TRUE, skipNul = TRUE)) > 0) 
        {
                longestLine <- max(nchar(oneLine), longestLine)
        }

        close(conn)
        print(longestLine)
}

# quizQuestion3(file.path(datadir, file.blogs))

# quizQuestion3(file.path(datadir, file.news))

quizQuestion4and5and6 <- function(sourcefilename) {
        conn <- file(sourcefilename, open = "rb")
        x <- readLines(conn, warn = TRUE, skipNul = TRUE)
        close(conn)
        # print(paste("Testing", sourcefilename, "for threashold", as.character(thresholdCount), "..."))
        love <- grep("love", x)
        hate <- grep("hate", x)
        
        love.sum <- length(love)
        hate.sum <- length(hate)
        
        print(love.sum / hate.sum)
        
        biostats <- grepl("biostats", x)
        print(x[biostats])
        
        phrase <- "A computer once beat me at chess, but it was no match for me at kickboxing"
        chess <- grepl(phrase, x, fixed = TRUE)
        print(length(x[chess]))
        
}

 quizQuestion4and5and6(file.path(datadir, file.twitter))



