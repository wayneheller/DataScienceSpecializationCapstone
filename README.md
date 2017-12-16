# Data Science Specialization Capstone
Coursera | Johns Hopkins University | Capstone Project | Data Science Specialization

## About this Assignment:
This course is the final project in a series of 10 courses in this specialization and is meant to demonstrate skills acquired over the 9 previous course and new learning -- Natural Language Processing (NLP)

*"This course will start with the basics, analyzing a large corpus of text documents to discover the structure in the data and how words are put together. It will cover cleaning and analyzing text data, then building and sampling from a predictive text model. Finally, you will use the knowledge you gained in data products to build a predictive text product you can show off to your family, friends, and potential employers."*

## Final Deliverables
[Shiny Application](https://rougeone.shinyapps.io/nextword/)  
[Pitch Deck](http://rpubs.com/wgheller/333875)  
Based on what I observed peer reviewing other learners' submissions, mine is in the zone of average, and there are better examples with superior performance, features and ui/presentation. I'm also not a software developer by trade, so my coding style and approach may not be anything to emulate.  That said, I received 10 of 11 points on the final submission.

## Advice for Future Students
As you are now painfully aware, the capstone project is very different than all the classes leading up to it.
Very little information is provided in the Coursera materials in terms of how to go about solving the word 
prediction task.  If you are just staring out and are scratching your head, read mentor Len Greski's post 
[Capstone Strategy](https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-simplifiedApproach.md) and take it to heart.  Here is some additional advice based on my experience:  

+ I wasted too much time on the tm package and should have switched to quanteda earlier.  It wasn't until I could not overcome memory issues that I bit the bullet and learned to approach with quanteda library.  Not only is it faster, less memory intensive, it is actually easier to work with.  

+ I regret combining the 3 data files into 1 and losing the connection to the source (twitter, news, blogs).  My sampling routine, samples from each source and creates 1 sample file from which I load into a corpus object. It would have been better to retain the source and add it as meta data.  That way I could have computed accuracy against each source to see if there where notable differences.  

+ The learning curve on this project was not linear for me.  I spent a lot of time in the early week's trying and failing and getting my head wrapped around the task.  Once I had a working shiny application and prediction model, iterating on it went much faster.  I intentially delayed taking the quizzes until my model was more robust.  In prior courses, I was religious about staying on track week by week with the assignments.  For this one, a more fluid approach to the work, in my opinion, works better.  Get to a working application as fast as you can, then take the quizzes after you've had a chance to build and tune your model.  

+ To help you with the quizzes, modify your prediction routine to take in the phrase and multiple choice options on the quiz questions and return the probabilities for each choice/option. 

+ I watched Dan Jurafski's video series on NLP about 2 dozen times, no kidding.  They do not teach you the how but they are great for helping you grok the concepts. https://www.youtube.com/watch?v=s3kKlUBa3b0

+ I did not find any good concreate textbook examples of Modified Keneser-Nye smoothing for trigrams or higher and even the ones for bigrams did not seem complete.  See links below.

## Sources / References
+ Profanity List - https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/  Was looking to download directly from Google but have not yet found that link.  

+ Modified Kneser-Nye concept:https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf  See pages 16 and 17.

* Some textbook examples of smoothing:
     + https://hpi.de/fileadmin/user_upload/fachgebiete/plattner/teaching/MachineTranslation/MT2015/MT11_LanguageModels.pdf
     + http://www.cis.uni-muenchen.de/~davidk/ap/lectures/13.pdf
     + http://jon.dehdari.org/teaching/uds/lt1/ngram_lms.pdf




