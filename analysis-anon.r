## Julian Lemmerich
## 24.03.2021
## Code for 1081-bs RA-DL

library(syuzhet)
library(readr)
library(stringr)

basepath <- ("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/20200930_Gesamtkorpus/")

#basepath <- ("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/Testcorpus/") #testcorpus

# for Teilcorpora for split operation
#setwd("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/Teilcorpora/4")
#basepath <- ("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/Teilcorpora/4/")

setwd(basepath)

filelist.texte <- list.files(paste0(basepath, "Texte_csv/"), pattern=NULL) #use substr(f,1,nchar(f)-4) to get without file-ending
filelist.reviews <- list.files(paste0(basepath, "Reviews/"), pattern=NULL) #this is not used
#reading in the filelists for reviews and texts

list.comansentiment <- c()      #List of Anonymous Comment Sentimentvalues
list.comnasentiment <- c()      #List of Non-Anonymous Comment Sentimentvalues 
        
for (f in filelist.reviews) {   
    f.rev <- paste0(basepath, "Reviews/", substr(f,1,nchar(f)-4), "_Reviews.csv")

    #not all texts have a matching review, so the analysis part will only trigger if there are reviews.
    if (file.exists(f.rev)) {
        # comparing anonymous to non-anonymous comments

        reviews.tib.anon.df <- reviews.df[grepl("(anonymer Benutzer)", reviews.df[[2]]),]           #deletes all rows from the df, that are written by anon
        reviews.tib.nonanon.df <- reviews.df[!grepl("(anonymer Benutzer)", reviews.df[[2]]),]       #deletes all rows from the df, that are written by name
        if (length(reviews.tib.anon.df[[1]]) != 0) {                        #this if loop checks if any comments are left after filtering
            for (i.anon in 1:length(reviews.tib.anon.df[[1]])) {
                comment <- reviews.tib.anon.df[[5]][i.anon]
                comment.sentences <- get_sentences(comment)
                comment.sentiment <- get_sentiment(comment.sentences, method="nrc", language="german")
                comment.sentiment.mean <- mean(comment.sentiment)

                list.comansentiment <- c(list.comansentiment, comment.sentiment.mean)
            }
        } else {list.comansentiment <- c(list.comansentiment, "0")}
        if (length(reviews.tib.nonanon.df[[1]]) != 0)               #this if loop checks if any comments are left after filtering
            for (i.nonanon in 1:length(reviews.tib.nonanon.df[[1]])) {
                comment <- reviews.tib.nonanon.df[[5]][i.nonanon]
                comment.sentences <- get_sentences(comment)
                comment.sentiment <- get_sentiment(comment.sentences, method="nrc", language="german")
                comment.sentiment.mean <- mean(comment.sentiment)

                list.comnasentiment <- c(list.comnasentiment, comment.sentiment.mean)
            }
        } else {list.comnasentiment <- c(list.comnasentiment, "0")}

        print("completed anon sentiment")
    }
}