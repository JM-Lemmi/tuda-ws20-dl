## Julian Lemmerich
## 24.03.2021
## Code for 1081-bs RA-DL

library(syuzhet)
library(readr)
library(stringr)

#basepath <- ("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/20200930_Gesamtkorpus/")

#basepath <- ("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/Testcorpus/") #testcorpus

setwd("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/Teilcorpora/2")
basepath <- ("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/Teilcorpora/2/")

for split operation

#setwd("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/Teilcorpora/4")
#basepath <- ("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Corpus/Teilcorpora/4/")

# Timing the Code
ptm <- proc.time()

## Analysis ##

## Analysis complex ##

# Fragen
#	- Differenzen zwischen Kommentare und Text
#		○ Korpus average vs Kommentare average
#		○ Korpus Texte average difference average
#		○ Textebene average vs Kommentare average
#		○ Textebene Chapter vs chapter-Kommentare average difference
#		○ Kapitelebene average vs Kommentare average
#	- Kommentare mit Kommentarantworten
#		○ Differenz zw Chapter-Sentiment, Kommentarsentiment (non-concatenated), Antwortsentiment
#	- Kommentare die Anonymous sind
#		○ Durchschnittssentiment aller anonymen vs alle nicht-anonyme kommentare
#		○ Differenz zw Chapter-Sentiment & Kommentar bei non-anonymous und Chapter-Sentiment & Kommentar anonymous.


filelist.texte <- list.files(paste0(basepath, "Texte_csv/"), pattern=NULL) #use substr(f,1,nchar(f)-4) to get without ending
filelist.reviews <- list.files(paste0(basepath, "Reviews/"), pattern=NULL) #this is useless
#reading in the filelists for reviews and texts

#var-declaration
list.text <- c()                #List of Textnames
list.chapter <- c()             #List of Chapternumbers
list.chapsentiment <- c()       #List of Chapter Sentimentvalues
list.comsentiment <- c()        #List of Comment Sentimentvalues
#will contain multiples for Text or Chapter, if a text contains multiple chapters or a chapter contains mulitple comments.
#This is to make later aggregation easier
list.comchap <- c()             #List fo Textnames for Comments
list.comtext <- c()             #List of Chapternumbers for Comments

list.combsentiment <- c()       #List of Comment Sentimentvalues for Reviews
list.anssentiment <- c()        #List of Answer Sentimentvalues

list.comansentiment <- c()      #List of Anonymous Comment Sentimentvalues
list.comnasentiment <- c()      #List of Non-Anonymous Comment Sentimentvalues

for (f in filelist.texte) {
    text.tib <- read_csv2(paste0(basepath, "Texte_csv/", f), col_names=TRUE)       #read text csv
    print(paste0("successfully read ", f))
    #since one file can contain multiple chapters, it will have to loop again for each chapter
    for (i.chap in 1:length(text.tib[[3]])) {         #loop over rows in csv to read chapter text
        text <- text.tib[[3]][i.chap]
        text.sentences <- get_sentences(text)
        text.sentiment <- get_sentiment(text.sentences, method="nrc", language="german")
        text.sentiment.mean <- mean(text.sentiment) #get average sentiment of all sentences in the chapter
        # TODO average deviation

        #add all to the list
        list.chapsentiment <- c(list.chapsentiment, text.sentiment.mean)
        list.text <- c(list.text, f)
        list.chapter <- c(list.chapter, text.tib[[2]][i.chap])
        #print(paste0("added sentiment ", text.sentiment.mean, ". Length is now ", length(list.chapsentiment)))
    }

    print("completed text sentiment")

    f.rev <- paste0(basepath, "Reviews/", substr(f,1,nchar(f)-4), "_Reviews.csv")

    #not all texts have a matching review, so the analysis part will only trigger if there are reviews.
    if (file.exists(f.rev)) { #substr ist to get rid of the file-format from the Text and replace it with the _Reviews ending

        print(paste0(f.rev, " exists."))
        reviews.tib <- read_csv2(f.rev, locale(encoding = "Windows-1252"), col_names=TRUE, col_types = NULL)    #read the comments csv
        #The comments csv are encoded in Windows-1252 not in UTF-8
        print(paste0("successfully read reviews ", f.rev))
        #since one file can contain multiple comments, it will have to loop again for each comment

        #convert to dataframe to prepare for filtering
        reviews.df  <-  as.data.frame(matrix(unlist(reviews.tib), nrow=length(unlist(reviews.tib[1]))))

        #filtering chapterless reviews. There are not that many and they fuck up some shit.
        reviews.tib.filtered.df <- reviews.df[!grepl("zur Geschichte", reviews.df[[3]]),]


        if (length(reviews.tib.filtered.df[[1]]) != 0) { #this if is neede, in case there are only text and no chapter reviews
            reviews.chapters <- gsub("zu Kapitel ", "", reviews.tib.filtered.df[[3]])

            #comments are concatenated by tip of Pianzola 2020.
            reviews.concat <- aggregate(reviews.tib.filtered.df[[5]], by=list(Category=reviews.tib.filtered.df[[3]]), function (x) paste(x, collapse=" ")) #collapse instead of sep, so it will not create a vector of vectors
            
            #from https://stackoverflow.com/a/15934100/9397749 and https://stackoverflow.com/a/7202211/9397749

            for (i.rev in 1:length(reviews.concat[[2]])) {
                comment <- reviews.concat[[2]][i.rev]
                comment.sentences <- get_sentences(comment)
                comment.sentiment <- get_sentiment(comment.sentences, method="nrc", language="german")
                comment.sentiment.mean <- mean(comment.sentiment)

                list.comsentiment <- c(list.comsentiment, comment.sentiment.mean)
                list.comchap <- c(list.comchap, reviews.concat[[1]][i.rev])
                list.comtext <- c(list.comtext, f)
            }

            print("completed comment sentiment.")

            #to increase performance, file read operations should only be done once.
            #multithreading would be nice. But its not really a viable option.
            #multithreading could be done manually with only a part of the corpus or similar.

            # comparing answers to the comment and to the chapter
            # they will not be connected to their original text, since the comparison is only meant to be to compare the comment and the review.
            
            reviews.tib.answered.df <- reviews.df[!grepl("0", reviews.df$V6),]       #deletes all rows from the df, that are "0", which is without an answer
            if (length(reviews.tib.answered.df[[1]]) != 0) {                        #this if loop checks if any comments are left after filtering
                for (i.rev in 1:length(reviews.tib.answered.df[[1]])) {
                    comment <- reviews.tib.answered.df[[5]][i.rev]
                    comment.sentences <- get_sentences(comment)
                    comment.sentiment <- get_sentiment(comment.sentences, method="nrc", language="german")
                    comment.sentiment.mean <- mean(comment.sentiment)

                    answer <- reviews.tib.answered.df[[6]][i.rev]
                    answer.sentences <- get_sentences(answer)
                    answer.sentiment <- get_sentiment(answer.sentences, method="nrc", language="german")
                    answer.sentiment.mean <- mean(answer.sentiment)

                    list.combsentiment <- c(list.combsentiment, comment.sentiment.mean)
                    list.anssentiment <- c(list.anssentiment, answer.sentiment.mean)
                }
            }

            print("completed answer sentiment")

        }
    }

}

#dataframe mean sentiment of chapter and mean sentiment of comments to that chapter
chap.dataframe <- data.frame(list.text, list.chapter, list.chapsentiment)
com.dataframe <- data.frame(list.comtext, list.comchap, list.comsentiment)
#dataframe for comparing reviews and answers.
ans.dataframe <- data.frame(list.combsentiment, list.anssentiment)
anon.dataframe <- data.frame(list.comansentiment, list.comnasentiment)

write.csv(chap.dataframe, file="chapter_dataframe.csv")

# Stop the clock
proc.time() - ptm

# TODO console.log for everything, so I know whats going on.