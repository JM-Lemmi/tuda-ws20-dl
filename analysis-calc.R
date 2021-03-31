## Julian Lemmerich
## 30.03.2021
## Code for 1081-bs RA-DL
## Continuation of analysis.r

library(dplyr)
library(readr)

basepath <- ("C:/Users/julian.lemmerich/OneDrive/User Data/Uni/Semester 7/1081-bs RA DL/Analysis/")

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

## Combining Chapter and Comms

chap.tib.p1 <- read_csv(paste0(basepath, "chapter_dataframe_p1.csv"), col_names=TRUE)
chap.tib.p3 <- read_csv(paste0(basepath, "chapter_dataframe_p3.csv"), col_names=TRUE)
chap.tib.p4 <- read_csv(paste0(basepath, "chapter_dataframe_p4.csv"), col_names=TRUE)
chap.tib.p5 <- read_csv(paste0(basepath, "chapter_dataframe_p5.csv"), col_names=TRUE)
com.tib.p1 <- read_csv(paste0(basepath, "com_dataframe_p1.csv"), col_names=TRUE)
com.tib.p3 <- read_csv(paste0(basepath, "com_dataframe_p3.csv"), col_names=TRUE)
com.tib.p4 <- read_csv(paste0(basepath, "com_dataframe_p4.csv"), col_names=TRUE)
com.tib.p5 <- read_csv(paste0(basepath, "com_dataframe_p5.csv"), col_names=TRUE)

chap.tib <- bind_rows(chap.tib.p1, chap.tib.p3, chap.tib.p4, chap.tib.p5)
com.tib <- bind_rows(com.tib.p1, com.tib.p3, com.tib.p4, com.tib.p5)

chap.df <- as.data.frame(matrix(unlist(chap.tib), nrow=length(unlist(chap.tib[1]))))
com.df <- as.data.frame(matrix(unlist(com.tib), nrow=length(unlist(com.tib[1]))))

com.df[[3]] <- gsub("zu Kapitel ", "", com.df[[3]]) #cleanup chapter column

## creating new dataframes for the analysis
# this part is probably not strictly necessary, but it helps me keep order

#creating df for chapters: story, chapternr, chapsentiment, comsentiment (0)
df.a1 <- chap.df[[2]]
df.a2 <- chap.df[[3]]
df.b <- as.double(chap.df[[4]])
df.c <- rep(0, length(df.a1))
df.chap <- data.frame(df.a1, df.a2, df.b, df.c)

#creating df for coms: story, chapternr, chapsentiment (0), comsentiment
df.a1 <- com.df[[2]]
df.a2 <- com.df[[3]]
df.c <- as.double(com.df[[4]])
df.b <- rep(0, length(df.a1))
df.com <- data.frame(df.a1, df.a2, df.b, df.c)

#concatenating both dataframes
df.combined <- bind_rows(df.chap, df.com)
#aggregating dataframe by chapter and story.
df.combined <- aggregate(list(df.combined[[3]], df.combined[[4]]), by = list(df.combined[[1]], df.combined[[2]]), sum)

# difference between the values. positive value indicates a comments more positive than text.
df.combined[[5]] <- (df.combined[[4]] - df.combined[[3]])
# aggregate with mean for text
df.combined.agg <- aggregate(list(df.combined[[3]], df.combined[[4]], df.combined[[5]]), by = list(df.combined[[1]]), mean)
# mean for whole corpus
mean(df.combined[[3]])
mean(df.combined[[4]])
mean(df.combined[[5]])

mad(df.combined[[3]]) #mean distance
mad(df.combined[[4]])
mad(df.combined[[5]])

# create visualization with x and y dots
plot(x=df.combined[[3]], y=df.combined[[4]], col=rgb(0.58, 0.84, 0.22),
     xlab="plot sentiment", ylab="review sentiment", main="Comparison of Plot and Review Sentiment")


## Answer

ans.tib.p1 <- read_csv(paste0(basepath, "ans_dataframe_p1.csv"), col_names=TRUE)
ans.tib.p3 <- read_csv(paste0(basepath, "ans_dataframe_p3.csv"), col_names=TRUE)
ans.tib.p4 <- read_csv(paste0(basepath, "ans_dataframe_p4.csv"), col_names=TRUE)
ans.tib.p5 <- read_csv(paste0(basepath, "ans_dataframe_p5.csv"), col_names=TRUE)

ans.tib <- bind_rows(ans.tib.p1, ans.tib.p3, ans.tib.p4, ans.tib.p5)
ans.df <- as.data.frame(matrix(unlist(ans.tib), nrow=length(unlist(ans.tib[1]))))
# calculate difference for each
ans.df[[4]] <- (ans.df[[3]] - ans.df[[2]])
# means
mean(ans.df[[2]])
mean(ans.df[[3]]) #mean of all answers
mean(ans.df[[4]]) #mean of difference #if this is positive, answers are more positive than their respective comments

mad(ans.df[[2]]) #mean distance
mad(ans.df[[3]])
mad(ans.df[[4]])


# create visualization with x and y dots
plot(x=ans.df[[3]], y=ans.df[[4]], col=rgb(0.58, 0.84, 0.22),
     xlab="review sentiment", ylab="answer sentiment", main="Comparison of Review and Answer Sentiment")

### saving

write.csv(df.combined, file="combined_dataframe.csv")
write.csv(ans.df, file="ans_combined_dataframe.csv")

write.csv(df.combined[[1]], file="textlist.csv")
