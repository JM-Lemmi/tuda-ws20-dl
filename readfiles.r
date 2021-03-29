### Aussortierter Code aus analysis.r

## Descrambling Text ##

text.tib <- read_csv2(paste0(basepath, "Texte_csv/4a1d83f90000df9b06900fa0_Die-stärkste-Prinzessin-der-magischen-Dimension.csv"), col_names=TRUE) #need read_csv2, because it uses ";" as delimiter
#the csv have two columns of chapternumbering and then 1 column filles with the text.
#read_csv2 can handle UTF-8

text.review.tib <- read_csv2(paste0(basepath, "Reviews/4a1d83f90000df9b06900fa0_Die-stärkste-Prinzessin-der-magischen-Dimension_Reviews.csv"), col_names=TRUE)
# x1, reviewer_username, chapter_reviewed, review_date, review_text, review_reply
#problem with UTF-8?

# For a single .txt
text <- read_file("4a1d83f90000df9b06900fa0_Die-stärkste-Prinzessin-der-magischen-Dimension.txt")

text.sentences <- get_sentences(text)
text.sentiment <- get_sentiment(text.sentences, method="nrc", language="german")
#might have problems with UTF-8, but seems alright so far.

# dataframe creation
text.sentiment.df <- data.frame(text.sentences, text.sentiment)

# dataframe aggregation
text.df.aggregate <- aggregate(text.sentiment.df$text.sentiment, by=list(Category=text.df$text.sentences), FUN=sum)