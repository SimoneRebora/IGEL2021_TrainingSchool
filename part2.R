### 1. Enter UDpipe
# https://bnosac.github.io/udpipe/en/

install.packages("udpipe")

library(udpipe)

udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)
x <- udpipe_annotate(udmodel, x = "He was not only a happy man, he was also a happy potato")
x <- as.data.frame(x, detailed = TRUE)
View(x)

# let's try on our own text
text <- readLines("corpus/Doyle_Study_1887.txt")
text <- paste(text, collapse = "\n")

text_annotated <- udpipe_annotate(udmodel, x = text, trace = T)
text_annotated <- as.data.frame(text_annotated, detailed = TRUE)
View(text_annotated)

# now it's time to do sentiment analysis!
text_annotated$sentiment <- 0 # initialize the value
download.file("https://github.com/mjockers/syuzhet/raw/master/R/sysdata.rda", destfile = "syuzhet_dict.RData")
load("syuzhet_dict.RData")

for(i in 1:length(syuzhet_dict$word)){
  
  text_annotated$sentiment[which(text_annotated$lemma == syuzhet_dict$word[i])] <- syuzhet_dict$value[i]
  
}

View(text_annotated)

### -------------- Your turn
# Try SA on another text with another dictionary
# tip: the NRC dictionary has a different format: you will need to convert it
my_sentiment <- "joy"
my_language <- "english"
my_nrc <- nrc[nrc$lang == my_language & nrc$sentiment == my_sentiment,]
### --------------

### 2. Enter Tidyverse
install.packages("tidyverse")
library(tidyverse)

# faster sentiment annotation
text_annotated <- left_join(text_annotated, syuzhet_dict, by = c("lemma" = "word"))

# get overall values per sentence
sentences_annotated <- text_annotated %>%
  group_by(sentence_id) %>%
  summarize(mean_sentiment = mean(sentiment))

### -------------- Your turn
# Try to get mean sentiment with of the "value" column (the one with NAs)
### -------------- 

### 3. Plot sentiment with tidyverse

###function for rolling plot (taken from https://github.com/mjockers/syuzhet/blob/master/R/syuzhet.R)
rolling_plot <- function (raw_values, window = 0.1){
  wdw <- round(length(raw_values) * window)
  rolled <- rescale(zoo::rollmean(raw_values, k = wdw, fill = 0))
  half <- round(wdw/2)
  rolled[1:half] <- NA
  end <- length(rolled) - half
  rolled[end:length(rolled)] <- NA
  return(rolled)
}

# apply rolling function
sentences_annotated$rolled_sentiment <- rolling_plot(sentences_annotated$mean_sentiment)
View(sentences_annotated)

# create index for percentage of book
sentences_annotated$book_percentage <- 1:length(sentences_annotated$sentence_id)/length(sentences_annotated$sentence_id)*100
View(sentences_annotated)

p1 <- ggplot(data = sentences_annotated) +
  geom_line(mapping = aes(x = book_percentage, y = rolled_sentiment)) +
  ggtitle("my sentiment arc") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(minor_breaks = seq(0,100,5), breaks = seq(0,100,10))
p1  

### 4. Multi-dimensional SA (with SentiArt)

# download SentiArt
download.file("https://github.com/matinho13/SentiArt/raw/main/250kSentiArt_EN.xlsx", destfile = "250kSentiArt_EN.xlsx")

install.packages("readxl")
library(readxl)

# read SentiArt as excel file
sentiart <- read_excel("250kSentiArt_EN.xlsx")
View(sentiart)

# note: Sentiart includes values per word (not lemma) in lowercase, so we need to lowercase the tokens in our text and perform the analysis on them
text_annotated$token_lower <- tolower(text_annotated$token)

# use left_join to add multiple annotations at once
text_annotated <- left_join(text_annotated, sentiart, by = c("token_lower" = "word")) 

# possible issue: the annotation of stopwords!
# workaround: use the POS tags to limit the analysis
sentiart_POS_sel <- c("NOUN", "VERB", "ADV", "ADJ")
text_annotated$token_lower[which(!text_annotated$upos %in% sentiart_POS_sel)] <- NA
text_annotated <- left_join(text_annotated, sentiart, by = c("token_lower" = "word")) 

### -------------- Your turn
# Think about a possible rule to manage valence shifters
### -------------- 

### -------------- Tip
# A very nice package that does valence shifters: https://github.com/trinker/sentimentr
# ...but it works just for English: https://github.com/trinker/sentimentr/issues/74