# 1. Prepare text(s) for manual annotation

library(udpipe)
library(tidyverse)

udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)

text <- readLines("corpus/Doyle_Study_1887.txt") 
text <- paste(text, collapse = "\n")

text_annotated <- udpipe_annotate(udmodel, x = text, trace = T)
text_annotated <- as.data.frame(text_annotated, detailed = TRUE)

sentiart <- read.csv("resources/SentiArt.csv", stringsAsFactors = F)
text_annotated$token_lower <- tolower(text_annotated$token)
sentiart_POS_sel <- c("NOUN", "VERB", "ADV", "ADJ")
text_annotated$token_lower[which(!text_annotated$upos %in% sentiart_POS_sel)] <- NA
text_annotated <- left_join(text_annotated, sentiart, by = c("token_lower" = "word")) 

# get overall values per sentence
sentences_annotated <- text_annotated %>%
  group_by(sentence_id, sentence) %>%
  summarize(mean_sentiment = mean(AAPz, na.rm = T))

sentences_annotated$mean_sentiment[is.na(sentences_annotated$mean_sentiment)] <- 0

neutral_threshold <- 0.15

sentences_annotated$valence <- "Neutral"
sentences_annotated$valence[sentences_annotated$mean_sentiment < -neutral_threshold] <- "Negative"
sentences_annotated$valence[sentences_annotated$mean_sentiment > neutral_threshold] <- "Positive"

table(sentences_annotated$valence)

annotators <- c("Simone", "Annotator2", "Annotator3")

sentences_annotated$manual_annotation <- NA
sentences_annotated$annotator <- rep(annotators, each = floor(length(sentences_annotated$sentence_id)/length(annotators)))[1:length(sentences_annotated$sentence_id)]
sentences_annotated$comment <- ""

write.csv(sentences_annotated, file = "Annotations_file.csv", row.names = F)

### 2. Compare annotations

install.packages("irr")
library(irr)

sentences_annotated <- read.csv("Annotations_file_NEW.csv")

# select just sentences that were actually annotated
admitted_labels <- c("Positive", "Negative", "Neutral")
sentences_annotated <- sentences_annotated[sentences_annotated$manual_annotation %in% admitted_labels,]

# find overall agreement between human(s) and computer

annotation_matrix <- matrix(c(sentences_annotated$valence, sentences_annotated$manual_annotation), ncol = 2)

kappa2(annotation_matrix)

# Tip ----------------------
# another possible metrics is Krippendorff's alpha: kripp.alpha(t(annotation_matrix), method = "nominal")

# find agreement between each human and the computer

for(annotator in annotators){
  
  tmp_df <- sentences_annotated[sentences_annotated$annotator == annotator,]
  
  print(annotator)
  
  annotation_matrix <- matrix(c(tmp_df$valence, tmp_df$manual_annotation), ncol = 2)
  
  print(kappa2(annotation_matrix))
  
}

