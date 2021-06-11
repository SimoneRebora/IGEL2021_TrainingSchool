### 1. Prepare the dataset for FARM

sentences_annotated <- read.csv("Annotations_file_NEW.csv")

# select just sentences that were actually annotated
admitted_labels <- c("Positive", "Negative", "Neutral")
sentences_annotated <- sentences_annotated[sentences_annotated$manual_annotation %in% admitted_labels,]

# use simple proportion for train/test corpus
train_test_proportion <- 0.8

# shuffle the dataset (to avoid that it is trained just on the beginning and tested just on the end)
sentences_annotated <- sentences_annotated[sample(1:length(sentences_annotated$sentence_id), length(sentences_annotated$sentence_id)),]

# create a dataframe just with text and annotation
all_annotations <- data.frame(text = sentences_annotated$sentence, coarse_label = sentences_annotated$manual_annotation, stringsAsFactors = F)

# check that there are no \t in the text
length(which(grepl(pattern = "\t", x = all_annotations$text)))

# split into train and test
train <- all_annotations[1:trunc(length(all_annotations$text)*train_test_proportion),]
test <- all_annotations[(trunc(length(all_annotations$text)*train_test_proportion)+1):length(all_annotations$text),]

# write all
dir.create("BERT_files")
write.table(train, file = "BERT_files/train.tsv", sep = "\t", row.names = F, quote = F)
write.table(test, file = "BERT_files/test.tsv", sep = "\t", row.names = F, quote = F)

# now everything is ready for transfer learning with BERT!

### -------------- Tip
# Run the experiment with FARM at this link: https://colab.research.google.com/drive/130_7dgVC3VdLBPhiEkGULHmqSlflhmVM