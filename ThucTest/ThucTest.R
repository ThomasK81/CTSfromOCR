setwd("~/OneDrive/GithubProjects/CTSfromOCR/ThucTest")
load("backup.RData")

cts.identifier <- as.character(corpus[,1])

pageTOsentences_GREEK <- function(x) {
  unlist(strsplit(as.character(x), "(?<=[\\.\\;\\:])", perl = TRUE))}

corpus_in_sentences <- lapply(as.character(corpus[,2]), pageTOsentences_GREEK)

sentencenumbers <- vector()
for (i in 1:length(cts.identifier)) {
  sentencenumbers[i] <- length(unlist(corpus_in_sentences[i]))
}

sentence_identifier <- list()
for (i in 1:length(sentencenumbers)) {
  sentence_identifier[[i]] <- paste(cts.identifier[i], '.', 1:sentencenumbers[i], sep = "")
}
sentence_identifier <- as.character(unlist(sentence_identifier))

corpus_in_sentences <- data.frame(sentence_identifier, as.character(unlist(corpus_in_sentences)))
tidytext <- gsub("[^[:alpha:]]","", corpus_in_sentences[,2])
CharacterCount <- nchar(tidytext)
corpus_in_sentences <- data.frame(sentence_identifier, corpus_in_sentences[,2], CharacterCount)
colnames(corpus_in_sentences) <- c("SentenceID", "Text", "No.Character")

corpus_in_sentences <- corpus_in_sentences[which(corpus_in_sentences$No.Character > 0),]
corpus_in_sentences <- corpus_in_sentences[which(corpus_in_sentences$No.Character > 3),]

sentence_numbers <- strsplit(as.character(corpus_in_sentences[,1]), "(?<=[\\.\\;\\:])", perl = TRUE)
sentence_numbers <- sapply(sentence_numbers, function(x){paste(x[1:9], collapse = "")})
for (i in 1:length(sentence_numbers)) {
  sentence_numbers[i] <- paste(sentence_numbers[i], i, collapse = "", sep = "")
}


