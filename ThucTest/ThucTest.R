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

corpus_in_sentences[,1] <- sentence_numbers

############################### Old stuff just copy paste

## libraries needed

library(tm)
library(XML)
library(RCurl)
library(plyr)
library(lda)
library(LDAvis)
library(compiler)

## optional

library(RColorBrewer)
library(rCharts)
library(d3heatmap)


## User settings:
K <- 12
G <- 5000
alpha <- 0.02
eta <- 0.02
seed <- 37
terms_shown <- 40
swLatin <- TRUE
swEnglish <- FALSE
swGreek <- FALSE
swAdditional <- TRUE
language <- "Greek" # (Persian, Arabic, Latin)
requestURN <- "urn:cts:greekLit:tlg0003.tlg001" # urn:cts:latinLit:phi0448.phi001
capabilities_URL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetCapabilities"
baseURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetPassage&urn="
reffURL <- "http://www.perseus.tufts.edu/hopper/CTS?request=GetValidReff&urn="
morpheusURL <- "http://services.perseids.org/bsp/morphologyservice/analysis/word?word="
searchterms <- ""

## read in some stopwords:

stopwords_english <- stopwords("SMART")
stopwords_latin <- c("ab", "ac", "ad", "adhic", "aliqui", "aliquis", "an", "ante", "apud", "at", "atque", "aut", "autem", "cum", "cur", "de", "deinde", "dum", "ego", "enim", "ergo", "es", "est", "et", "etiam", "etsi", "ex", "fio", "haud", "hic", "iam", "idem", "igitur", "ille", "in", "infra", "inter", "interim", "ipse", "is", "ita", "magis", "modo", "mox", "nam", "ne", "nec", "necque", "neque", "nisi", "non", "nos", "o", "ob", "per", "possum", "post", "pro", "quae", "quam", "quare", "qui", "quia", "quicumque", "quidem", "quilibet", "quis", "quisnam", "quisquam", "quisque", "quisquis", "quo", "quoniam", "sed", "si", "sic", "sive", "sub", "sui", "sum", "super", "suus", "tam", "tamen", "trans", "tu", "tum", "ubi", "uel", "uero", "ut", "t", "cos2", "coepio", "sum", "edo")
stopwords_greek <- c("μή", "ἑαυτοῦ", "ἄν", "ἀλλ’", "ἀλλά", "ἄλλοσ", "ἀπό", "ἄρα", "αὐτόσ", "δ’", "δέ", "δή", "διά", "δαί", "δαίσ", "ἔτι", "ἐγώ", "ἐκ", "ἐμόσ", "ἐν", "ἐπί", "εἰ", "εἰμί", "εἴμι", "εἰσ", "γάρ", "γε", "γα^", "ἡ", "ἤ", "καί", "κατά", "μέν", "μετά", "μή", "ὁ", "ὅδε", "ὅσ", "ὅστισ", "ὅτι", "οὕτωσ", "οὗτοσ", "οὔτε", "οὖν", "οὐδείσ", "οἱ", "οὐ", "οὐδέ", "οὐκ", "περί", "πρόσ", "σύ", "σύν", "τά", "τε", "τήν", "τῆσ", "τῇ", "τι", "τί", "τισ", "τίσ", "τό", "τοί", "τοιοῦτοσ", "τόν", "τούσ", "τοῦ", "τῶν", "τῷ", "ὑμόσ", "ὑπέρ", "ὑπό", "ὡσ", "ὦ", "ὥστε", "ἐάν", "παρά", "σόσ")
# stopwords_arabic and stopwords_persian are currently based on frequency only. I welcome pointers to stopword lists for Classical Arabic and Persian

## Decide which set of stopwords

stop_words <- stopwords_greek

# Enable JIT-compiling

enableJIT(3)

### Functions:

## Take the terms from a word list and put them into a format needed by the LDA package

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))}

## Replace word-token with lemmata-vector

lemmatiser <- function(x){
  lemmatised <- stem_dictionary[[x]]
  return(lemmatised)}

## Choose lemma from each lemmata-vector based on frequency of that lemma in the research corpus

choose_lemma <- function(x){
  lemma <- names(which(NumberOccurrences[x]==max(NumberOccurrences[x])))
  if (length(lemma)==1) {
    return(lemma)}
  else {
    return (x[1])
  }
}

### parsing the XML in R is a bit of a pain. I am happy for suggestions to make this more efficient!

XMLminer <- function(x){
  xname <- xmlName(x)
  xattrs <- xmlAttrs(x)
  c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}

XMLpassage1 <-function(xdata){
  result <- xmlParse(xdata)
  as.data.frame(t(xpathSApply(result, "//*/tei:body", XMLminer)), stringsAsFactors = FALSE)}

XMLpassage2 <-function(xdata){
  result <- xmlParse(xdata)
  temp.df <- as.data.frame(t(xpathSApply(result, "//*/hdwd", XMLminer)), stringsAsFactors = FALSE)
  as.vector(temp.df[['text']])}

### parsing function: Uses Perseids morphology API to retrive vector of lemmata
### two drawbacks: 1. internet problems would not break code (not anymore), but lead to no lemma returned for requested.
### Uses US server in Boston. Quick in Boston very slow from Europe
### Possible solutions: Requesting already parsed data for edition, thus reducing the API requests from n=number of forms in a corpus to n=1.

parsing <- function(x){
  word_form <- x
  URL <- paste(morpheusURL, word_form, "&lang=grc&engine=morpheusgrc", sep = "")
  message(round((match(word_form, corpus_words)-1)/length(corpus_words)*100, digits=2), "% processed. Checking ", x," now.")
  
  URLcontent <- tryCatch({
    getURLContent(URL)}, 
    error = function(err)
    {tryCatch({
      Sys.sleep(0.1)
      message("Try once more")
      getURLContent(URL)},
      error = function(err)
      {message("Return original value: ", word_form)
        return(word_form)
      })
    })
  if (URLcontent == "ServerError") {
    lemma <- x
    message(x, " is ", lemma)
    return(lemma)}
  else {
    lemma <- if (is.null(XMLpassage2(URLcontent)) == TRUE) {
      lemma <- x
      message(x, " is ", lemma)
      return(lemma)}
    else {lemma <- tryCatch({XMLpassage2(URLcontent)},
                            error = function(err) {
                              message(x, " not found. Return original value.")
                              lemma <- "NotFound1"
                              message(x, " is ", lemma)
                              return(lemma)})
    
    lemma <- gsub("[0-9]", "", lemma)
    lemma <- tolower(lemma)
    lemma <- unique(lemma)
    if (nchar(lemma) == 0) {
      lemma <- x
      message(x, " is ", lemma)
      return(lemma)}
    else {
      message(x, " is ", lemma)
      return(lemma)
    }
    }
  }
}

### quick helper functions for vector splitting

first_element <- function(x){
  first_element <- head(x, n=1)
  return(first_element)}

last_element <- function(x){
  last_element <- tail(x, n=1)
  return(last_element)}

### find out how topic similarity of citable units
### comparing the mean deviation of theta-values for each topic

is_similar <- function(x) {
  check <- all.equal(theta.frame[which(theta.frame[,1] == first_element(unlist(x))),], theta.frame[which(theta.frame[,1] == last_element(unlist(x))),]) # comparing with all.equal
  result <- mean(as.numeric(sub(".*?difference: (.*?)", "\\1", check)[3:length(check)])) 
  return(result)
} # produces NA if compared with itself

### building test matrix to compare a sentence with all other sentences in the corpus

build_test <- function(x){
  test_cases <- output_names [! output_names %in% x]
  first_column <- rep(x, length(test_cases))
  test_matrix <- matrix(nrow=length(test_cases), ncol = 2)
  test_matrix[,1] <- first_column
  test_matrix[,2] <- test_cases
  return(test_matrix)
}

## Mark up known vocabulary with Markdown tags

emph_function <- function(x){
  replacement <- paste("**", text_vector[x], "**", sep="")
  result <- c(text_vector[x], replacement) 
  return(result)
}

### Import corpus from CTS repository

research_corpus <- as.character(corpus_in_sentences[,2])
output_names <- as.character(corpus_in_sentences[,1])

### pre-processing:

research_corpus <- tolower(research_corpus)  # force to lowercase
research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
# research_corpus <- gsub("v", "u", research_corpus) # normalise to 'u'
# research_corpus <- gsub("j", "i", research_corpus) # normalise to 'i'

research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers


## tokenize on space and output as a list:
doc.list <- strsplit(research_corpus, "[[:space:]]+")
corpus_words <- unique(unlist(doc.list))
corpus_words <- sort(corpus_words)

NumberOfForms <- max(unique(sapply(stem_dictionary, length)))
number_lemmata <- sapply(stem_dictionary, length)

## correcting
t1 <- Sys.time()

temp <- strsplit(research_corpus, " ")
temp_correct <- sapply(research_corpus,function(x) NULL)
for (i in 1:length(temp)) {
  temp_correct[[i]] <- sapply(temp[[i]], lemmatiser) 
}
NumberOccurrences <- table(unlist(temp_correct))

build_corrected_corpus <- function(x) {
  temp_corrected <- sapply(temp_correct[[x]], choose_lemma)
  return(temp_corrected)
}

corrected_corpus <- sapply(research_corpus, build_corrected_corpus)

###

rm(temp)
rm(temp_correct)
rm(temp_corrected)

for (i in 1:length(corrected_corpus)) {
  corrected_corpus[i] <- paste(unlist(corrected_corpus[[i]]), collapse=" ")
}

research_corpus <- as.character(unlist(corrected_corpus))

# Save corrected corpus to disk

temp.corpus <- matrix(nrow=length(research_corpus), ncol = 2)
temp.corpus[, 1] <- output_names
temp.corpus[, 2] <- research_corpus
colnames(temp.corpus) <- c("identifier", "text")
corpus_parsed <- temp.corpus
rm(temp.corpus)
write.table(corpus_parsed, file = 'corpus_parsed.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

### Compare length of corpus and corpus_parsed

length_corpus <- length(corpus_in_sentences)/2
test_corpus_length <- vector()
for (i in 1:length_corpus){
  test_corpus_length[i] <- length(unlist(strsplit(as.character(unlist(corpus_parsed[i,2])), "[[:space:]]+")[1])) == length(unlist(strsplit(as.character(unlist(corpus_in_sentences[i,2])), "[[:space:]]+")[1]))
}
table_corpus_length <- table(test_corpus_length)
bug_report <- which(test_corpus_length == FALSE)
difference <- sapply(bug_report, function(x) {length(unlist(strsplit(corpus[x,2], "[[:space:]]+"))) - length(unlist(strsplit(corpus_parsed[x,2], "[[:space:]]+")))})
# Split to word level

doc.list <- strsplit(research_corpus, "[[:space:]]+")
t2 <- Sys.time()

confidence_placement <- 100 - 100 * length(bug_report)/(length(corpus)/2)

### Prepare Topic-modelling 

## compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

## compute additional stop_words:
add_stop_words <- as.data.frame(term.table)
add_stop_words <- row.names(as.data.frame(add_stop_words[1:10,]))

stop_words <- c(stop_words, add_stop_words)
stop_words <- unique(stop_words)

## remove terms that are stop words or occur fewer than "occurenses" times:
occurences <- 3
del <- names(term.table) %in% stop_words | term.table < occurences
term.table <- term.table[!del]
vocab <- names(term.table)

## now put the documents into the format required by the lda package:

documents <- lapply(doc.list, get.terms)

## Compute some statistics related to the data set:
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus

## Fit the model:
set.seed(seed)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
modelling_time <- t2 - t1

## Visualize
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

research_corpusAbstracts <- list(phi = phi,
                                 theta = theta,
                                 doc.length = doc.length,
                                 vocab = vocab,
                                 term.frequency = term.frequency)

## create the JSON object to feed the visualization:
json <- createJSON(phi = research_corpusAbstracts$phi, 
                   theta = research_corpusAbstracts$theta, 
                   doc.length = research_corpusAbstracts$doc.length, 
                   vocab = research_corpusAbstracts$vocab, 
                   term.frequency = research_corpusAbstracts$term.frequency,
                   R = terms_shown)

## Visualise and start browser
serVis(json, out.dir = 'Greek_vis', open.browser = FALSE)

## get the tables

dir.create("Greek_tab")

# names(head(sort(phi.frame[,1], decreasing = TRUE)))

## get topic-term distributions and export as csv
phi.t <- t(phi)
phi.t.df <- data.frame(matrix(nrow=length(phi.t[, 1]), ncol = K+1))
phi.t.df[, 1] <- names(phi.t[,1])
for (i in 1:K){
  phi.t.df[, i+1] <- phi.t[, i]
}
phicolnames <- vector(mode="character", length=K+1)
phicolnames[1] <- "term"
for (i in 1:K){
  phicolnames[i+1] <- paste(head(phi.t.df[order(phi.t.df[,i+1],decreasing=TRUE),], n=7)[,1], sep="", collapse="_")
}
colnames(phi.t.df) <- phicolnames
write.table(phi.t.df, file = 'Greek_tab/phi.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

## get document-topic distributions and export as csv
theta.frame <- data.frame(matrix(nrow=length(theta[,1]), ncol = K+1))
theta.frame[, 1] <- output_names
for (i in 1:K){
  theta.frame[, i+1] <- theta[, i]
}
thetacolnames <- phicolnames
thetacolnames[1] <- "identifier"
colnames(theta.frame) <- thetacolnames
write.table(theta.frame, file = 'Greek_tab/theta.csv', append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

