setwd("~/OneDrive/GithubProjects/CTSfromOCR")

vol.number <- "1"
ocr_edition <- "coo.31924054872803_ocr"

path_name <- paste('./data/Vol.-', vol.number, "/", ocr_edition, '/', sep = "")
files <- list.files(path = path_name, pattern = "[.]txt$")
page_numbers <- gsub('[[:alpha:]]', '', files)
files <- paste(path_name, files, sep = "")

library(doParallel)
registerDoParallel(cores = 4)
library(foreach)

data <- foreach(i = files, .combine = rbind) %dopar% {
  list(i, scan(file = i, what = character(), sep = '\n'))
}

GreekOrLatinCharacter <- function(x) {
  bitesize <- length(rawToBits(charToRaw(x)))
  return(bitesize)
}

GreekOrLatin <- function(x) {
  tested_line <- x
  tested_line <- gsub(" ", "", tested_line, fixed = TRUE)
  tested_line <- gsub("[[:punct:]]", "", tested_line)
  all_characters <- unlist(strsplit(tested_line, split = "", fixed = TRUE))
  bit_size <- unlist(lapply(all_characters, GreekOrLatinCharacter))
  pos <- which(bit_size <= 16)
  bit_size <- bit_size[pos]
  if (length(bit_size) == 0) {
    result <- "Uncertain"
  } else {
    mean_bit <- mean(bit_size)
    if (mean_bit >= 12) {
      result <- "Greek"
    } else if (mean_bit <= 9) {
      result <- "Latin"
    } else {
      result <- "Uncertain"
    }
  }
  return(result)
}

all_lines <- unname(unlist(data[,2]))
linenumbers <- vector()
for (i in 1:length(page_numbers)) {
  linenumbers[i] <- length(unlist(data[i,2]))
}
linenumbers <- linenumbers[which(linenumbers > 0)]
identifier <- list()
for (i in 1:length(linenumbers)) {
  identifier[[i]] <- paste(page_numbers[i], 1:linenumbers[i], sep = "")
}
identifier <- as.character(unlist(identifier))

language <- vector()
for (i in 1:length(all_lines)) {
  language[i] <- GreekOrLatin(all_lines[i])
}

page_numbers <- strsplit(identifier, ".", fixed = TRUE)
page_numbers <- sapply(page_numbers, "[", 1)
test <- data.frame(identifier, all_lines, page_numbers, language)

### Split into Greek and Latin texts

GreekText <- test[which(test[,4] == "Greek"), c(1:3)]
LatinText <- test[which(test[,4] == "Latin"), c(1:3)]
UncertainText <- test[which(test[,4] == "Uncertain"), c(1:3)]

### resolve hyphens Greek

pos_hyphen <- grep("-$", GreekText[,2])
linesplit <- vector()
for (i in pos_hyphen) {
  id_hyphen <- as.character(GreekText[i,1])
  id_hyphen <- as.integer(unlist(strsplit(id_hyphen, ".", fixed = TRUE)))
  id_next <- as.character(GreekText[i+1,1])
  id_next <- as.integer(unlist(strsplit(id_next, ".", fixed = TRUE)))
  id_next2 <- as.character(GreekText[i+2,1])
  id_next2 <- as.integer(unlist(strsplit(id_next2, ".", fixed = TRUE)))
  if (is.na(id_next)) {
    id_next <- c(0,0)
  }
  if (is.na(id_next2)) {
    id_next2 <- c(0,0)
  }
  if (dist(t(matrix(c(id_hyphen, id_next), nrow = 2))) == 1) {
    linesplit[which(pos_hyphen == i)] <- TRUE
  } else if ((id_next[1] - id_hyphen[1] == 1) & (dist(t(matrix(c(id_next, id_next2), nrow = 2))) == 1)) {
    linesplit[which(pos_hyphen == i)] <- TRUE
  } else{
    linesplit[which(pos_hyphen == i)] <- FALSE
  }
}
pos_hyphen <- pos_hyphen[which(linesplit == TRUE)]

####### summarise by page

summariser <- levels(factor(GreekText$page_numbers))

GreekTextbyPage <- vector()
for (i in 1:length(summariser)) {
  pagetext <- paste(as.character(GreekText$all_lines)[which(as.character(GreekText$page_numbers) == summariser[i])], collapse = " ")
  # not necessary anymore: pagetext <- gsub("-$", "##hyphenbreak##", pagetext) # if page ends on hyphen replace it
  if (i == 1) {
    next_pagetext <- paste(as.character(GreekText$all_lines)[which(as.character(GreekText$page_numbers) == summariser[i+1])], collapse = " ")
    pagetext <- paste(c(pagetext, unlist(strsplit(next_pagetext, "(?<=[\\.\\;\\:])", perl = TRUE))[1]), collapse = " ")
    GreekTextbyPage[i] <- gsub("- ", "", pagetext, fixed = TRUE) #resolve hyphens within
  } else if (i == length(summariser)) {
    list_pagetext <- unlist(strsplit(pagetext, "(?<=[\\.\\;\\:])", perl = TRUE))
    pagetext <- paste(list_pagetext[2:length(list_pagetext)], collapse = "")
    GreekTextbyPage[i] <- gsub("- ", "", pagetext, fixed = TRUE) #resolve hyphens within
  } else {
    list_pagetext <- unlist(strsplit(pagetext, "(?<=[\\.\\;\\:])", perl = TRUE))
    pagetext <- paste(list_pagetext[2:length(list_pagetext)], collapse = "")
    next_pagetext <- paste(as.character(GreekText$all_lines)[which(as.character(GreekText$page_numbers) == summariser[i+1])], collapse = " ")
    pagetext <- paste(c(pagetext, unlist(strsplit(next_pagetext, "(?<=[\\.\\;\\:])", perl = TRUE))[1]), collapse = " ")
    GreekTextbyPage[i] <- gsub("- ", "", pagetext, fixed = TRUE) #resolve hyphens within
  }
}

identifiers <- paste(vol.number, ':', ocr_edition, ':', unique(as.character(GreekText$page_numbers)), "GREEK", sep = "")
GreekTextbyPage <- gsub("^\\s+", "", GreekTextbyPage)
GreekTextbyPage <- data.frame(identifiers, GreekTextbyPage)

### resolve hyphens Latin

pos_hyphen <- grep("-$", LatinText[,2])
linesplit <- vector()
for (i in pos_hyphen) {
  id_hyphen <- as.character(LatinText[i,1])
  id_hyphen <- as.integer(unlist(strsplit(id_hyphen, ".", fixed = TRUE)))
  id_next <- as.character(LatinText[i+1,1])
  id_next <- as.integer(unlist(strsplit(id_next, ".", fixed = TRUE)))
  id_next2 <- as.character(LatinText[i+2,1])
  id_next2 <- as.integer(unlist(strsplit(id_next2, ".", fixed = TRUE)))
  if (is.na(id_next)) {
    id_next <- c(0,0)
  }
  if (is.na(id_next2)) {
    id_next2 <- c(0,0)
  }
  if (dist(t(matrix(c(id_hyphen, id_next), nrow = 2))) == 1) {
    linesplit[which(pos_hyphen == i)] <- TRUE
  } else if ((id_next[1] - id_hyphen[1] == 1) & (dist(t(matrix(c(id_next, id_next2), nrow = 2))) == 1)) {
    linesplit[which(pos_hyphen == i)] <- TRUE
  } else{
    linesplit[which(pos_hyphen == i)] <- FALSE
  }
}
pos_hyphen <- pos_hyphen[which(linesplit == TRUE)]

####### summarise by page

summariser <- levels(factor(LatinText$page_numbers))

LatinTextPage <- vector()
for (i in 1:length(summariser)) {
  pagetext <- paste(as.character(LatinText$all_lines)[which(as.character(LatinText$page_numbers) == summariser[i])], collapse = " ")
  # not necessary anymore: pagetext <- gsub("-$", "##hyphenbreak##", pagetext) # if page ends on hyphen replace it
  if (i == 1) {
    next_pagetext <- paste(as.character(LatinText$all_lines)[which(as.character(LatinText$page_numbers) == summariser[i+1])], collapse = " ")
    pagetext <- paste(c(pagetext, unlist(strsplit(next_pagetext, "(?<=[\\.\\;\\:])", perl = TRUE))[1]), collapse = " ")
    LatinTextPage[i] <- gsub("- ", "", pagetext, fixed = TRUE) #resolve hyphens within
  } else if (i == length(summariser)) {
    list_pagetext <- unlist(strsplit(pagetext, "(?<=[\\.\\;\\:])", perl = TRUE))
    pagetext <- paste(list_pagetext[2:length(list_pagetext)], collapse = "")
    LatinTextPage[i] <- gsub("- ", "", pagetext, fixed = TRUE) #resolve hyphens within
  } else {
    list_pagetext <- unlist(strsplit(pagetext, "(?<=[\\.\\;\\:])", perl = TRUE))
    pagetext <- paste(list_pagetext[2:length(list_pagetext)], collapse = "")
    next_pagetext <- paste(as.character(LatinText$all_lines)[which(as.character(LatinText$page_numbers) == summariser[i+1])], collapse = " ")
    pagetext <- paste(c(pagetext, unlist(strsplit(next_pagetext, "(?<=[\\.\\;\\:])", perl = TRUE))[1]), collapse = " ")
    LatinTextPage[i] <- gsub("- ", "", pagetext, fixed = TRUE) #resolve hyphens within
  }
}

identifiers <- paste(vol.number, ':', ocr_edition, ':', unique(as.character(LatinText$page_numbers)), "LATIN", sep = "")
LatinTextPage <- gsub("^\\s+", "", LatinTextPage)
LatinTextPage <- data.frame(identifiers, LatinTextPage)

dir.create("TSVout")

filename <- paste("TSVout/", vol.number, "LATIN", ".tsv", sep = "")
write.table(LatinTextPage, file = filename, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)

filename <- paste("TSVout/", vol.number, "GREEK", ".tsv", sep = "")
write.table(GreekTextbyPage, file = filename, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)