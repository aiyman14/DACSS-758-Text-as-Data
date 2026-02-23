##########################################################
# DACSS 758 SP26
# Week 4 R Lab
# Prof. Irene Morse
##########################################################

# Regex

library(stringr)

tester_string <- "For 3 years I lived at 127 Lincoln Rd. The 1st year was great."

str_detect(tester_string, "Lincoln")
str_extract(tester_string, "Lincoln")
str_extract(tester_string, "road")

# disjuctions
str_extract(tester_string, "for")
str_extract(tester_string, "[Ff]or")
str_extract(tester_string, "[0123456789]")
str_extract_all(tester_string, "[0123456789]")

# ranges
str_extract_all(tester_string, "[0-9]")
str_extract_all(tester_string, "[A-Z]")
str_extract_all(tester_string, "[A-Za-z]")

# negations
str_extract_all(tester_string, "[^A-Za-z]")
str_extract_all(tester_string, "[^A-Za-z\\s]")

# optional characters and counters
str_extract_all(tester_string, "years?")
str_extract_all(tester_string, "lived?")
str_extract_all(tester_string, "[0-9]+")
str_extract_all(tester_string, "[0-9]{3}")

# wild cards
str_extract_all(tester_string, ".")
str_extract_all(tester_string, "\\.")  # escaping
str_extract_all(tester_string, ". years")

# anchors
str_extract_all(tester_string, "^.+years")
str_extract_all(tester_string, "years.+$")

# more operators
str_extract_all(tester_string, "\\d")  # digits
str_extract_all(tester_string, "\\D")  # non-digits
str_extract_all(tester_string, "\\w")  # alpha numeric
str_extract_all(tester_string, "\\W")  # non alpha numeric
str_extract_all(tester_string, "\\s")  # white space
str_extract_all(tester_string, "\\S")  # non white space

# find and replace
str_replace_all(tester_string, "Rd", "Road")
str_replace_all(tester_string, "year(s)?", "month\\1")

##########################################################

# POS Tagging and Dependency Parsing

library(tidyverse)
library(udpipe)
library(here)
library(textplot)
library(ggraph)

# download and load language model
udpipe::udpipe_download_model(language = "english-ewt")
m_eng <- udpipe_load_model(file = here::here("english-ewt-ud-2.5-191206.udpipe"))

# read in text data
prideprej <- readLines("C:/Users/irene/Documents/UMass DACSS/DACSS 758/Data/prideprejudice.txt")
prideprej[1:50]

# annotate using udpipe
annotations <- as.data.frame(udpipe::udpipe_annotate(m_eng, prideprej))
head(annotations, 34)

# check out the most common nouns
annotations %>%
  filter(upos == "NOUN") %>%
  group_by(lemma) %>% 
  summarize(count = n()) %>%
  top_n(n=20) %>%
  arrange(desc(count))

# extract adj-noun pairs
adj_noun_pairs <- subset(annotations, dep_rel == "amod")
pairs <- merge(adj_noun_pairs, annotations, 
               by.x = c("doc_id", "sentence_id", "head_token_id"), 
               by.y = c("doc_id", "sentence_id", "token_id"), 
               suffixes = c("_adj", "_noun"))
results <- data.frame(
  Adjective = pairs$token_adj,
  Noun_Modified = pairs$token_noun,
  Sentence_ID = pairs$sentence_id
)
head(results, 20)

results %>% filter(Noun_Modified == "sister")
results %>% filter(Noun_Modified == "letter")

# dependency parsing
sent <- paste(prideprej[10:11], collapse=" ")
sent
sent_annotated <- as.data.frame(udpipe::udpipe_annotate(m_eng, x = sent))

dplot <- textplot::textplot_dependencyparser(sent_annotated, size = 3) 
# show plot
dplot

##########################################################
