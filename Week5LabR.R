##########################################################
# DACSS 758 SP26
# Week 5 R Lab
# Prof. Irene Morse
##########################################################

# very useful reference: https://tutorials.quanteda.io/

library("quanteda")
library("quanteda.textstats")
library("quanteda.textplots")
library("stringr")
library("ggplot2")
library("RColorBrewer")

# read in text data
prideprej <- readLines("C:/Users/irene/Documents/UMass DACSS/DACSS 758/Data/prideprejudice.txt")
prideprej[1:50]

prpr_corpus <- corpus(prideprej)

# no pre-processing
prpr_tokens <- tokens(prpr_corpus)
sum(ntoken(prpr_tokens))
prpr_tokens[30]

prpr_dfm <- dfm(prpr_tokens)
prpr_dfm[1:5, 1:5]
prpr_dfm <- dfm(prpr_tokens, tolower = FALSE)
prpr_dfm[1:5, 1:5]

textplot_wordcloud(prpr_dfm)
most_common <- textstat_frequency(prpr_dfm, n=50)
ggplot(most_common, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() + 
  labs(x = "Frequency", y = "Feature")

# add pre-processing
prpr_tokens <- tokens(prpr_corpus, 
                    #remove_numbers = TRUE,  
                    remove_punct = TRUE,
                    remove_url = TRUE,
                    remove_symbols = TRUE)
sum(ntoken(prpr_tokens))
prpr_tokens[30]

# customize a bit
str_remove_all("_You_", "_")  # test run
prideprej <- str_remove_all(prideprej, "_")  # run on full data
prpr_corpus <- corpus(prideprej)
prpr_tokens <- tokens(prpr_corpus, 
                      #remove_numbers = TRUE,  
                      remove_punct = TRUE,
                      remove_url = TRUE,
                      remove_symbols = TRUE)
# remove stop words
stopwords("en")
stopwords("es")
stopwords("zh")
# for different options look into tidytext::stop_words
prpr_tokens <- tokens_select(prpr_tokens, 
                             stopwords("en"), 
                             selection = "remove" )
sum(ntoken(prpr_tokens))
prpr_tokens[30]

prpr_dfm <- dfm(prpr_tokens)

textplot_wordcloud(prpr_dfm)
most_common <- textstat_frequency(prpr_dfm, n=50)
ggplot(most_common, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() + 
  labs(x = "Frequency", y = "Feature")

# stemming
# Martin Porter's stemming algorithm
prpr_tokens_stemmed <- tokens_wordstem(prpr_tokens)
prpr_tokens_stemmed[30]

prpr_dfm_stemmed <- dfm(prpr_tokens_stemmed)

textplot_wordcloud(prpr_dfm_stemmed)
most_common <- textstat_frequency(prpr_dfm_stemmed, n=50)
ggplot(most_common, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() + 
  labs(x = "Frequency", y = "Feature")

# customize some more
# create bigrams for Mr. Blah, Mrs. Blah, Miss Blah
prpr_tokens <- tokens_compound(prpr_tokens, pattern = phrase("Mr *"))
prpr_tokens <- tokens_compound(prpr_tokens, pattern = phrase("Mrs *"))
prpr_tokens <- tokens_compound(prpr_tokens, pattern = phrase("Miss *"))
prpr_dfm <- dfm(prpr_tokens)

textplot_wordcloud(prpr_dfm)
most_common <- textstat_frequency(prpr_dfm, n=50)
ggplot(most_common, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_point() + 
  labs(x = "Frequency", y = "Feature")

# customizing wordclouds
set.seed(2026) # set seed for reproducibility
textplot_wordcloud(prpr_dfm,
                   min_size = 0.5,
                   max_size = 4,
                   min_count = 3,
                   max_words = 200,
                   color = brewer.pal(8, "Dark2"))
# color palettes: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html

# key words in context
fam_df <- kwic(prpr_tokens, "family", window = 5)
View(fam_df)

# readability
# useful reference: https://quanteda.io/reference/textstat_readability.html
readability <- textstat_readability(prpr_corpus, 
                                    measure = c("Flesch", "Flesch.Kincaid"))
View(readability)
mean(readability$Flesch, na.rm = TRUE)
mean(readability$Flesch.Kincaid, na.rm = TRUE)

readability <- textstat_readability(paste(prpr_corpus, collapse = " "), 
                                    measure = c("Flesch", "Flesch.Kincaid"))
View(readability)

# compare with US presidents' inauguration speeches
# quanteda included data
data(package = "quanteda")
head(data_corpus_inaugural)
readability2 <- textstat_readability(data_corpus_inaugural, 
                                    measure = c("Flesch", "Flesch.Kincaid"))
mean(readability2$Flesch)
mean(readability2$Flesch.Kincaid)
# modern speeches
readability3 <- textstat_readability(data_corpus_inaugural[49:60], 
                                     measure = c("Flesch", "Flesch.Kincaid"))
mean(readability3$Flesch)
mean(readability3$Flesch.Kincaid)

##########################################################
