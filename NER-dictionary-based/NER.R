

#libraries
library(tm)
library(NLP)
library(fobitools)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(readxl)
library(rentrez)
library(xml2)
library(XML)
library(SnowballC)
library(wordcloud)
library(stringi)
#
# 0. Building a corpus

#ONLY ABSTRACTS
# your.ids <- c("32560627", "32967625")

#Grapes and berries "32967625"
#Biomarkers for tropical fruits 32560627
#Mediterranean diet 31521398
#Red meat 0535398   #PROBLEM W THIS ONE IDK WHY

all.ids <- c("31521398", "32560627", "32967625")

# rentrez function to get the data from pubmed db
fetch.pubmed <- entrez_fetch(db = "pubmed", id = all.ids,
                             rettype = "xml", parsed = T)

# Extract the Abstracts for the respective IDS.
abstracts = xpathApply(fetch.pubmed, '//PubmedArticle//Article', function(x)
  xmlValue(xmlChildren(x)$Abstract))

abstracts <- lapply(abstracts, iconv, to = "utf-8" )

# Change the abstract names to the IDS.
names(abstracts) <- all.ids



#Create corpus
# lapply(abstracts, enc2utf8)
abs_corpus <- VCorpus(VectorSource(abstracts))

inspect(abs_corpus[[1]])

inspect(abs_corpus)

###Transformations



#lowercase
abs <- tm_map(abs_corpus, content_transformer(tolower))     #WORKED BY REMOVING THE CONTENT TRANSFORMER
# abs <- tm_map(abs, PlainTextDocument) 
#whitespaces
abs <- tm_map(abs, stripWhitespace)
#stopwords
abs <- tm_map(abs, removeWords, stopwords("english"))

my_stop_words <- c("also", "recently")

# regex matching punctuation except intra word commas (?<!\d)[,.]+

library(stringr)
remove_copyright <- function(x){
  x <- str_replace_all(x, "\\u00AE|\\u00a9|\\u2122", "")
  x
}

abs <- tm_map(abs, content_transformer(remove_copyright))

removepunct <- function(x){
  x <- str_replace_all(x, regex(pattern = "[,](?=\\d)" ), "KEEP-tHis-COMmAA")
  x <- removePunctuation(x, preserve_intra_word_contractions = T, preserve_intra_word_dashes = T)
  x <- str_replace_all(x, regex(pattern = "(KEEP-tHis-COMmAA)" ), "," )
  x
}

abs <- tm_map(abs, content_transformer(removepunct))



# NNOT WORKInG

translate_greek <- function(x){
  lets <- names(greek)[1:48]
  tran <- unname(greek_vector[1:48])
  x <- stri_replace_all_regex(x, lets, tran, vectorize = FALSE)
  x
}

#corp <- tm_map(abs, content_transformer(.......)


#stemming
abs_stemmed <- tm_map(abs, stemDocument)



inspect(corp[[2]])


#term-document matrix
dtm <- DocumentTermMatrix(abs)
dtm_stemmed <- DocumentTermMatrix(abs_stemmed)
inspect(dtm)
# 
# Terms(dtm)
# tidy(dtm)

#3. Exploratory analysis --> wordclouds
summary(abs)
set.seed(88)

#unstemmed
corpus_mtrx <- as.matrix(TermDocumentMatrix(abs))
sort_mtrx <- sort(rowSums(corpus_mtrx), decreasing = TRUE)
df <- data.frame(word = names(sort_mtrx), freq = sort_mtrx)
wordcloud(words = df$word, freq = df$freq, min.freq = 2, max.words = 60, scale=c(2, .5),
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
#stemmed
corpus_mtrx_stemmed <- as.matrix(TermDocumentMatrix(abs_stemmed))
sort_mtrx_stemmed <- sort(rowSums(corpus_mtrx_stemmed), decreasing = TRUE)
df_stemmed <- data.frame(word = names(sort_mtrx_stemmed), freq = sort_mtrx_stemmed)
wordcloud(words = df_stemmed$word, freq = df_stemmed$freq, min.freq = 2, max.words = 60, scale=c(2, .5),
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))



#N-Grams and histograms DO THIS WITH TIDY DATA, NOT CORPUs
library(ggplot2)
df_abs <- tibble(text = sapply(abstracts, as.character), PMID = names(abstracts))

#remove stop words
data("stop_words")

df_uni <- df_abs %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

df_uni %>%
  count(word, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  coord_flip()

##########

df_bi <- df_abs %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2)

df_bi_sep <- df_bi %>%
  separate(word, c("word1", "word2"), sep = " ")

df_bi_filtered <- df_bi_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

df_bi_count <- df_bi_filtered %>%
  count(word1, word2, sort = TRUE)

df_bi_united <- df_bi_filtered %>%
  unite(word, word1, word2, sep = " ")
  
  
df_bi_united %>%
  filter(n > 1) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  coord_flip()

###########

df_tri <- df_abs %>% 
  unnest_tokens(word, text, token = "ngrams", n = 3) 

df_tri_sep <- df_tri %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

df_tri_filtered <- df_tri_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

df_tri_count <- df_tri_filtered %>%
  count(word1, word2, word3, sort = TRUE)

df_tri_united <- df_tri_filtered %>%
  unite(word, word1, word2, word3, sep = " ")
  
  

df_tri_united %>%
  count(word, sort = TRUE)
  filter(n == 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  coord_flip()



#############

bi_idf <- df_bi_united %>%
  count(PMID,word) %>%
    bind_tf_idf(word,PMID,n) %>%
    arrange(desc(tf_idf))

  tri_idf <- df_tri_united %>%
    count(PMID,word) %>%
    bind_tf_idf(word,PMID,n) %>%
    arrange(desc(tf_idf))

#SENTENCES

df_sentences <- df_abs %>%
  group_by(PMID) %>%
  unnest_tokens(sentence, text, token = "sentences")  %>%
  mutate(sent_index = row_number()) %>%
  mutate(id = paste(PMID, sent_index, sep = "_"))

# sentences_ann <- sentences %>%
#   ungroup() %>%
#   select(id, sentence)

# df_sent <- df_ngrams %>%
#   unnest_sentences(sentence, text)


# #############FULL TEXT###########################
# library(pdftools)
# directory <-  "/home/vant/Desktop/TFG/TMpipeline/corpus_ABSTRACTS"
# my_corpus <- VCorpus(DirSource(directory, pattern = ".pdf"), 
#                      readerControl = list(reader = readPDF))
# my_corp <- VCorpus(DirSource(directory = directory, encoding = "UTF-8"), list(reader = readPDF))
# lapply(my_corp[1], as.character)
# 
# ###Transformations
# #whitespaces
# full <- tm_map(my_corp, stripWhitespace)
# #lowecase
# full <- tm_map(my_corp, content_transformer(tolower))
# 
# #remove stopwords
# full <- tm_map(full, removeWords, stopwords("english"))



# 1. Building a dictionary (both for foods and metabolites)
# setwd("~/Desktop/TFG/TMpipeline/NER-dictionary-based")
setwd("C:/Users/pauca/OneDrive/Escritorio/Marina TFG/TMpipeline/NER-dictionary-based")

#FOR METS

#we want IDs + synonyms
lines <- readLines("metabolites-2022-06-23_HDMB")
dat <- read.csv(textConnection(lines))

hdmb_dict <- dat %>%
  select(HMDB_ID, NAME, INCHIKEY) %>%
  rename(HMDB = HMDB_ID) %>%
  rename(name = NAME) %>%
  rename(InChIKey = INCHIKEY) %>%
  filter(!is.na(name)) %>%
  mutate(name = tolower(name))
  
fobi_mets <- fobitools::parse_fobi(terms = "FOBI:01501", get = "des") %>%
  select(name, alias, HMDB, InChIKey) %>%
  filter(!duplicated(name)) %>%
  mutate(name = tolower(name))

exp <- read_excel("biomarkers_exposome.xlsx") 
exp <- exp %>%
  select(Name, Synonyms, `HMDB ID`, InChIKey) %>%
  rename(HMDB = `HMDB ID`) %>%
  rename(name = Name) %>%
  filter(!is.na(HMDB)) %>%
  filter(!duplicated(name)) %>%
  mutate(name = tolower(name)) %>%
  separate_rows(Synonyms, sep = ";") %>%
  mutate(Synonyms = trimws(tolower(Synonyms)))


#combine HMDB + exp + FOBI metabolites
a <- full_join(fobi_mets, exp, by = c("name", "HMDB", "InChIKey"))
dict_met <- full_join(a, hdmb_dict, by = c("name", "HMDB", "InChIKey")) %>%
  select(name,alias,Synonyms)
  
#FOR FOODS
dict_food <- fobitools::foods %>%
  select(name)

#2 Which words from our dictionary appear in the abstracts?

mini_dict <- c("fruits")
inspect(DocumentTermMatrix(abs,
                           list(dictionary = mini_dict)))





subset_of_docs <- corpus[1:10]
subset_of_docs
lapply(subset_of_docs, content)
