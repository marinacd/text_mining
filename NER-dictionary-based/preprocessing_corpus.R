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
library(RColorBrewer)


###########Create corpus
file = "/Users/pauca/OneDrive/Escritorio/Marina TFG/TMpipeline/MetaboliteNER-main/Corpus/GoldStandard.txt"

#Corpus
corpus <- read.delim(file, header = FALSE, sep = "\t", dec = ".") %>%
  rename(PMC = V1, SentID = V2, text = V3) %>%
  mutate(doc_id = paste(PMC, SentID, sep = "_")) 
#   select(doc_id, text)

corpus <- VCorpus(DataframeSource(corpus))

#Check metadata
# NLP::meta(corpus)



#########Exploratory analysis


length(unique(NLP::meta(corpus)$PMC)) 
#183

# #Performed for 9 first documents
# subset_of_docs <- gold_corp %>% group_split(PMC)
# names(subset_of_docs) <- unique(gold_corp$PMC)
# 
# #subset of 9 for exploratory analysis
# subset <- subset_of_docs[1:9]
# 
# subset_to_corp <- subset_of_docs %>%
#   select(doc_id, text)
# 
# corpus_subset <- VCorpus(VectorSource(subset_to_corp))



####Exploratory analysis before preprocessing

#wordclouds before processing

set.seed(234567)

#unstemmed
corpus_mtrx <- as.matrix(TermDocumentMatrix(corpus))
corpus_mtrx <- sort(rowSums(corpus_mtrx), decreasing = TRUE)
corpus_mtrx <- data.frame(word = names(corpus_mtrx), freq = corpus_mtrx)
wordcloud(words = corpus_mtrx$word, freq = corpus_mtrx$freq, min.freq = 2, max.words = 60, scale=c(2, .5),
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))



#########Basic preprocessing

#lower case
corpus <- tm_map(corpus, content_transformer(tolower)) 
inspect(corpus[[2]])

# #stopwords
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# inspect(corpus[[2]])


#unite words around dashes!
unite_dashes <- function(x){
  x <- str_replace_all(x, regex(pattern = "-\\s" ), "-")
  x
}

corpus <- tm_map(corpus, content_transformer(unite_dashes))                       
inspect(corpus[[153]])

#remove punctuation
removepunct <- function(x){
  x <- str_replace_all(x, regex(pattern = "[,](?=\\d)" ), "KEEP-tHis-COMmAA")
  x <- removePunctuation(x, preserve_intra_word_contractions = T, preserve_intra_word_dashes = T)
  x <- str_replace_all(x, regex(pattern = "(KEEP-tHis-COMmAA)" ), "," )
  x
}

corpus <- tm_map(corpus, content_transformer(removepunct))
inspect(corpus[[163]])


# ###subset for further analysis
# ind <- which(meta(corpus)$PMC == "PMC2267737")
# subset <- corpus[ind]
# 
# dtm <- DocumentTermMatrix(subset)
# terms <- Terms(dtm)
# 
# Terms(dtm) 
#returns all terms, as we can see:

#there's unit names which we will add to stop-word dictionary
#we are using it like this because we are not in tidy format right now
stop = c("ie", "na", "ppm" ,"mmoll", "som", "min", "fos" , "igt", "nmr", "pku", "mgdl", "whereas",
         "p", "mz", "lc-msms", "h", "μl", "lmwm", "fos-ngt", "fos-igt", "μmoll", "also",  "μm", "without", 
         "yet", "within", "worse" , "whether" , "τ" , "et al", "° c", "well", "fmolmg", "ml", "mg100",
         "μmolmmol", "mmolkg", "nmolmmol", "mmoldkg", "nmol", "pmolmg", "pmollmg")

custom_stop_words <- c(stopwords("english"), stop)
corpus <- tm_map(corpus, removeWords, custom_stop_words )
inspect(corpus[[2]])

#there's lots of numbers so we could apply something to remove them

#remove numbers but only if alone
removenum <- function(x){
  x <- stri_replace_all_regex(x, "[:blank:][:digit:]+[:blank:]", "")
  x <- stri_replace_all_regex(x, "[:digit:]+\\.[:digit:]+", "")
  
  x
}

corpus <- tm_map(corpus, content_transformer(removenum))
inspect(corpus[[12]]) #num should be removed
inspect(corpus[[71]]) #num should be removed
inspect(corpus[[163]]) #num should NOT be removed


#whitespaces
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[[2]])

corpus <- tm_map(corpus, content_transformer(str_squish))
inspect(corpus[[2]])

#stemming
corp_stemmed <- tm_map(corpus, stemDocument)
inspect(corp_stemmed[[2]])

#wordclouds after basic preprocessing

#unstemmed
corpus_mtrx <- as.matrix(TermDocumentMatrix(corpus))
corpus_mtrx <- sort(rowSums(corpus_mtrx), decreasing = TRUE)
corpus_mtrx <- data.frame(word = names(corpus_mtrx), freq = corpus_mtrx)
wordcloud(words = corpus_mtrx$word, freq = corpus_mtrx$freq, min.freq = 2, max.words = 60, scale=c(2, .5),
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))


#stemmed
corpus_mtrx_stemmed <- as.matrix(TermDocumentMatrix(corp_stemmed))
corpus_mtrx_stemmed <- sort(rowSums(corpus_mtrx_stemmed), decreasing = TRUE)
corpus_mtrx_stemmed <- data.frame(word = names(corpus_mtrx_stemmed), freq = corpus_mtrx_stemmed)
wordcloud(words = corpus_mtrx_stemmed$word, freq = corpus_mtrx_stemmed$freq, min.freq = 2, max.words = 60, scale=c(2, .5),
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

#AS WE CAN SEE, STEMMING SEEMS NECESSARY IN THIS CASE ????

original_tidy_corp <- tidy(corpus) 
original_tidy_corp$PMC <- NLP::meta(corpus)$PMC
original_tidy_corp$SentID <- NLP::meta(corpus)$SentID
original_tidy_corp <- original_tidy_corp %>%
  select(id, text, PMC, SentID)


#we will use quanteda to tokenize to avoid stripping punctuation
library(quanteda)

original_tidy_corp <- original_tidy_corp %>%
  mutate(text= str_replace_all(text, "-", "_"))


#Splitting in tokens (using quanteda)
original_tidy_corp <- corpus(original_tidy_corp, docid_field = "id", text_field = "text", meta = NLP::meta(corpus),unique_docnames = TRUE)


tokens <- tokens(original_tidy_corp, split_hyphens = FALSE, include_docvars = TRUE)
names(tokens) <- names(corpus)


qterms_df <- dfm(tokens)
qterms_df <- tidy(qterms_df)
qterms_df[c("PMC", "SentID")] <- str_split_fixed(qterms_df$document, "_", n = 2)

#SAME but FOR STEMMED CORPUS
original_tidy_corp_stemmed <- tidy(corp_stemmed) 
original_tidy_corp_stemmed$PMC <- NLP::meta(corp_stemmed)$PMC
original_tidy_corp_stemmed$SentID <- NLP::meta(corp_stemmed)$SentID
original_tidy_corp_stemmed <- original_tidy_corp_stemmed %>%
  select(id, text, PMC, SentID)

original_tidy_corp_stemmed <- original_tidy_corp_stemmed %>%
  mutate(text= str_replace_all(text, "-", "_"))


#Splitting in tokens (using quanteda)
original_tidy_corp_stemmed <- corpus(original_tidy_corp_stemmed, docid_field = "id", text_field = "text", meta = NLP::meta(corp_stemmed),unique_docnames = TRUE)


tokens_stemmed <- tokens(original_tidy_corp_stemmed, split_hyphens = FALSE, include_docvars = TRUE)
names(tokens_stemmed) <- names(corp_stemmed)


qterms_df_stemmed <- dfm(tokens_stemmed)


qterms_df_stemmed <- tidy(qterms_df_stemmed)
qterms_df_stemmed[c("PMC", "SentID")] <- str_split_fixed(qterms_df_stemmed$document, "_", n = 2)


# tidy_corp <- qterms_df %>%
#   str_replace_all(text, "-", "_") %>%
#   unnest_tokens(word,text, token = "words", strip_punct = FALSE, strip_numeric = FALSE)

# custom_stop_df <- stop_words %>%
#   rbind()
# 
# tidy_corp <- tidy_corp %>%
#   anti_join(stop_words) 


# tidy_corp <- tidy_corp %>%
#   group_by(PMC) %>%
#   filter(n() > 10) %>%
#   ungroup()

