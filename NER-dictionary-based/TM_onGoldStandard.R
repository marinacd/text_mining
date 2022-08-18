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
gold_corp <- read.delim(file, header = FALSE, sep = "\t", dec = ".") %>%
  rename(PMC = V1, SentID = V2, text = V3) %>%
  mutate(doc_id = paste(PMC, SentID, sep = "_")) 
#   select(doc_id, text)

corpus <- VCorpus(DataframeSource(gold_corp))

#Check metadata
meta(corpus)



#########Exploratory analysis


length(unique(gold_corp$PMC)) 
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
sort_mtrx <- sort(rowSums(corpus_mtrx), decreasing = TRUE)
df <- data.frame(word = names(sort_mtrx), freq = sort_mtrx)
wordcloud(words = df$word, freq = df$freq, min.freq = 2, max.words = 60, scale=c(2, .5),
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))



#########Basic preprocessing

#lower case
corpus <- tm_map(corpus, content_transformer(tolower)) 
inspect(corpus[[2]])

#stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus[[2]])

#stemming
corp_stemmed <- tm_map(corpus, stemDocument)
inspect(corp_stemmed[[2]])

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
         "yet", "within", "worse" , "whether" , "τ" , "et al", "° c", "well")

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
inspect(corpus[[12]])
inspect(corpus[[71]])#num should be removed
inspect(corpus[[163]]) #num should NOT be removed


#whitespaces
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[[2]])

#wordclouds after basic preprocessing

#unstemmed
corpus_mtrx <- as.matrix(TermDocumentMatrix(corpus))
sort_mtrx <- sort(rowSums(corpus_mtrx), decreasing = TRUE)
df <- data.frame(word = names(sort_mtrx), freq = sort_mtrx)
wordcloud(words = df$word, freq = df$freq, min.freq = 2, max.words = 60, scale=c(2, .5),
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))


#stemmed
corpus_mtrx_stemmed <- as.matrix(TermDocumentMatrix(corp_stemmed))
sort_mtrx_stemmed <- sort(rowSums(corpus_mtrx_stemmed), decreasing = TRUE)
df_stemmed <- data.frame(word = names(sort_mtrx_stemmed), freq = sort_mtrx_stemmed)
wordcloud(words = df_stemmed$word, freq = df_stemmed$freq, min.freq = 2, max.words = 60, scale=c(2, .5),
          random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

#AS WE CAN SEE, STEMMING SEEMS NECESSARY IN THIS CASE ????

original_tidy_corp <- tidy(corpus) 
original_tidy_corp$PMC <- NLP::meta(corpus)$PMC
original_tidy_corp$SentID <- NLP::meta(corpus)$SentID
original_tidy_corp <- original_tidy_corp %>%
  select(id, text, PMC, SentID)


#we will use quanteda to tokenize to avoid stripping punctuation
library(quanteda)

to_q_corp <- original_tidy_corp %>%
  mutate(text= str_replace_all(text, "-", "_"))


#Splitting in tokens (using quanteda)
qcorp <- corpus(to_q_corp, docid_field = "id", text_field = "text", meta = NLP::meta(corpus),unique_docnames = TRUE)


tokens <- tokens(qcorp, split_hyphens = FALSE, include_docvars = TRUE)
names(tokens) <- names(corpus)


qdfm <- dfm(tokens)
 
 
qterms_df <- tidy(qdfm)
qterms_df[c("PMC", "SentID")] <- str_split_fixed(qterms_df$document, "_", n = 2)

#SAME but FOR STEMMED CORPUS
original_tidy_corp_stemmed <- tidy(corp_stemmed) 
original_tidy_corp_stemmed$PMC <- NLP::meta(corp_stemmed)$PMC
original_tidy_corp_stemmed$SentID <- NLP::meta(corp_stemmed)$SentID
original_tidy_corp_stemmed <- original_tidy_corp_stemmed %>%
  select(id, text, PMC, SentID)

to_q_corp_stemmed <- original_tidy_corp_stemmed %>%
  mutate(text= str_replace_all(text, "-", "_"))


#Splitting in tokens (using quanteda)
qcorp_stemmed <- corpus(to_q_corp_stemmed, docid_field = "id", text_field = "text", meta = NLP::meta(corp_stemmed),unique_docnames = TRUE)


tokens_stemmed <- tokens(qcorp_stemmed, split_hyphens = FALSE, include_docvars = TRUE)
names(tokens_stemmed) <- names(corp_stemmed)


qdfm_stemmed <- dfm(tokens_stemmed)


qterms_df_stemmed <- tidy(qdfm_stemmed)
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
  
####Term frequency
qterms_df %>% 
  count(term, sort = TRUE)

qterms_df_stemmed %>% 
  count(term, sort = TRUE)


##checking to add to stopwords in other doc## 


#######################Exploratory analysis
#Analyzing document term frequency
#term freq by document
doc_count <- qterms_df %>%
  count(PMC,term,sort = TRUE)

library(ggplot2)

#words that appear more than 50 times per document
qterms_df %>%
  count(PMC, term, sort = TRUE) %>%
  filter(n > 65) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n)) +
  geom_col(aes(fill=PMC)) +
  xlab(NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  theme_classic(base_size = 12) +
  labs(fill= "Article", title="Word frequency", subtitle="n > 65")+
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer(palette="Spectral")


total_count <- doc_count %>%
  group_by(PMC) %>%
  summarize(total = sum(n))

doc_sum <- left_join(doc_count, total_count, by = "PMC")


#distribution of n/total for each article
subset = c("PMC5929433", "PMC3513544", "PMC3682342" ,"PMC6708594" , "PMC4396978", "PMC6636917", "PMC2538910" )

#looking only at 7 articles!
subset_sum <- doc_sum %>%
  filter(PMC %in% subset)

ggplot(subset_sum, aes(n/total, fill = PMC))+
  geom_histogram(show.legend = FALSE) +
  stat_bin(bins = 50)+
  facet_wrap(~PMC, ncol = 2, scales = "free_y")     
                                    #we can see that there are VERY LONG TAILS TO THE RIGHT --> very common words not shown in PLOTs!!!!!!
                                    #we have similar distribution in all plots --> many words that occur rarely and fewer that occur frequently

#this distribution is very common in NL --> the freq that a word appears is inversely proportional to its rank (ZIPF'S LAW)
#TO CALCULATE THIS WE'LL USE ALL ARTICLES FOR A MOMENT
freq_by_rank <- doc_sum %>%
  group_by(PMC) %>%
  mutate(rank = row_number(),freq = n/total) %>%
  ungroup()

freq_by_rank %>% 
  ggplot(aes(rank, freq, color = PMC)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#not quite constant tho??? --> broken power law????
rank_subset <- freq_by_rank %>%
  filter(rank < 100, rank >10)

lm(log10(freq)~log10(rank), data = rank_subset)   #doesnt seem to follow the power law! #reflections on page 37 of book ig

#SOOOO LEt's use TF-IDF:
#           - to decrease weight of common words and increase weights that arent used much in the corpus
#           - to find the words that are important but not too common


doc_sum <- doc_sum %>%
  bind_tf_idf(term, PMC, n)  #there's no 0 tf_idf because there's no 0 idf --> words that appear in all documents! #probably stop words tho


#this terms are (almost) unique to the articles --> probably metabolites
doc_sum %>%
  arrange(desc(tf_idf)) %>%
  select(PMC,term,n,tf_idf)


#we will save the 5 most common ones in each PMC for later use
high_tf_idf <- doc_sum %>%
  arrange(desc(tf_idf)) %>%
  select(PMC,term,n,tf_idf) %>%
  group_by(PMC) %>%
  slice_head(n = 5)
  

#for the subset we saw before
subset_tf_idf <- high_tf_idf %>%
  filter(PMC %in% subset)

library(forcats)

subset_tf_idf %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(term, tf_idf), fill = PMC)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~PMC, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#we can see that what seem to be metabolite names have been cut into 2 so we could analyze bigrams or even trigrams instead of single words:

################################# NGRAM ANALYSIS

#Bigram analysis

bigrams <- tokens_ngrams(tokens, n = 2, concatenator = " ")

#make it tidy
bi_qdfm <- dfm(bigrams)

bi_df <- tidy(bi_qdfm)
bi_df[c("PMC", "SentID")] <- str_split_fixed(bi_df$document, "_", n = 2)


#we calculate tf_idf as before:
bi_tf_idf <- bi_df %>%
  count(PMC, term) %>%
  bind_tf_idf(term,PMC,n) %>%
  arrange(desc(bi_tf_idf))
  
   

#we will save the 5 most common ones in each PMC for later use
bi_high_tf_idf <- bi_tf_idf  %>%
  select(PMC,term,n,tf_idf) %>%
  group_by(PMC) %>%
  slice_head(n = 5)

#plot the subset
subset_bi_tf_idf <- bi_high_tf_idf %>%
  filter(PMC %in% subset)

subset_bi_tf_idf %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(term, tf_idf), fill = PMC)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~PMC, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


##############################maybe analyse trigrams too? ######################################
#Trigram analysis
trigrams <- tokens_ngrams(tokens, n = 3, concatenator = " ")

#make it tidy
tri_qdfm <- dfm(trigrams)

tri_df <- tidy(tri_qdfm)
tri_df[c("PMC", "SentID")] <- str_split_fixed(tri_df$document, "_", n = 2)


tri_tf_idf <- tri_df %>%
  count(PMC, term) %>%
  bind_tf_idf(term,PMC,n) %>%
  arrange(desc(tf_idf))

#we will save the 5 most common ones in each PMC for later use
tri_high_tf_idf <- tri_tf_idf %>%
  select(PMC,term,n,tf_idf) %>%
  group_by(PMC) %>%
  slice_head(n = 5)

#plot the subset
subset_tri_tf_idf <- tri_high_tf_idf %>%
  filter(PMC %in% subset)

subset_tri_tf_idf %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(term, tf_idf), fill = PMC)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~PMC, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

#############################CONCLUSIONS:

#Bigram analysis seems to be the most useful since it shows more metabolite names --> our metabolites might be split in 2!
#Also, n-gram analysis in general is interesting in very large datasets since the count is usually sparser

#Trigrams don't show as many metabolite names apparently!

#########################################

#Since we've determined that bigrams show metabolite names we'll keep analyzing them a little

#Network of bigrams
library(igraph)

bi_sep <- bi_df %>%
  separate(term, c("term1","term2"), sep = " ") %>%
  count(term1, term2, sort = TRUE)
#this ALSO suggests that we should use stemming! --> amino acids/amino acid

#filter only common bigrams 
bi_graph <- bi_sep %>%
  filter(n > 40) %>%
  graph_from_data_frame()

library(ggraph)

ggraph(bi_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bi_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#################Correlation between words
library(widyr)

#unt the number of times two words appear within documents).
word_pairs <- qterms_df%>%
  pairwise_count(term,PMC, sort = TRUE)

word_pairs_stemmed <- qterms_df_stemmed%>%
  pairwise_count(term,PMC, sort = TRUE)


######################################Dictionary based-NER##############################################
###########Create dictionaries

#HMDB dict
setwd("C:/Users/pauca/OneDrive/Escritorio/Marina TFG/TMpipeline/NER-dictionary-based")


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

#HMDB + FOBI + EXP dict

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

#in a vector
met_vec <- append(dict_met$name, dict_met$alias[(!is.na(dict_met$alias))])
met_vec <- append(met_vec, dict_met$Synonyms[(!is.na(dict_met$Synonyms))])


#FOR FOODS
dict_food <- fobitools::foods %>%
  select(name)

###################Check####################

#Create DTMs from dictionary
dtm_nodic <- DocumentTermMatrix(corpus)
dtm_stemmed_nodic <- DocumentTermMatrix(corp_stemmed)

              #ALMOST ALL DOCUMENT-WORD PAIRS ARE 0


dtm <- DocumentTermMatrix(corpus, list(dictionary = met_vec))
dtm_stemmed <- DocumentTermMatrix(corp_stemmed, list(dictionary = met_vec))

#COMPARE 
inspect(dtm)   
inspect(dtm_nodic) 

#ALMOST ALL DOCUMENT-WORD PAIRS ARE 0
inspect(dtm_stemmed)
inspect(dtm_stemmed_nodic)
#We can tidy them to only see the non-zero values with (tidy)




####################################


