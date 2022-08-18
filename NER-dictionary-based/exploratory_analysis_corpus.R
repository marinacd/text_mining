
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

doc_count <- left_join(doc_count, total_count, by = "PMC")


#distribution of n/total for each article
subset = c("PMC5929433", "PMC3513544", "PMC3682342" ,"PMC6708594" , "PMC4396978", "PMC6636917", "PMC2538910" )

#looking only at 7 articles!
subset_sum <- doc_count %>%
  filter(PMC %in% subset)

ggplot(subset_sum, aes(n/total, fill = PMC))+
  geom_histogram(show.legend = FALSE) +
  stat_bin(bins = 50)+
  facet_wrap(~PMC, ncol = 2, scales = "free_y")     
                                    #we can see that there are VERY LONG TAILS TO THE RIGHT --> very common words not shown in PLOTs!!!!!!
                                    #we have similar distribution in all plots --> many words that occur rarely and fewer that occur frequently

#this distribution is very common in NL --> the freq that a word appears is inversely proportional to its rank (ZIPF'S LAW)
#TO CALCULATE THIS WE'LL USE ALL ARTICLES FOR A MOMENT
freq_by_rank <- doc_count %>%
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


doc_count <- doc_count %>%
  bind_tf_idf(term, PMC, n)  #there's no 0 tf_idf because there's no 0 idf --> words that appear in all documents! #probably stop words tho


#this terms are (almost) unique to the articles --> probably metabolites
doc_count %>%
  arrange(desc(tf_idf)) %>%
  select(PMC,term,n,tf_idf)


#we will save the 5 most common ones in each PMC for later use
high_tf_idf <- doc_count %>%
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
bigrams <- dfm(bigrams)

bigrams <- tidy(bigrams)
bigrams[c("PMC", "SentID")] <- str_split_fixed(bigrams$document, "_", n = 2)


#we calculate tf_idf as before:
bigrams <- bigrams %>%
  count(PMC, term) %>%
  bind_tf_idf(term,PMC,n) %>%
  arrange(desc(tf_idf))
  
   

#we will save the 5 most common ones in each PMC for later use
bi_high_tf_idf <- bigrams  %>%
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
trigrams <- dfm(trigrams)

trigrams <- tidy(trigrams)
trigrams[c("PMC", "SentID")] <- str_split_fixed(trigrams$document, "_", n = 2)


tri_tf_idf <- trigrams %>%
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

bi_sep <- bigrams %>%
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


###########################Correlation between terms######################################
library(widyr)

#count the number of times two terms appear within documents
word_pairs <- qterms_df%>%
  pairwise_count(term,PMC, sort = TRUE)

word_pairs_stemmed <- qterms_df_stemmed%>%
  pairwise_count(term,PMC, sort = TRUE)


##############Calculate correlation coefficient

#using phi coefficient --> measure for binary correlation   #equivalent to pearson correlation
#how much more likely iy is that either both X and Y appeat ot neither do than that one appears without the other

    #insert formula

#filter by common words first (quin threshold?)
word_cor <-  qterms_df %>%
  group_by(term) %>%
  filter(n() >= 3) %>%
  pairwise_cor(term, PMC, sort = TRUE)


word_cor_stemmed <-  qterms_df_stemmed %>%    #word-pairs appear twice!
  group_by(term) %>%
  filter(n() >= 3) %>%
  pairwise_cor(term, PMC, sort = TRUE)

#example of words more correlated to "acid"
word_cor %>%
  filter(item2 == "acid")

word_cor %>%
  filter(item1 == "acid") %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_cor_stemmed %>%
  filter(item2 == "acid")

word_cor_stemmed %>%
  filter(item1 == "acid") %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

#example of words more correlated to "vitamin"
word_cor %>%
  filter(item2 == "vitamin")

word_cor %>%
  filter(item1 == "vitamin") %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


word_cor_stemmed %>%
  filter(item2 == "vitamin")

word_cor_stemmed %>%
  filter(item1 == "vitamin") %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

#We will change punctuation back to the original to match dictionary better

word_cor <- word_cor %>%
  mutate(item1 = str_replace_all(item1, "_", "-"))%>%
  mutate(item2 = str_replace_all(item2, "_", "-"))


word_cor_stemmed <- word_cor_stemmed %>%
  mutate(item1 = str_replace_all(item1, "_", "-"))%>%
  mutate(item2 = str_replace_all(item2, "_", "-"))



#correlation graphs ONLY FOR WORDS IN DICTIONARY
set.seed(12345)
library(ggrepel)

#ONLY FOR WORDS IN DICTIONARY (METS)
word_cor %>%
  filter(item1 %in% met_vec) %>%
  filter(correlation > .9) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, size = 2) +
  theme_void()

#THIS IS WORKINGGGGGGGGGGGGGGGGGGGGGGGG --> ALL LOOK LIKE METABOLITES TOO

#ONLY FOR WORDS IN DICTIONARY STEMMED (METS)
word_cor_stemmed %>%
  filter(item1 %in% met_vec) %>%
  filter(correlation > .9) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  theme_void()

#THIS IS WORKINGGGGGGGGGGGGGGGGGGGGGGGG --> ALL LOOK LIKE METABOLITES TOO



#aqui posar algo en plan al grafic podem veure algunes relacions amb menjar (bluleberries, soymilk..)
#amb foods no funciona tant pq hi ha mil maneres de ferho aixi q farem dictionary matching utilitzant algun tipus de mesura de similaritat (next page)


food_vec <-as.vector(dict_food)$name
##ONLY FOR WORDS IN DICTIONARY (FOODS)

word_cor %>%
  filter(item1 %in% food_vec) %>%
  filter(correlation > .9) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, size = 2) +
  theme_void()


#ONLY FOR WORDS IN DICTIONARY STEMMED (FOODS)
word_cor_stemmed %>%
  filter(correlation > .99) %>%
  filter(item1 %in% food_vec) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  theme_void()



#####################
###############FUZZYJOIN
library(fuzzyjoin, quietly = TRUE)
library(dplyr, quietly = TRUE)


wt <- qterms_df %>%
  filter(PMC %in% subset)

met_vec <- tibble(met_vec) %>%
  rename(mets = met_vec)


joined_jw <- wt %>%
  stringdist_inner_join(met_vec, method = "jw", by = c(term = "mets"), max_dist = 1)

joined_lv <- wt %>%
  stringdist_inner_join(met_vec, method = "lv", by = c(term = "mets"), max_dist = 1)

joined_dl <- wt %>%
  stringdist_inner_join(met_vec, method = "dl", by = c(term = "mets"), max_dist = 1)


#jo tambe vull que calculi la distancia per poder filtrar jo


##PROVES PER TRIAR MESURA

#RECORDLINKAGE JAROWRINKLER

library(RecordLinkage)
#######################################
b <- "luteolin"

b_no <- "luteal"
b_si <- "luteol"

jarowinkler(b,b_no) #0.8916667
jarowinkler(b,b_si) #0.95

levenshteinSim(b,b_no) #0.625
levenshteinSim(b,b_si) #0.75
######################################
c <- "trans-5-o-caffeoyl-d-quinic acid"

c_no <- "trans-4hydroxyproline"
c_si <- "trans-5-o-caffeoyl"

jarowinkler(c,c_no) #0.7779762
jarowinkler(c,c_si) #0.9125

levenshteinSim(c,c_no) #0.28125
levenshteinSim(c,c_si) #0.5625
######################################
d <- "aamu"

d_si <- "aamu" 
d_no <- "amu"

jarowinkler(d,d_si) # 1
jarowinkler(d,d_no) # 0.925

levenshteinSim(d,d_si) #1
levenshteinSim(d,d_no) #0.75
######################################
e <- "carnitine"

e_si <- "l-carnitine"
e_no <- "carnosine"

jarowinkler(e,e_si) #0.9393939
jarowinkler(e,e_no)# 0.911111

levenshteinSim(e,e_si) #0.8181818
levenshteinSim(e,e_no) #0.7777778
#######################
