#still getting some weird results with numbers so we'll check those out:
original_tidy_corp %>%
  filter(str_detect(text, "8-oxogua")) %>%
  select(id)

gold_corp %>%
  filter(doc_id == "PMC2771025_A01002") %>%
  select(text)

#compare with preprocessed text
original_tidy_corp %>%
  filter(id == "PMC2771025_A01002") %>%
  select(text)

#everything seems okay!

############################################################

#jwh
original_tidy_corp %>%
  filter(str_detect(text, "jwh")) %>%
  select(id)

gold_corp %>%
  filter(doc_id == "PMC3963402_D00003") %>%
  select(text)

#compare with preprocessed text
original_tidy_corp %>%
  filter(id == "PMC3963402_D00003") %>%
  select(text)
#we might keep this one!

############################################################

#oh
original_tidy_corp %>%
  filter(str_detect(text, "oh")) %>%
  select(id)

gold_corp %>%
  filter(doc_id == "PMC2721977_A02015") %>%
  select(text)

#compare with preprocessed text
original_tidy_corp %>%
  filter(id == "PMC2721977_A02015") %>%
  select(text)
#keep 

############################################################
#oh
original_tidy_corp %>%
  filter(str_detect(text, "2")) %>%
  select(id)

gold_corp %>%
  filter(doc_id == "PMC2267737_D00053") %>%
  select(text)

#compare with preprocessed text
original_tidy_corp %>%
  filter(id == "PMC2267737_D00053") %>%
  select(text)
#keep 

###########################################################

#oh
original_tidy_corp %>%
  filter(str_detect(text, "2")) %>%
  select(id)

gold_corp %>%
  filter(doc_id == "PMC2267737_D00053") %>%
  select(text)

#compare with preprocessed text
original_tidy_corp %>%
  filter(id == "PMC2267737_D00053") %>%
  select(text)
#keep 

############################################################
#D
original_tidy_corp %>%
  filter(str_detect(text, " d ")) %>%
  select(id)

gold_corp %>%
  filter(PMC == "PMC3513544") %>%
  select(text)

#compare with preprocessed text
original_tidy_corp %>%
  filter(PMC == "PMC3513544") %>%
  select(text)
#keep because it refers to vitamin D!

###########################################################

#We find "p" and "C" as most common wordS?
original_tidy_corp %>%
  filter(str_detect(text, " p ")) %>%
  select(text)
#P is used for p value so we'll add to stopwords

original_tidy_corp %>%
  filter(str_detect(text, " c ")) %>%
  select(text)
#we will keep C because it might be related to vitamin C!

#check most freq words again
tidy_corp %>%
  count(word, sort = TRUE)

############################################################
#τ   #weird symbol
original_tidy_corp %>%
  filter(str_detect(text, "τ")) %>% 
  filter(PMC == "PMC3513544") %>%
  select(id)

gold_corp %>%
  filter(doc_id == "PMC3513544_M04000") %>%
  select(text)

#compare with preprocessed text
original_tidy_corp %>%
  filter(PMC == "PMC3513544") %>%
  select(text)
#remove because it's a weird symbol and refers to T statistic

###########################################################
m <- stri_extract_all_fixed(original_tidy_corp$text, 	
                            +              "mg100", simplify = TRUE)

original_tidy_corp[59,]$text


#######################OTHER STUFF############################

subset <- corp_df[1:146, ]
interesting_met <- corp_df[163,]

raw_counts <- subset %>%
  unnest_tokens(word,text, strip) %>%
  count(PMC, word, sort = TRUE)

counts_by_tf_idf <- raw_counts %>%
  bind_tf_idf(word, PMC, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

counts_by_tf_idf %>%
  group_by(PMC) %>%
  top_n(15, tf_idf) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = PMC)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~PMC, ncol = 2, scales = "free") +
  coord_flip()


gold_corp %>%
  filter(str_detect(text, "NMR")) %>%
  select(text)

#remove stop words
# data("stop_words")
stop = c("i.e", "NA", "p.p.m", "mmoll", "som", "min", "FOS" , "IGT", "nmr", "PKU")
# my_stop_words = tibble(word = stop, lexicon = "custom")

# custom_stop_words <- bind_rows(my_stop_words, stop_words)
# raw_counts <- anti_join(raw_counts, custom_stop_words, by = "word")

plot <- raw_counts %>%
  bind_tf_idf(word, PMC, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(PMC) %>%
  top_n(15, tf_idf) %>%
  ungroup() 

ggplot(plot, aes(word, tf_idf, fill = PMC)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~PMC, ncol = 2, scales = "free") +
  coord_flip()



subset <- gold_corp[1:50,]  #WORKING WITH A SUBSET FOR NOW

library(ggplot2)

df_uni <- subset %>% 
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)

df_uni %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  coord_flip()

##########

df_bi <- subset %>% 
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

subset <- gold_corp[1:500,]

df_tri <- subset %>% 
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
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
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
