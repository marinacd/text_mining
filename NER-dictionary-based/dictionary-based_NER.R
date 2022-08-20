######################################Dictionary based-NER##############################################
###########Create dictionaries

#HMDB dict
setwd("~/Desktop/TFG/text_mining/NER-dictionary-based")


#we want IDs + synonyms
dat <- readLines("metabolites-2022-06-23_HDMB")
dat <- read.csv(textConnection(dat))

hdmb_dict <- dat %>%
  select(HMDB_ID, NAME, INCHIKEY) %>%
  rename(HMDB = HMDB_ID) %>%
  rename(name = NAME) %>%
  rename(InChIKey = INCHIKEY) %>%
  filter(!is.na(name)) %>%
  mutate(name = tolower(name))

#HMDB + FOBI + EXP dict

fobi_mets <- fobitools::parse_fobi(terms = "FOBI:01501", get = "des") %>%
  select(name, HMDB, InChIKey) %>%
  filter(!duplicated(name)) %>%
  mutate(name = tolower(name))
 
fobi_classes <- unique(fobitools::fobi$is_a_name)
fobi_classes <- fobi_classes[-133]

remove_patterns <- c("and", "derivatives", "compounds" , "substituted", "NA", "related" , 
                     "lipids" , "molecules", "acid", "amino", "acids", NA, "amino acids, peptides,", "substituted derivatives"  )
fobi_classes <- fobi_classes[-which(fobi_classes %in% fobitools::foods$is_a_name)] %>%
  stri_split_fixed("and") %>%
  unlist() %>%
  str_squish() %>%
  tolower()
fobi_classes <- fobi_classes[-which(fobi_classes %in% tolower(remove_patterns))]
  

fobi_alias <- unique(fobitools::fobi$alias)[1:74]


# FOR FOODS
# dict_food <- fobitools::foods %>%
#   select(name, is_a_name) 
# vec_food <- unique(c(dict_food$name, dict_food$is_a_name))




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
dict_met <- full_join(fobi_mets, exp, by = c("name", "HMDB", "InChIKey"))
dict_met <- full_join(exp, hdmb_dict, by = c("name", "HMDB", "InChIKey")) %>%
  select(name,Synonyms)
#missing classes and alias from fobi

#in a vector
# met_vec <- append(dict_met$name, dict_met$alias[(!is.na(dict_met$alias))])
met_vec <- append(dict_met$name, dict_met$Synonyms[(!is.na(dict_met$Synonyms))])
met_vec <- append(met_vec, fobi_alias)
met_vec <- tolower(unique(append(met_vec, fobi_classes)))
met_vec <- met_vec[-which(is.na(met_vec))]



##################Check for everything################
wt <- qterms_df
###################Check for subset####################
# wt <- qterms_df %>%
#   filter(PMC %in% subset) 

# wt_st <- qterms_df_stemmed %>%
#   filter(PMC %in% subset)

##################Filter non-alpha chars#############
wt <- wt %>%
  filter(str_detect(term, "[A-Za-z, ]+"))

###############LAYER 1: EXACT MATCH WITH NAME#################
library(stringdist)

ref_name <- dict_met %>%
  rename(term = name)

match_lay_1 <- semi_join(wt, ref_name, by = "term")
no_match_lay_1 <- anti_join(wt, ref_name, by = "term")

#check
nrow(match_lay_1) + nrow(no_match_lay_1) == nrow(wt) 

# #get coordinates
# library(stringr)
# 
# 
# 
# match_lay_1 <- match_lay_1 %>%
#   str_locate(content(corpus), term )
# 
# 
# # df <- tibble(start = m[1], end = m[2])
# # > df
# # # A tibble: 1 × 2
# # start   end
# # <int> <int>
# #   1   117   126
# # > rbind(df, c(m[1], m[2]))
# # # A tibble: 2 × 2
# # start   end
# # <int> <int>
# #   1   117   126
# # 2   117   126
# #############LAYER 2a: EXACT MATCH WITH SYNONYM ################

ref_syn <- dict_met %>%
  rename(term = Synonyms)

match_lay_2a <- merge(no_match_lay_1, ref_syn, by = "term")
no_match_lay_2a <- anti_join(no_match_lay_1, ref_syn, by = "term")

#check
nrow(match_lay_2a) + nrow(no_match_lay_2a) == nrow(no_match_lay_1)  #CHECK WHY THIS IS GIVING FALSE??? i think there was an NA somewhere

#############LAYER 2a: EXACT MATCH WITH ALIAS ################
ref_alias <- dict_met %>%
  rename(term = alias)

match_lay_2b <- merge(no_match_lay_2a, ref_alias, by = "term")
no_match_lay_2b <- anti_join(no_match_lay_2a, ref_alias, by = "term")

#check
nrow(match_lay_2b) + nrow(no_match_lay_2b) == nrow(no_match_lay_2a) 


#############LAYER 3a: FUZZY MATCH WITH NAME################
library(RecordLinkage)
#choose similarity
sim = 0.8
#I HAVE TO DO IT IN SUBSETS BC I CAN'T aLLOCATE THIS MUCH MEMORY

subset1 <- no_match_lay_2b[0: (nrow(no_match_lay_2b)/2) ,]
subset2 <- no_match_lay_2b[((nrow(no_match_lay_2b)/2) +1): nrow(no_match_lay_2b),]
# subset3 <-
# subset4 <- 
  
#subset1
wordlist_lv <- expand.grid(term = subset1$term, ref = ref_name$term, stringsAsFactors = FALSE) %>% 
  group_by(term) %>% 
  mutate(match_score = levenshteinSim(term, ref)) %>%
  summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])

match_lay_3a <- merge(no_match_lay_2b, wordlist_lv[which(wordlist_lv$match >= sim),])
no_match_lay_3a <- anti_join(no_match_lay_2b, wordlist_lv[which(wordlist_lv$match >= sim),])

#subset2
wordlist_lv <- expand.grid(term = no_match_lay_2b$term, ref = ref_name$term, stringsAsFactors = FALSE) %>% 
  group_by(term) %>% 
  mutate(match_score = levenshteinSim(term, ref)) %>%
  summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])

match_lay_3a <- merge(no_match_lay_2b, wordlist_lv[which(wordlist_lv$match >= sim),])
no_match_lay_3a <- anti_join(no_match_lay_2b, wordlist_lv[which(wordlist_lv$match >= sim),])


#general
# wordlist_lv <- expand.grid(term = no_match_lay_2b$term, ref = ref_name$term, stringsAsFactors = FALSE) %>% 
#   group_by(term) %>% 
#   mutate(match_score = levenshteinSim(term, ref)) %>%
#   summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])
# 
# match_lay_3a <- merge(no_match_lay_2b, wordlist_lv[which(wordlist_lv$match >= sim),])
# no_match_lay_3a <- anti_join(no_match_lay_2b, wordlist_lv[which(wordlist_lv$match >= sim),])


#check
nrow(match_lay_3a) + nrow(no_match_lay_3a) == nrow(no_match_lay_2b) 

#############LAYER 3b: FUZZY MATCH WITH ALIAS################

wordlist_lv <- expand.grid(term = no_match_lay_3a$term, ref = ref_alias$term, stringsAsFactors = FALSE) %>% 
  group_by(term) %>% 
  mutate(match_score = levenshteinSim(term, ref)) %>%
  summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])

match_lay_3b <- merge(no_match_lay_3a, wordlist_lv[which(wordlist_lv$match >= sim),])
no_match_lay_3b <- anti_join(no_match_lay_3a, wordlist_lv[which(wordlist_lv$match >= sim),])

#############LAYER 3b: FUZZY MATCH WITH SYNONYM################

wordlist_lv <- expand.grid(term = no_match_lay_3b$term, ref = ref_syn$term, stringsAsFactors = FALSE) %>% 
  group_by(term) %>% 
  mutate(match_score = levenshteinSim(term, ref)) %>%
  summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])

match_lay_3c <- merge(no_match_lay_3b, wordlist_lv[which(wordlist_lv$match >= sim),])
no_match_lay_3c <- anti_join(no_match_lay_3b, wordlist_lv[which(wordlist_lv$match >= sim),])


#check
nrow(match_lay_3a) + nrow(no_match_lay_3a) == nrow(no_match_lay_2b) 

#explain why for we are using levenshtein distance --> because metabolites have similar prefixes and jarowinkler fucks that up, also works better for shorter names
#use examples_IN PRESENTATION (EXTRA DIAPO???)

# control/cortol
# cpf/cmpf
#3_hydroxybutyrate      0.874 3-hydroxybutyrylcarnitine 

###################FINAL LAYER AGAINST STEMMED DIC########################
#Create DTMs from dictionary
dtm_stemmed_nodic <- DocumentTermMatrix(corp_stemmed)

#ALMOST ALL DOCUMENT-WORD PAIRS ARE 0

dtm_stemmed <- TermDocumentMatrix(corp_stemmed, list(dictionary = met_vec))

#ALMOST ALL DOCUMENT-WORD PAIRS ARE 0
inspect(dtm_stemmed)
inspect(dtm_stemmed_nodic)

#########################################################################
  
#   
# wordlist <- expand_grid(words = wt$term, ref = dict_met$name) %>%
#   mutate(detect = stringr::str_detect(words, paste0("\\b", ref, "\\b"))) %>%
#   filter(ref == words | detect == "TRUE") %>%
#   select(-detect)
# 
# result0 <- merge(ffq, wordlist, by = "words")
# result0 <- merge(result0, fobi_foods, by = "ref") %>%
#   select(FOOD_ID, FOOD_NAME, id_code, name) %>%
#   filter(!duplicated(.))
# 
# no_matched <- ffq %>% filter(!FOOD_NAME %in% result0$FOOD_NAME)




####################Get coordinates of matches#################
library(stringr)

str_locate(content(corpus[[1]]), "lipoprotein" )
#       start end
# [1,]    46  56

# LAYER 1:


# LAYER 2a:
  
# LAYER 2b:
  
# LAYER 3a:
  
# LAYER 3b:

# LAYER 3c:













#Create DTMs from dictionary
dtm_nodic <- DocumentTermMatrix(corpus)
dtm_stemmed_nodic <- DocumentTermMatrix(corp_stemmed)

#ALMOST ALL DOCUMENT-WORD PAIRS ARE 0


dtm <- TermDocumentMatrix(corpus, list(dictionary = met_vec))
dtm_stemmed <- TermDocumentMatrix(corp_stemmed, list(dictionary = met_vec))
#COMPARE 
inspect(dtm)   
inspect(dtm_nodic) 

#ALMOST ALL DOCUMENT-WORD PAIRS ARE 0
inspect(dtm_stemmed)
inspect(dtm_stemmed_nodic)
#We can tidy them to only see the non-zero values with (tidy)

# https://github.com/pcastellanoescuder/fobitools/blob/master/R/annotate_foods.R
# https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance
# https://stackoverflow.com/questions/653157/a-better-similarity-ranking-algorithm-for-variable-length-strings
# https://stackoverflow.com/questions/11535625/similarity-scores-based-on-string-comparison-in-r-edit-distance
# https://cran.r-project.org/web/packages/RecordLinkage/RecordLinkage.pdf #levenshteinSim
# https://github.com/markvanderloo/stringdist

####################################


