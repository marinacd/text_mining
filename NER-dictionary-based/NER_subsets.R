#trying with subset (9 articles)

subset_1 <- unique(docvars(corpus)$PMC)[1:20]
subset_2 <- unique(docvars(corpus)$PMC)[21:31]
subset_3 <- unique(docvars(corpus)$PMC)[32:42]
subset_4 <- unique(docvars(corpus)$PMC)[43:60]
subset_5 <- unique(docvars(corpus)$PMC)[61:75]
subset_6 <- unique(docvars(corpus)$PMC)[76:90]
subset_7 <- unique(docvars(corpus)$PMC)[91:105]
subset_8 <- unique(docvars(corpus)$PMC)[106:120]
subset_9 <- unique(docvars(corpus)$PMC)[121:135]
subset_10 <- unique(docvars(corpus)$PMC)[136:150]
subset_11 <- unique(docvars(corpus)$PMC)[151:165]
subset_12 <- unique(docvars(corpus)$PMC)[166:183]

subset <- subset_12

#################################NER with quanteda
###########Create corpus
file = "/Users/pauca/OneDrive/Escritorio/Marina TFG/TMpipeline/MetaboliteNER-main/Corpus/GoldStandard.txt"

#Corpus
corpus <- read.delim(file, header = FALSE, sep = "\t", dec = ".") %>%
  rename(PMC = V1, SentID = V2, text = V3) %>%
  mutate(doc_id = paste(PMC, SentID, sep = "_")) 
#   select(doc_id, text)


corpus <- quanteda::corpus(corpus)

head(docvars(corpus))
#length(unique(docvars(corpus)$PMC)) #check

head(summary(corpus))



#only 1st PMC for analysis 
corp_first <- corpus_subset(corpus, PMC %in% subset)
head(summary(corp_first))

tok_first <- tokens(corp_first, split_hyphens = FALSE, include_docvars = TRUE)


#######################1st layer --> EXACT MATCHING

lay1 <- index(tok_first, pattern = met_vec, valuetype = "fixed", case_insensitive = TRUE)

result <- NULL
for(i in 1:nrow(lay1)){
  y <- gregexpr(lay1[i,]$pattern, corp_first[[lay1[i,]$docname]], ignore.case = TRUE)
  match <- lay1[i,]$pattern
  doc <- docvars(corp_first[lay1[i,]$docname])$PMC
  sent <- docvars(corp_first[lay1[i,]$docname])$SentID
  result <- rbind(result, tibble(start = unlist(y), term = match, PMC = doc, SentID = sent))
  
}

result <- result %>%
  mutate(start = start - 1) %>%
  mutate(term = as.character(term)) %>%
  mutate(end = start + nchar((term))) %>%
  select(PMC, SentID, start, end, term)

openxlsx::write.xlsx(result, "subset_results_exact_matching/result_subset12.xlsx")

######no match for next layer
qterms_df_first <- dfm(tok_first)
qterms_df_first <- tidy(qterms_df_first)
qterms_df_first[c("PMC", "SentID")] <- str_split_fixed(qterms_df_first$document, "_", n = 2)

wt <- qterms_df_first %>%
  filter(PMC %in% subset)%>%
  filter(str_detect(term, "[A-Za-z, ]+")) %>%
  anti_join(stop_words, by = c("term" = "word"))
nrow(wt) #892

no_match_lay1 <- anti_join(wt, result, by = "term") 


####################################LAYER 2: FUZZY MATCHING
library(RecordLinkage)
#choose similarity
simlv = 0.75

#CALCULATE TIME FOR one
wordlist_lv <- expand.grid(term = no_match_lay1$term, ref = met_vec, stringsAsFactors = FALSE) %>% 
  group_by(term) %>% 
  mutate(match_score = levenshteinSim(term, ref)) %>%
  summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])

lay2 <- wordlist_lv %>% filter(match > simlv) #serveix per desfernos de paraules curtes i tal

sim = 0.8
wordlist_jw <- expand.grid(term = lay2$term, ref = met_vec, stringsAsFactors = FALSE) %>% 
  group_by(term) %>% 
  mutate(match_score = jarowinkler(term, ref)) %>%
  summarise(match = match_score[which.max(match_score)], matched_to = ref[which.max(match_score)])

lay2 <- wordlist_jw %>% filter(match >= sim)

lay2 <- merge(no_match_lay1, lay2)

result2 <- NULL
for(i in 1:nrow(lay2)){
  y <- gregexpr(lay2[i,]$term, corp_first[[lay2[i,]$document]], ignore.case = TRUE)
  match <- lay2[i,]$term
  doc <- docvars(corp_first[lay2[i,]$document])$PMC
  sent <- docvars(corp_first[lay2[i,]$document])$SentID
  result2 <- rbind(result2, tibble(start = unlist(y), term = match, PMC = doc, SentID = sent, matched_to = lay2[i,]$matched_to))
  
}

result2 <- result2 %>%
  mutate(start = start - 1) %>%
  mutate(term = as.character(term)) %>%
  mutate(end = start + nchar((term))) %>%
  select(PMC, SentID, start, end, matched_to)


openxlsx::write.xlsx(result, "subset_results_exact_matching/result2_subset12.xlsx")



no_match_lay2 <- anti_join(no_match_lay1, lay2, by = "term")


# # new_list=dict_met$name[-which(is.na(dict_met$name))] 
# 
# 
# y <- (gregexpr("creatinine", corp_first[[1]]))
# regmatches(corp_first[[1]], y) #gives creatinine
