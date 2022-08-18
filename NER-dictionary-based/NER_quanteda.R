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
corp_first <- corpus_subset(corpus, PMC == "PMC2267737")
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

######no match for next layer
qterms_df_first <- dfm(tok_first)
qterms_df_first <- tidy(qterms_df_first)
qterms_df_first[c("PMC", "SentID")] <- str_split_fixed(qterms_df_first$document, "_", n = 2)

wt <- qterms_df_first %>%
    filter(PMC == "PMC2267737")%>%
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
  result2 <- rbind(result2, tibble(start = unlist(y), term = match, PMC = doc, SentID = sent))
  
}

result2 <- result2 %>%
  mutate(start = start - 1) %>%
  mutate(term = as.character(term)) %>%
  mutate(end = start + nchar((term))) %>%
  select(PMC, SentID, start, end, term)
  #FUCK YESSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 

nrow(result) + nrow(result2) #66 de 72 hello??????


no_match_lay2 <- anti_join(no_match_lay1, lay2, by = "term")
#si no son metabolits ens son igual!! --> pero els hem de contar igual? els errors son importants tambe



#AQUI PODEM FER ALGO PER SEPARAR ELS MATCHES DELS NO MATCHES ????
#si contenten lletra grega --> probably match

#also algo a fer amb els prefixos --> d-lactate and lactate arent matching --> maybe try jarowinkler then?

jarowinkler("lactate", "d-lactate") #0.925  #DO A LAYER WITH JAROWINKLER????


#RECORDA A L'HORA DE GENERALITZAR Q NO CAL FERHO AMB TOT EL CORPUS, MAYBE NOMES UNS QUANTS




# # new_list=dict_met$name[-which(is.na(dict_met$name))] 
# 
# 
# y <- (gregexpr("creatinine", corp_first[[1]]))
# regmatches(corp_first[[1]], y) #gives creatinine
