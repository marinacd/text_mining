
#Results


#load annotation
file = "/Users/pauca/OneDrive/Escritorio/Marina TFG/TMpipeline/MetaboliteNER-main/Corpus/GoldStandardAnnot.xlsx"

gold_standard_ann <- read_excel(file, col_names = FALSE ) %>%
  rename(PMC = 1, SentID = 2, start = 3, stop = 4, term = 5)
