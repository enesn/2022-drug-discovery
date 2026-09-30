## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#

sic <- read_dta("raw-input-data/SICtoGVKEY_crosswalk.dta")

## =================== Patent Data Cleaining ==================================================#

#Read patentview.org data
patent<- read_csv("raw-input-data/clean_patent_data.csv") %>% 
  select(-nber_subcategory, -nber_category)
patent$number<-as.character(patent$number)


#Clean wrong PATENT-ASSIGNEE GVKEY matches 
patent <- dplyr::mutate(patent, org_str = substr(assignee_organization, 1, 28))

patent$assignee_gvkey <- ifelse(patent$org_str == "THE UNITED STATES OF AMERICA", NA, patent$assignee_gvkey)
patent$assignee_gvkey <- ifelse(patent$assignee_organization == "SHARP KABUSHIKI KAISHA", NA, patent$assignee_gvkey)
patent$assignee_gvkey <- ifelse(patent$org_str == "THE SECRETARY OF STATE FOR D", NA, patent$assignee_gvkey)
patent$assignee_gvkey <- ifelse(patent$assignee_organization == "MONSANTO TECHNOLOGY", 	140760, patent$assignee_gvkey)

#Drop patents without assignee_gvkey 
patent_wGVKEY <- patent %>% filter(!is.na(assignee_gvkey))
patent_wGVKEY$assignee_gvkey <- as.character(patent_wGVKEY$assignee_gvkey)
patent_wGVKEY<- left_join(patent_wGVKEY, sic, by = c("assignee_gvkey" = "gvkey"))

#Call NBER patent categories
nber <- as.data.frame(fread("raw-input-data/patentview_nber.tsv", quote =  ""))  
nber_cat <- as.data.frame(fread("raw-input-data/patentview_nber_category.tsv", quote =  ""))  
nber_subcat <- as.data.frame(fread("raw-input-data/patentview_nber_subcategory.tsv", quote =  ""))  
nber <- left_join(nber, nber_cat, by = c("category_id" ="id"))
nber <- left_join(nber, nber_subcat, by = c("subcategory_id" ="id"))
nber <- nber %>% dplyr::rename(nber_category = title.x, nber_subcategory = title.y) %>%
  select(patent_id, nber_category, nber_subcategory)
nber$patent_id <- as.character(nber$patent_id)

patent_wGVKEY$number <- as.character(patent_wGVKEY$number)
patent$number <- as.character(patent$number)
patent_wGVKEY <- left_join(patent_wGVKEY, nber, by = c("number" = "patent_id"))

#Select patents according to SIC code
crsp_pharma_patent <- patent_wGVKEY %>% filter(sic == 2834  |  sic == 2835  |sic == 2836) %>%
  distinct(number, .keep_all = T)


