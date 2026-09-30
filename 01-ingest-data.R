
## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# January 2021
# EI
## ===========================================================================================#

## ==========================  Compustat entire pharma sample   ==============================#

pharma_crsp <- read_dta("raw-input-data/pharma_compustat.dta") %>% #SIC 2834, 35, 36
  dplyr:: filter(fyear > 1979) 

pharma_crsp$gvkey <- sub("^0+\\B", "", pharma_crsp$gvkey) #remove leading zeros in gvkey

## =======START============  Towards all drug producing firms   =========START=================#

## ==================================  FDA data ===============================================#

#Import FDA drug data
source("raw-input-data/FDA_drug_data/FDAsponsor_name_cleaning.R")

#Unify NAs, N/As, and UNKNOWNS
fda_drug <- fda_drug %>% 
  dplyr:: mutate(SubmissionClassCode = 
                   if_else(
                     is.na(SubmissionClassCode), "UNKNOWN", SubmissionClassCode)
                 )
fda_drug <- fda_drug %>% 
  dplyr:: mutate(SubmissionClassCode = 
                   if_else(
                     SubmissionClassCode == "N/A", "UNKNOWN", SubmissionClassCode)
                 )
fda_drug <- fda_drug %>% 
  dplyr:: mutate(ReviewPriority = if_else(
    is.na(ReviewPriority), "UNKNOWN", ReviewPriority)
    )

#Match drugs with Compustat GVKEY
source("raw-input-data/FDA_drug_data/FDA_handCRSPmatching.R")

  #Number of original drug applications 
n_drug <-  fda_drug %>%
  # To distinguish original (ORIG) NDA submissions with supplemental applications for labelling etc.
  dplyr:: filter(SubmissionType == "ORIG") %>% 
  # To capture initial original drug application 
  dplyr::group_by(ApplNo) %>% 
  slice(which.min(sub_year)) %>% 
  dplyr::group_by(gvkey, 
           sub_year, #approval year
           SubmissionClassCode, 
           ApplType) %>%
  dplyr::summarise(n_drug=n())

n_drug$drug_class <- ifelse(
  n_drug$SubmissionClassCode == "TYPE 1" & n_drug$ApplType == "NDA"| 
    n_drug$SubmissionClassCode == "TYPE 2" & n_drug$ApplType == "NDA" |
    n_drug$SubmissionClassCode == "TYPE 1" & n_drug$ApplType == "BLA" | 
    n_drug$SubmissionClassCode == "TYPE 2" & n_drug$ApplType == "BLA","NME_NCE",
  ifelse(
    n_drug$SubmissionClassCode == "TYPE 3" & n_drug$ApplType == "NDA" |   
    n_drug$SubmissionClassCode == "TYPE 4"& n_drug$ApplType == "NDA" |
    n_drug$SubmissionClassCode == "TYPE 1/4" & n_drug$ApplType == "NDA" |
    n_drug$SubmissionClassCode == "TYPE 5" & n_drug$ApplType == "NDA"|
    n_drug$SubmissionClassCode == "TYPE 2/3" & n_drug$ApplType == "NDA"| 
    n_drug$SubmissionClassCode == "TYPE 2/4" & n_drug$ApplType == "NDA"|
    n_drug$SubmissionClassCode == "TYPE 3/4" & n_drug$ApplType == "NDA"|
    n_drug$SubmissionClassCode == "TYPE 4/5" & n_drug$ApplType == "NDA"|
      n_drug$SubmissionClassCode == "TYPE 1" & n_drug$ApplType == "BLA" |  
      n_drug$SubmissionClassCode == "TYPE 2" & n_drug$ApplType == "BLA" |   
      n_drug$SubmissionClassCode == "TYPE 3" & n_drug$ApplType == "BLA" |   
      n_drug$SubmissionClassCode == "TYPE 4"& n_drug$ApplType == "BLA" |
      n_drug$SubmissionClassCode == "TYPE 1/4" & n_drug$ApplType == "BLA"|
      n_drug$SubmissionClassCode == "TYPE 5" & n_drug$ApplType == "BLA"|
      n_drug$SubmissionClassCode == "TYPE 2/3" & n_drug$ApplType == "BLA"| 
      n_drug$SubmissionClassCode == "TYPE 2/4" & n_drug$ApplType == "BLA"|
      n_drug$SubmissionClassCode == "TYPE 3/4" & n_drug$ApplType == "BLA"|
      n_drug$SubmissionClassCode == "TYPE 4/5" & n_drug$ApplType == "BLA","METOO",
  ifelse(
  #Not ND  
    n_drug$SubmissionClassCode == "TYPE 6" & n_drug$ApplType == "NDA"|
      n_drug$SubmissionClassCode == "TYPE 9- BLA" & n_drug$ApplType == "NDA"|
      n_drug$SubmissionClassCode == "TYPE 7" & n_drug$ApplType == "NDA"|
      n_drug$SubmissionClassCode == "TYPE 8" & n_drug$ApplType == "NDA"|
      n_drug$SubmissionClassCode == "TYPE 9" & n_drug$ApplType == "NDA" |
      n_drug$SubmissionClassCode == "TYPE 10" & n_drug$ApplType == "NDA" |
      n_drug$SubmissionClassCode == "EFFICACY" & n_drug$ApplType == "NDA" |
      n_drug$SubmissionClassCode == "LABELING" & n_drug$ApplType == "NDA" |
      n_drug$SubmissionClassCode == "MANUF (CMC)" & n_drug$ApplType == "NDA" |
      n_drug$SubmissionClassCode == "MEDGAS" & n_drug$ApplType == "NDA" |
      n_drug$SubmissionClassCode == "MANUF (CMC)" & n_drug$ApplType == "NDA" |
      n_drug$SubmissionClassCode == "TYPE 6" & n_drug$ApplType == "BLA"|
      n_drug$SubmissionClassCode == "TYPE 9- BLA" & n_drug$ApplType == "BLA"|
      n_drug$SubmissionClassCode == "TYPE 7" & n_drug$ApplType == "BLA"|
      n_drug$SubmissionClassCode == "TYPE 8" & n_drug$ApplType == "BLA"|
      n_drug$SubmissionClassCode == "TYPE 9" & n_drug$ApplType == "BLA"| 
      n_drug$SubmissionClassCode == "TYPE 10" & n_drug$ApplType == "BLA"| 
      n_drug$SubmissionClassCode == "EFFICACY" & n_drug$ApplType == "BLA" |
      n_drug$SubmissionClassCode == "LABELING" & n_drug$ApplType == "BLA" |
      n_drug$SubmissionClassCode == "MANUF (CMC)" & n_drug$ApplType == "BLA" |
      n_drug$SubmissionClassCode == "MEDGAS" & n_drug$ApplType == "BLA" |
      n_drug$SubmissionClassCode == "MANUF (CMC)" & n_drug$ApplType == "BLA","NOTND",
    ifelse(n_drug$SubmissionClassCode == "UNKNOWN", NA, F)
    ) )
  )

## ==================================  Patent data   =======================================#

#Import Patent data 
source("02-data-cleaning.R")


kpss_2020_koganetal <- read_csv("raw-input-data/KPSS_2020_public.csv")  %>%
  dplyr::mutate(patent_num =  as.character(patent_num)) %>%
  select(-issue_date, -filing_date, -permno)

crsp_pharma_patent <- crsp_pharma_patent %>% left_join(kpss_2020_koganetal, 
                                                       by = c("number" = "patent_num")
                                                  )

#number of patent numbers 
n_patent <- crsp_pharma_patent %>% 
  dplyr::group_by(assignee_gvkey, grant_year) %>%
  dplyr::summarise(n_patent = n(), 
                   n_citations = sum(cites, na.rm = T),
                   total_xi = sum(xi_real, na.rm = T), 
  ) %>% dplyr::mutate(
    avg_cite = n_citations / n_patent, 
    avg_xi  = total_xi / n_patent, 
  )

## ===================================  Matching  ===========================================#

matched <- pharma_crsp %>%
  #match drugs
  left_join(
    n_drug,
    by = c("gvkey","fyear" = "sub_year")
  ) %>%
  #match patents
  left_join(
    n_patent,
    by = c("gvkey" = "assignee_gvkey", "fyear" = "grant_year")
  )

## =================================== A final cleaning  ======================================#

# Identify drug producers
is_producer <- n_drug %>%
  group_by(gvkey) %>% 
  dplyr:: filter(drug_class == "NME_NCE" | 
                   drug_class == "METOO") %>%
  dplyr::summarise(n_drug = n()) %>% 
  dplyr:: select(gvkey) %>% 
  dplyr:: filter(!is.na(gvkey)) %>% 
  dplyr:: mutate(drug_producer = T)

matched <- left_join(matched, is_producer, by = "gvkey") 

all_drug_producing <- matched %>% dplyr::filter(drug_producer == T)

all_drug_producing_US <- all_drug_producing %>% 
  dplyr:: filter(fic == "USA")

# #quantitative representativeness
# all_drug_producing %>%
#   filter(fyear == 2018) %>%
#   distinct(gvkey, .keep_all = T)  %>%
#   summarise(sale = sum(sale, na.rm = T)*1000000)
# 
# 
# all_drug_producing_US %>%
#   filter(fyear == 2018) %>%
#   distinct(gvkey, .keep_all = T)  %>%
#   summarise(sale = sum(sale, na.rm = T)*1000000)

## =======END=====================   All drug producing firms   ===========END=================#
## =============================================================================================
## =============================================================================================

## ===================     Identify  Top50 firms   ============================================#

pharma_crsp <- left_join(pharma_crsp, is_producer, by = "gvkey") 

pharma_crsp50 <- pharma_crsp %>%
  dplyr:: filter(drug_producer == T) %>%
  dplyr:: group_by(fyear) %>%
  top_n(50, sale) 

is_top50once <- pharma_crsp50 %>% 
  dplyr:: group_by(gvkey) %>% 
  dplyr:: summarise(n=n()) %>% 
  dplyr::select(gvkey)  %>% 
  dplyr:: mutate(top50_atleastonce = T)

pharma_crsp <- left_join(pharma_crsp, is_top50once, by = "gvkey")

all_drug_producing <- left_join(all_drug_producing, is_top50once, by = "gvkey")
all_drug_producing_US <- left_join(all_drug_producing_US, is_top50once, by = "gvkey")

balanced <- pharma_crsp %>% 
  dplyr::group_by(gvkey,conm) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(existing_allyears = ifelse(n == 39, T, F),
         existing_20years  = ifelse(n > 19, T, F ),
         existing_10years  = ifelse(n > 9, T, F ) ) %>% 
  select(gvkey, existing_allyears, existing_20years, existing_10years) 

pharma_crsp <- left_join(pharma_crsp, balanced, by = "gvkey")
all_drug_producing <- left_join(all_drug_producing, balanced, by = "gvkey")
all_drug_producing_US <- left_join(all_drug_producing_US, balanced, by = "gvkey")

#Create free resource for RAM
rm(sic, patent_wGVKEY, patent, nber_cat, nber_subcat, nber, 
   fda_submissions, fda_applications, fda_applications_docs, fda_products,
   fda_subclass, matched)




#Representativeness

print(pharma_crsp %>%
       filter(drug_producer == T) %>%
       filter(top50_atleastonce == T) %>%
       dplyr::group_by(fyear) %>%
       dplyr::summarise(total.sales = sum(sale, na.rm = T)))

temp <- pharma_crsp %>% 
  filter(drug_producer == T) %>%
  filter(top50_atleastonce == T) 

length(unique(temp$gvkey))
nrow(temp)

# ## ===================    FDA and Compustat differences   ============================================#
# temp_ <- filter(fda_drug, sub_year > 1979 & sub_year < 2019 & SubmissionType == "ORIG" & ApplType == "NDA")
# temp <- fda_drug %>% filter(sub_year > 1979 & sub_year < 2019) %>%
#   filter(SubmissionType == "ORIG") %>% 
#   filter(ApplType == "NDA") %>% 
#   # To capture initial original drug application 
#   group_by(ApplNo) %>% slice(which.min(sub_year)) %>% 
#   group_by(SponsorName, SubmissionClassCode) %>%
#   summarise(n_drug=n()) %>%
#   left_join(
#     fda_drug %>% distinct(SponsorName, .keep_all = T) %>% select(SponsorName,gvkey),
#     by = "SponsorName"
#   )
# 
# tmp <- filter(temp, is.na(gvkey))
# tmp2 <- tmp %>% filter(SubmissionClassCode == "TYPE 1")
