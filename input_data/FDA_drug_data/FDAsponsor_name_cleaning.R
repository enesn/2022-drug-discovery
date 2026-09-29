
# combine the splitted FDA Drug data 
fda_submissions <- read_tsv("input_data/FDA_drug_data/FDA_Submissions.txt") 
fda_applications <- read_tsv("input_data/FDA_drug_data/FDA_Applications.txt")  
fda_applications_docs <- read_tsv("input_data/FDA_drug_data/FDA_ApplicationDocs.txt")  %>% select(ApplNo, ApplicationDocsDate)
fda_subclass <- read_tsv("input_data/FDA_drug_data/FDA_SubmissionClass_Lookup.txt")
fda_products <- read_tsv("input_data/FDA_drug_data/FDA_Products.txt")

fda_drug <- left_join(fda_applications, fda_products, by = "ApplNo") %>% 
  left_join(fda_applications_docs, by = "ApplNo") %>%
  left_join(fda_submissions, by = "ApplNo") %>% 
  left_join(fda_subclass, by = "SubmissionClassCodeID")

fda_drug$sub_year <- year(fda_drug$SubmissionStatusDate)

#Drugname standardization 
fda_drug$SponsorName <- clean_chr(fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MERCK SHARP DOHME", "MERCK", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MSD MERCK CO", "MERCK", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ABBVIE ENDOCRINE INC", "ABBVIE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ABBVIE ENDOCRINE", "ABBVIE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ABBOTT LABS", "ABBOTT", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ABRAXIS BIOSCIENCE", "ABRAXIS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ABRAXIS PHARM", "ABRAXIS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACACIA PHARMA", "ACACIA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACACIA PHARMS", "ACACIA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACELLA PHARMS", "ACELLA", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACTAVIS ELIZABETH", "ACTAVIS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACTAVIS LABS", "ACTAVIS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACTAVIS LABS FL", "ACTAVIS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACTAVIS LABS UT", "ACTAVIS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACTAVIS MID ATLANTIC", "ACTAVIS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ACTAVIS TOTOWA", "ACTAVIS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AHARMA US PHARMS", "AHARMA PHARMS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AHARMA US PHARMS", "AHARMA PHARMS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AHARMA PHARMS", "AHARMA", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AKORN MFG", "AKORN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ALCON RES", "ALCON", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ALKERMES GAINESVILLE", "ALKERMES", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ALLERGAN HOLDINGS", "ALLERGAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AMNEAL PHARMS", "AMNEAL", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ALCON PHARMS LTD", "ALCON", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ALCON PHARMS", "ALCON", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ALCON LABS", "ALCON", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ALLERGAN HERBERT", "ALLERGAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ALCON PHARMA", "ALCON", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ASTRAZENECA AB", "ASTRAZENECA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ASTRAZENECA UK", "ASTRAZENECA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ASTRAZENECA PHARMS", "ASTRAZENECA", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AMAG PHARMA USA", "AMAG PHARMS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AMNEAL PHARM", "AMNEAL PHARMS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AMNEAL PHARMS CO", "AMNEAL PHARMS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AMNEAL PHARMS NY", "AMNEAL PHARMS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "AMPHASTAR PHARM", "AMPHASTAR PHARMS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "APOTEX TECHNOLOGIES", "APOTEX", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BAUSCH AND LOMB", "BAUSCH", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BAYER HEALTHCARE PHARMS", "BAYER", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BAYER PHARMS", "BAYER", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BAYER HEALTHCARE", "BAYER", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BAYER HLTHCARE", "BAYER", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BECTON DICKINSON CO", "BECTON DICKINSON", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BIOVAIL LABS INTL", "BIOVAIL LABS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BRAINTREE LABS", "BRAINTREE", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BRECKENRIDGE PHARM", "BRECKENRIDGE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BRISTOL MYERS", "BRISTOL MYERS SQUIBB", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CHARTWELL PHARMA", "CHARTWELL", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CHARTWELL RX", "CHARTWELL", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CHIESI USA", "CHIESI", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CIPLA USA", "	CIPLA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CIS BIO INTL SA", "CIS BIO INTL", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CLINIGEN HLTHCARE", "CLINIGEN", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "COLGATE", "COLGATE PALMOLIVE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "COLGATE PALMOLIVE CO", "COLGATE PALMOLIVE", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CONCORDIA PHARMS", "CONCORDIA", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BRECKENRIDGE PHARMS", "BRECKENRIDGE", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "B BRAUN MEDICAL", "B BRAUN", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BARR LABS", "BARR", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BARR LABS DIV TEVA", "BARR", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BIOMARIN PHARM", "BIOMARIN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CELGENE CORP", "CELGENE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CELGENE INTL", "CELGENE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "BIOGEN", "BIOGEN IDEC", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "DR REDDYS LA", "DR REDDYS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "DR REDDYS LABS", "DR REDDYS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "DR REDDYS LABS SA", "DR REDDYS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "DR REDDYS LA", "DR REDDYS", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CENTOCOR ORTHO BIOTECH", "CENTOCOR", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "CHIESI USA", "CHIESI USA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ENDO PHARM", "ENDO PHARMS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "FERRING PHARMS", "FERRING", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "EMCURE PHARMA", "EMCURE PHARMS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "EXELA PHARMA SCS", "EXELA PHARMA SCIENCE", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "FRESENIUS", "FRESENIUS KABI USA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "FRESENIUS MEDCL", "FRESENIUS KABI USA", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "GALDERMA R AND D", "GALDERMA LABS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "GE HLTHCARE", "GE HEALTHCARE", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "GENUS", "GENUS LIFESCIENCES", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "GLAXO GRP ENGLAND", "GLAXO GRP", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "GLAXOSMITHKLINE CON", "GLAXOSMITHKLINE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "GLAXOSMITHKLINE CONS", "GLAXOSMITHKLINE", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HERITAGE PHARMA", "HERITAGE PHARMS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HERITAGE LIFE", "HERITAGE PHARMS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HIKMA FARMACEUTICA", "HIKMA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HIKMA INTL PHARMS", "HIKMA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HIKMA PHARMS", "HIKMA", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HOFFMANLA ROCHE", "HOFFMANN LA ROCHE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HOFFMANNLA ROCHE", "HOFFMANN LA ROCHE", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HQ SPECIALITY PHARMA", "HQ SPCLT PHARMA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "HQ SPECLT PHARMA", "HQ SPCLT PHARMA", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "IMPAX", "IMPAX LABS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "IMPAX PHARMS", "IMPAX LABS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "J AND J", "JOHNSON AND JOHNSON", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "J AND J CONSUMER", "JOHNSON AND JOHNSON", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "JANSSEN PHARMA", "JANSSEN BIOTECH", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "JANSSEN PHARMS", "JANSSEN BIOTECH", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "JANSSEN PRODS", "JANSSEN BIOTECH", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "JANSSEN R AND D", "JANSSEN BIOTECH", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "JANSSEN RES AND DEV", "JANSSEN BIOTECH", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "JANSSEN THERAP", "JANSSEN BIOTECH", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "JAZZ", "JAZZ PHARMS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LANTHEUS MEDICAL", "LANTHEUS MEDCL", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LEO PHARMA AS", "LEO PHARMA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LANNETT CO", "LANNETT", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LUPIN ATLANTIS", "LUPIN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LUPIN PHARMS", "LUPIN", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ELI LILLY AND CO", "LILLY", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ELI LILLY CO", "LILLY", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LILLY RES LABS", "LILLY", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LUNDBECK PHARMS", "LUNDBECK", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LUNDBECK NA", "LUNDBECK", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "LUNDBECK SEATTLE BIOPHARMACEUTICALS", "LUNDBECK", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MALLKRODT HOSP", "MALLKRODT", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MALLKRODT ARD", "MALLKRODT", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MCNEIL CONS", "MCNEIL CONSUMER", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MSD MERCK CO", "MERCK", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MERCK AND CO", "MERCK", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN INSTITUTIONAL", "MYLAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN IRELAND", "MYLAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN SPECIALITY", "MYLAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN LABS", "MYLAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN ASI", "MYLAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN PHARMA", "MYLAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN PHARMS", "MYLAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN SPECLT", "MYLAN", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MYLAN TECHNOLOGIES", "MYLAN", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MAYNE PHARMA INTL", "MAYNE PHARMA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "MERIDIAN MEDCL TECHN", "MERIDIAN MEDCL", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "NOVARTIS PHARMS", "NOVARTIS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "NOVARTIS PHARM", "NOVARTIS", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "NOVO NORDISK", "NOVO", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "OAK PHARMS AKORN", "OAK PHARMS", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ORTHO MCNEIL", "ORTHO MCNEIL PHARM", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ORTHO MCNEIL JANSSEN", "ORTHO MCNEIL PHARM", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "OTSUKA", "OTSUKA PHARM", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "OTSUKA PHARM CO", "OTSUKA PHARM", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "OTSUKA AMERICA", "OTSUKA PHARM", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "OTSUKA AMERICA PHARM", "OTSUKA PHARM", fda_drug$SponsorName)



fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "PFIZER IRELAND", "PFIZER", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "PFIZER PHARMS", "PFIZER", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "PHARMACIA UPJOHN", "PHARMACIA AND UPJOHN", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "ROCHE PALO", "ROCHE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SALIX PHARMS", "SALIX", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SANOFI AVENTIS", "SANOFI", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SANOFI AVENTIS US", "SANOFI", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SANOFI SYNTHELABO", "SANOFI", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SANOFIAVENTIS US", "SANOFI", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SANOFI US SERVICES", "SANOFI", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SANOFI US", "SANOFI", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SHIRE DEV", "SHIRE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SHIRE DEVELOPMENT", "SHIRE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SHIRE HUMAN GENETIC", "SHIRE", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SHIRE ORPHAN THERAP", "SHIRE", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SUN PHARMA GLOBAL", "SUN PHARMA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SUN PHARM INDS", "SUN PHARMA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SUN PHARM", "SUN PHARMA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SUN PHARM INDUSTRIES", "SUN PHARMA", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "SCHERING", "SCHERING PLOUGH", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "TEVA BRANDED PHARM", "TEVA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "TEVA PARENTERAL", "TEVA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "TEVA PHARMS INTL", "TEVA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "TEVA PRESPIRATORY", "TEVA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "TEVA WOMENS", "TEVA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "TEVA PHARMS USA", "TEVA", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "TEVA PHARMS", "TEVA", fda_drug$SponsorName)


fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "US PHARMS HOLDINGS I", "US PHARMS HOLDINGS", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "VALEANT INTL", "VALEANT", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "VALEANT LUXEMBOURG", "VALEANT", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "VALEANT PHARM INTL", "VALEANT", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "VALEANT PHARMS", "VALEANT", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "VALEANT PHARMS NORTH", "VALEANT", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "WYETH AYERST", "WYETH", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "WYETH PHARMS", "WYETH", fda_drug$SponsorName)

fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "3M DRUG DELIVERY", "3M", fda_drug$SponsorName)
fda_drug$SponsorName <- ifelse(fda_drug$SponsorName == "3M HEALTH CARE", "3M", fda_drug$SponsorName)



