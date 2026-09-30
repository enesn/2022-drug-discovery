## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#

desc_table_firm <- all_drug_producing %>%
  dplyr::group_by(fyear) %>%
  dplyr::filter(top50_atleastonce == T)  %>%
  dplyr::summarise(total_drug = sum(n_drug, na.rm = T))  %>%
  left_join(
    all_drug_producing %>%
      dplyr::filter(top50_atleastonce == T)  %>%
      dplyr::filter(drug_class == "NME_NCE" | drug_class == "METOO") %>%
      dplyr::group_by(fyear) %>%
      dplyr::summarise(total_nd1_drug = sum(n_drug, na.rm = T)), by =  "fyear"
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr::filter(top50_atleastonce == T)  %>%
      dplyr::filter(drug_class == "NME_NCE") %>%
      dplyr::group_by(fyear) %>%
      dplyr::summarise(total_nd2_drug = sum(n_drug, na.rm = T)), by =  "fyear"
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr::filter(top50_atleastonce == T)  %>%
      dplyr::group_by(fyear, gvkey) %>%
      dplyr::summarise(n_obs = n()) %>%
      dplyr::group_by(fyear) %>%
      dplyr::summarise(n_firm = n()), by =  "fyear"
    
  ) %>%
  left_join(all_drug_producing %>% 
              dplyr::filter(top50_atleastonce == T)  %>%
              dplyr::group_by(gvkey, fyear) %>% 
              dplyr::summarise(pat = min(n_patent)) %>% 
              dplyr::group_by(fyear) %>% 
              dplyr::summarise(n_patent = sum(pat, na.rm = T)), by =  "fyear"
  ) %>%
  left_join(
  pharma_crsp%>% 
    dplyr::filter(top50_atleastonce == T)  %>%
    dplyr::group_by(fyear)%>%
    dplyr::summarise(xrd = sum(xrd , na.rm = T)), by = "fyear"
  )%>%
  mutate(
    n_nd1_perBillionRD = (total_nd1_drug / (xrd*1000000))*1000000000,
    n_nd2_perBillionRD = (total_nd2_drug / (xrd*1000000))*1000000000,
    cumulative_patent = cumsum(n_patent),
    cumulative_nd1_drug = cumsum(total_nd1_drug),
    cumulative_nd2_drug = cumsum(total_nd2_drug),
    nd1_per_cumpatent  = cumulative_nd1_drug / cumulative_patent,
    nd2_per_cumpatent  = cumulative_nd2_drug / cumulative_patent,
    avg_patent = n_patent  / n_firm,
    avg_nd1 = total_nd1_drug / n_firm,
    avg_drug = total_drug / n_firm,
    avg_nd1_perBillionRD = ( (total_nd1_drug/n_firm) / ((xrd*1000000))*1000000000),
    avg_drug_per_cumpatent  = (cumulative_nd1_drug) / (cumulative_patent) /n_firm
  )%>%
  select(fyear, total_drug, total_nd2_drug,total_nd1_drug,
         n_patent, n_nd1_perBillionRD, cumulative_patent, n_nd2_perBillionRD,
         nd1_per_cumpatent, nd2_per_cumpatent)
