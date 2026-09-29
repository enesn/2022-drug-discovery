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

fig2b  <- ggplot(desc_table_firm, aes( x = fyear)) +
  geom_line(aes(y = total_nd2_drug, linetype = "Total ND2 drugs"), size = 1.5)+
  geom_line(aes(y = total_nd1_drug, linetype = "Total ND1 drugs"), size = 1.5)+
  scale_x_continuous(breaks = seq(1980, 2018, 2)) +
  labs( x = "", y = "")+ 
  geom_smooth(aes(y = total_nd1_drug), color = "black", se = F)+
  geom_smooth(aes(y = total_nd2_drug, color = "black"),se = F)+
  enes_theme


pdf("output_figures/fig2b.pdf", width = 12.17, height = 7.92)
print(fig2b)
dev.off()

fig2a <- ggplot(desc_table_firm, aes( x = fyear)) +
  geom_line(aes(y = n_patent, linetype = "Patent flow"), size = 1.5)+
  geom_line(aes(y = cumulative_patent/23, linetype = "Patent stock"), size = 1.5)+
  scale_x_continuous(breaks = seq(1980, 2018, 2)) + 
  scale_y_continuous(

    # Features of the first axis
    name = "Flow",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*23, name ="Stock")
  )+
  labs(x = "",y = "")+ 
  enes_theme


pdf("output_figures/fig2a.pdf", width = 12.17, height = 7.92)
print(fig2a)
dev.off()

fig2d <- ggplot(desc_table_firm, aes( x = fyear)) +
  geom_line(aes(y = n_nd1_perBillionRD, linetype = "ND1 per billion USD of R&D"), size = 1.5) + 
  geom_line(aes(y = n_nd2_perBillionRD, linetype = "ND2 per billion USD of R&D"), size = 1.5) + 
  # geom_line(aes(y = avg_nda_perBillionRD*45, color = " Average drugs per billion USD"), size = 1.5)+
  scale_x_continuous(breaks = seq(1980, 2018, 2)) + 
  # scale_y_continuous(
  #   
  #   # Features of the first axis
  #   name = "#Drugs per billion USD (flow)",
  #   
  #   # Add a second axis and specify its features
  #   sec.axis = sec_axis(~./45, name ="#Drugs per billion USD /#Firms")
  # )+ 
  labs(x = "",y = "")+ 
  enes_theme

pdf("output_figures/fig2d.pdf", width = 12.17, height = 7.92)
print(fig2d)
dev.off()

fig2c <- ggplot(desc_table_firm, aes( x = fyear)) +
  geom_line(aes(y = nd1_per_cumpatent, linetype = "ND1 per patents (cumulative)"), size = 1.5)+
  geom_line(aes(y = nd2_per_cumpatent, linetype = "ND2 drugs per patents (cumulative)"), size = 1.5)+
  scale_x_continuous(breaks = seq(1980, 2018, 2)) +
  # scale_y_continuous(
  #   
  #   # Features of the first axis
  #   name = "#Cumulative new drug approvals / # Cumulative patents",
  #   
  #   # Add a second axis and specify its features
  #   sec.axis = sec_axis(~./55, name ="#Cumulative new drug approvals / # Cumulative patents /  #Firms")
  # )+ 
  labs(x = "",y = "")+ 
  enes_theme


pdf("output_figures/fig2c.pdf", width = 12.17, height = 7.92)
print(fig2c)
dev.off()
  
