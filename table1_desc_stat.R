## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#

## ======================== Number of drugs and patents relations, overall ===================#

pharma_crsp$period <- ifelse(pharma_crsp$fyear > 1979 & pharma_crsp$fyear < 1990, 1, 
                             ifelse(pharma_crsp$fyear > 1989 & pharma_crsp$fyear < 2000, 2,
                                    ifelse(pharma_crsp$fyear > 1999 & pharma_crsp$fyear < 2010,3, 4)))

pharma_crsp$period2 <- ifelse(pharma_crsp$fyear > 1979 & pharma_crsp$fyear < 1986, 1, 
                             ifelse(pharma_crsp$fyear > 1985 & pharma_crsp$fyear < 1992, 2,
                                    ifelse(pharma_crsp$fyear > 1991 & pharma_crsp$fyear < 1998,3, 
                                           ifelse(pharma_crsp$fyear > 1997 & pharma_crsp$fyear < 2004,4,
                                                  ifelse(pharma_crsp$fyear > 2003 & pharma_crsp$fyear < 2010,5,6)))))


all_drug_producing$period <- ifelse(all_drug_producing$fyear > 1979 & all_drug_producing$fyear < 1990, 1, 
                             ifelse(all_drug_producing$fyear > 1989 & all_drug_producing$fyear < 2000, 2,
                                    ifelse(all_drug_producing$fyear > 1999 & all_drug_producing$fyear < 2010,3, 4)))


all_drug_producing$period2 <- ifelse(all_drug_producing$fyear > 1979 & all_drug_producing$fyear < 1986, 1, 
                              ifelse(all_drug_producing$fyear > 1985 & all_drug_producing$fyear < 1992, 2,
                                     ifelse(all_drug_producing$fyear > 1991 & all_drug_producing$fyear < 1998,3, 
                                            ifelse(all_drug_producing$fyear > 1997 & all_drug_producing$fyear < 2004,4,
                                                   ifelse(all_drug_producing$fyear > 2003 & all_drug_producing$fyear < 2010,5,6)))))

desc_table_firm_i <- pharma_crsp %>%
  dplyr::filter(drug_producer == T)  %>%
  dplyr::filter(top50_atleastonce == T)  %>%
  dplyr:: group_by(period) %>% 
  dplyr:: summarise(
            ib = sum(ib, na.rm = T), 
            ni = sum(ni, na.rm = T), 
            at = sum(at, na.rm = T),
            ppent= sum(ppent, na.rm = T),
            xrd= sum(xrd, na.rm = T), 
            xrd.avg  = mean(xrd, na.rm = T),
            sale= sum(sale, na.rm = T), 
            dv  = sum(dv, na.rm = T), 
            ppent = sum(ppent, na.rm = T), 
            oibdp= sum(oibdp, na.rm = T), 
            txt= sum(txt, na.rm = T), 
            prstkc= sum(prstkc, na.rm = T)
            ) %>% 
  left_join(
    all_drug_producing %>%
      dplyr:: filter(top50_atleastonce == T)  %>%
      dplyr:: filter(drug_class == "NME_NCE" | drug_class == "METOO")  %>%
      dplyr:: group_by(period) %>%
      dplyr:: summarise(total_nd1 = sum(n_drug, na.rm = T)) %>%
      mutate(n.year = c(10,10,10,9), avg_nd1_drug = total_nd1 / n.year), 
    by = c("period")
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr::  filter(top50_atleastonce == T)  %>%
      dplyr::  group_by(gvkey, conm, fyear, period)  %>%
      dplyr::  summarise(n_patent = min(n_patent)) %>%
      dplyr::  group_by(period) %>%
      dplyr::summarise(total_patent = sum(n_patent, na.rm = T)) %>%
      mutate(n.year = c(10,10,10,9), avg_patent = total_patent / n.year),
    by = c("period")
  ) %>% 
  left_join(
  all_drug_producing %>%
  group_by(period) %>%
  filter(top50_atleastonce == T)  %>%
    dplyr:: filter(drug_class == "NME_NCE")%>%
 dplyr:: summarise(total_nd2 = sum(n_drug, na.rm = T)) %>%
   mutate(n.year = c(10,10,10,9), avg_nd2_drug = total_nd2 / n.year), 
  by = c("period")
  ) %>%
  dplyr:: mutate(
                 RD = xrd / (oibdp-txt),
                 profit = (oibdp - txt) / ppent,
                 n_nd1_perBillionRD = (avg_nd1_drug / (xrd.avg*1000000))*1000000000,
                 n_nd2_perBillionRD = (avg_nd2_drug / (xrd.avg*1000000))*1000000000,
                 shareholder_payment 	=(prstkc+dv)/(oibdp-txt)) %>% 
  dplyr::  select(
    period, profit, RD, shareholder_payment, 
    avg_patent, avg_nd1_drug, avg_nd2_drug, n_nd2_perBillionRD,n_nd1_perBillionRD, 
        )  

desc_table_firm_ii <- pharma_crsp %>%
  dplyr::filter(drug_producer == T)  %>%
  dplyr::filter(fic == "USA") %>%
  dplyr::filter(top50_atleastonce == T)  %>%
  dplyr:: group_by(period) %>% 
  dplyr:: summarise(
    ib = sum(ib, na.rm = T), 
    ni = sum(ni, na.rm = T), 
    at = sum(at, na.rm = T),
    ppent= sum(ppent, na.rm = T),
    xrd= sum(xrd, na.rm = T), 
    xrd.avg  = mean(xrd, na.rm = T),
    sale= sum(sale, na.rm = T), 
    dv  = sum(dv, na.rm = T), 
    ppent = sum(ppent, na.rm = T), 
    oibdp= sum(oibdp, na.rm = T), 
    txt= sum(txt, na.rm = T), 
    prstkc= sum(prstkc, na.rm = T)
  ) %>% 
  left_join(
    all_drug_producing %>%
      dplyr:: filter(top50_atleastonce == T)  %>%
      dplyr::filter(fic == "USA") %>%
      dplyr:: filter(drug_class == "NME_NCE" | drug_class == "METOO")  %>%
      dplyr:: group_by(period) %>%
      dplyr:: summarise(total_nd1 = sum(n_drug, na.rm = T)) %>%
      mutate(n.year = c(10,10,10,9), avg_nd1_drug = total_nd1 / n.year), 
    by = c("period")
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr::  filter(top50_atleastonce == T)  %>%
      dplyr::filter(fic == "USA") %>%
      dplyr::  group_by(gvkey, conm, fyear, period)  %>%
      dplyr::  summarise(n_patent = min(n_patent)) %>%
      dplyr::  group_by(period) %>%
      dplyr::summarise(total_patent = sum(n_patent, na.rm = T)) %>%
      mutate(n.year = c(10,10,10,9), avg_patent = total_patent / n.year),
    by = c("period")
  ) %>% 
  left_join(
    all_drug_producing %>%
      group_by(period) %>%
      filter(top50_atleastonce == T)  %>%
      dplyr::filter(fic == "USA") %>%
      dplyr:: filter(drug_class == "NME_NCE")%>%
      dplyr:: summarise(total_nd2 = sum(n_drug, na.rm = T)) %>%
      mutate(n.year = c(10,10,10,9), avg_nd2_drug = total_nd2 / n.year), 
    by = c("period")
  ) %>%
  dplyr:: mutate(
    RD = xrd / (oibdp-txt),
    profit = (oibdp - txt) / ppent,
    n_nd1_perBillionRD = (avg_nd1_drug / (xrd.avg*1000000))*1000000000,
    n_nd2_perBillionRD = (avg_nd2_drug / (xrd.avg*1000000))*1000000000,
    shareholder_payment 	=(prstkc+dv)/(oibdp-txt)) %>% 
  dplyr::  select(
    period, profit, RD, shareholder_payment, 
    avg_patent, avg_nd1_drug, avg_nd2_drug, n_nd2_perBillionRD,n_nd1_perBillionRD, 
  )  


# desc_table_firm_iii <- pharma_crsp %>%
#   dplyr::filter(gvkey == 6266 | gvkey == 25648 | gvkey == 8530 | 
#                   gvkey == 101310 |gvkey == 7257 )  %>%
#   dplyr:: group_by(period, gvkey) %>% 
#   dplyr:: summarise(
#     ib = sum(ib, na.rm = T), 
#     ni = sum(ni, na.rm = T), 
#     at = sum(at, na.rm = T),
#     ppent= sum(ppent, na.rm = T),
#     xrd= sum(xrd, na.rm = T), 
#     xrd.avg  = mean(xrd, na.rm = T),
#     sale= sum(sale, na.rm = T), 
#     dv  = sum(dv, na.rm = T), 
#     ppent = sum(ppent, na.rm = T), 
#     oibdp= sum(oibdp, na.rm = T), 
#     txt= sum(txt, na.rm = T), 
#     prstkc= sum(prstkc, na.rm = T)
#   ) %>% 
#   left_join(
#     all_drug_producing %>%
#       dplyr::filter(gvkey == 8530 | gvkey == 7257 | gvkey == 6266 | 
#                       gvkey == 1078 | gvkey == 6730 )  %>%
#       dplyr:: filter(drug_class == "NME_NCE_METOO")  %>%
#       dplyr:: group_by(period, gvkey) %>%
#       dplyr:: summarise(total_nd1 = sum(n_drug, na.rm = T)) %>%
#       mutate(n.year = ifelse(period == 4, 9, 10), avg_nd1_drug = total_nd1 / n.year),
#     by = c("period", "gvkey")
#   ) %>%
#   left_join(
#     all_drug_producing %>%
#       dplyr::filter(gvkey == 8530 | gvkey == 7257 | gvkey == 6266 | 
#                       gvkey == 1078 | gvkey == 6730 )  %>%
#       dplyr::  group_by(gvkey, conm, fyear, period)  %>%
#       dplyr::  summarise(n_patent = min(n_patent)) %>%
#       dplyr::  group_by(period, gvkey) %>%
#       dplyr::summarise(total_patent = sum(n_patent, na.rm = T)) %>%
#       mutate(n.year = ifelse(period == 4, 9, 10), avg_patent = total_patent / n.year),
#     by = c("period","gvkey")
#   ) %>% 
#   left_join(
#     all_drug_producing %>%
#       dplyr::filter(gvkey == 8530 | gvkey == 7257 | gvkey == 6266 | 
#                       gvkey == 1078 | gvkey == 6730 )  %>%
#       dplyr::  group_by(period, gvkey) %>%
#       dplyr:: filter(drug_class == "NME_NCE")%>%
#       dplyr:: summarise(total_nd2 = sum(n_drug, na.rm = T)) %>%
#       mutate(n.year = ifelse(period == 4, 9, 10), avg_nd2_drug = total_nd2 / n.year),
#     by = c("period","gvkey")
#   ) %>%
#   dplyr:: mutate(
#     RD = xrd / (oibdp-txt),
#     profit = (oibdp - txt) / ppent,
#     n_nd1_perBillionRD = (avg_nd1_drug / (xrd.avg*1000000))*1000000000,
#     n_nd2_perBillionRD = (avg_nd2_drug / (xrd.avg*1000000))*1000000000,
#     shareholder_payment 	=(prstkc+dv)/(oibdp-txt)) %>% 
#   dplyr::  select(
#     gvkey, period, profit, RD, shareholder_payment, 
#     avg_patent, avg_nd1_drug, avg_nd2_drug, n_nd2_perBillionRD,n_nd1_perBillionRD, 
#   )  %>% arrange(period, gvkey)

desc_table <- rbind(desc_table_firm_i, desc_table_firm_ii) 

write_excel_csv(
desc_table %>% 
  mutate_if(is.numeric, round, digits = 2), file = "output_tables/desc_table.csv"
)

# 
# # ## ======================== Visualize ===================#
# 
# #Period 1 profitability 
# period_1<- filter(desc_table_firm, period2 == 1  )
# period_1$profitability_z <- round((period_1$profitability - mean(period_1$profitability))/sd(period_1$profitability), 2)  # compute normalized roa
# 
# period_1$profitability_type <- ifelse(period_1$profitability_z < 0, "below", "above")  # above / below avg flag
# period_1 <- period_1[order(period_1$profitability_z), ]  # sort
# period_1$conm <- factor(period_1$conm, levels = period_1$conm)  # convert to factor to retain sorted order in plot.
# 
# 
# period_1$roa_z <- round((period_1$roa - mean(period_1$roa))/sd(period_1$roa), 2)  # compute normalized roa
# 
# period_1$roa_type <- ifelse(period_1$roa_z < 0, "below", "above")  # above / below avg flag
# period_1 <- period_1[order(period_1$roa_z), ]  # sort
# period_1$conm <- factor(period_1$conm, levels = period_1$conm)  # convert to factor to retain sorted order in plot.
# 
# period_1$rd_z <- round((period_1$RDtoCV - mean(period_1$RDtoCV))/sd(period_1$RDtoCV), 2)  # compute normalized roa
# period_1$rd_type <- ifelse(period_1$rd_z < 0, "below", "above")  # above / below avg flag
# period_1 <- period_1[order(period_1$rd_z), ]  # sort
# period_1$conm <- factor(period_1$conm, levels = period_1$conm)  # convert to factor to retain sorted order in plot.
# 
# 
# 
# # Diverging Barcharts
# period_1rd <- ggplot(period_1, aes(x=conm, y=rd_z, label=rd_z)) + 
#   geom_bar(stat='identity', aes(fill=rd_type), width=.5)  +
#   scale_fill_manual(name="RD", 
#                     labels = c("Above Average", "Below Average"), 
#                     values = c("above"="#00ba38", "below"="#f8766d")) + 
#   labs(title="Normalised average RD to cash flow during the period of 1980-1985", 
#        subtitle= "Top drug producing firms existing during the entire sample period with at least one NDA during the entire sample period") + 
#   coord_flip() 
# 
# pdf("output_figures/period_1rd.pdf", width = 12.17, height = 7.92)
# print(period_1rd)
# dev.off()
# 
# 
# #Period 2 nda rd
# period_2<- dplyr::filter(desc_table_firm, period2 == 2)
# period_2[is.na(period_2)] <- 0
# period_2$rd_z <- round((period_2$RDtoCV - mean(period_2$RDtoCV))/sd(period_2$RDtoCV), 2)  # compute normalized roa
# period_2$conm <- factor(period_2$conm, levels = period_2$conm)  # convert to factor to retain sorted order in plot.
# 
# 
# period_2$rd_type <- ifelse(period_2$rd_z < 0, "below", "above")  # above / below avg flag
# period_2 <- period_2[order(period_2$rd_z), ]  # sort
# period_2$conm <- factor(period_2$conm, levels = period_2$conm)  # convert to factor to retain sorted order in plot.
# 
# 
# period_2$nda_z <- round((period_2$total_nda_drug - mean(period_2$total_nda_drug))/sd(period_2$total_nda_drug), 2)  # compute normalized roa
# 
# period_2$nda_type <- ifelse(period_2$nda_z < 0, "below", "above")  # above / below avg flag
# period_2 <- period_2[order(period_2$nda_z), ]  # sort
# period_2$conm <- factor(period_2$conm, levels = period_2$conm)  # convert to factor to retain sorted order in plot.
# 
# 
# period_2$sh_z <- round((period_2$shareholderpayoutToCV - mean(period_2$shareholderpayoutToCV))/sd(period_2$shareholderpayoutToCV), 2)  # compute normalized roa
# period_2$sh_type <- ifelse(period_2$sh_z < 0, "below", "above")  # above / below avg flag
# period_2 <- period_2[order(period_2$sh_z), ]  # sort
# period_2$conm <- factor(period_2$conm, levels = period_2$conm)  # convert to factor to retain sorted order in plot.
# 
# 
# # Diverging Barcharts
# period_2cf <- ggplot(period_2, aes(x=conm, y=sh_z, label=sh_z)) + 
#   geom_bar(stat='identity', aes(fill=sh_type), width=.5)  +
#   scale_fill_manual(name="Shareholder to CF", 
#                     labels = c("Above Average", "Below Average"), 
#                     values = c("above"="#00ba38", "below"="#f8766d")) + 
#   labs(title="Normalised shareholder payouts to cashflows during the period of 1986-1991", 
#        subtitle= "Top drug producing firms existing during the entire sample period with at least one NDA during the entire sample period") + 
#   coord_flip() 
# 
# pdf("output_figures/period_2cf.pdf", width = 12.17, height = 7.92)
# print(period_2cf)
# dev.off()
# 
# # Diverging Barcharts
# period_2nda <- ggplot(period_2, aes(x=conm, y=nda_z, label=nda_z)) + 
#   geom_bar(stat='identity', aes(fill=nda_type), width=.5)  +
#   scale_fill_manual(name="Total NDA", 
#                     labels = c("Above Average", "Below Average"), 
#                     values = c("above"="#00ba38", "below"="#f8766d")) + 
#   labs(title="Normalised total NDA production during the period of 1986-1991", 
#        subtitle= "Top drug producing firms existing during the entire sample period with at least one NDA during the entire sample period") + 
#   coord_flip() 
# 
# pdf("output_figures/period_2nda.pdf", width = 12.17, height = 7.92)
# print(period_2nda)
# dev.off()
# 
# period_2rd <- ggplot(period_2, aes(x=conm, y=rd_z, label=rd_z)) + 
#   geom_bar(stat='identity', aes(fill=rd_type), width=.5)  +
#   scale_fill_manual(name="Total NDA", 
#                     labels = c("Above Average", "Below Average"), 
#                     values = c("above"="#00ba38", "below"="#f8766d")) + 
#   labs(title="Normalised RD to cash flows during the period of 1986-1991", 
#        subtitle= "Top drug producing firms existing during the entire sample period with at least one NDA during the entire sample period") + 
#   coord_flip() 
# 
# pdf("output_figures/period_2rdb.pdf", width = 12.17, height = 7.92)
# print(period_2rd)
# dev.off()
# 
# # ## ======================== Visualize ===================#
# 
# desc_agg_all <- ggplot(gather(desc_table_firm, key = variable, value = value,
#               profitability, RDtoSale, RDtoCV, 
#               shareholderpayoutToCV, total_nda_drug, 
#               NDA1, NDA2, NDA3, BLA1, total_patent, 
#               n_nda_perBillionRD, n_nda1_perBillionRD, n_nda2_perBillionRD, n_nda3_perBillionRD,sale) 
#        ) + 
#   geom_col(aes(x = period, y = value)) +
#   facet_wrap(~variable,scale = "free_y")+
#   labs(y = "Period averages and sums", x = "Periods", title = "All drug producing TOP firms in the Compustat") +
#   enes_theme
# 
# pdf("output_figures/desc_agg_all_TOP.pdf", width = 12.17, height = 7.92)
# print(desc_agg_all)
# dev.off()
# 
# # ## ============ o	Number of drugs per 1 billion USD (R&Dt-5)===================================#
# perbillionRDT_5drug <- pharma_crsp  %>% 
#   filter(top50_atleastonce == T)  %>%
#   # filter(fic == "USA")  %>%
#   group_by(fyear)  %>% 
#   summarise(xrd = sum(xrd, na.rm = T)) %>% 
#   mutate(fyear = fyear + 5 ) %>% 
#   rename(xrd_t_5 = xrd)%>%
#   left_join(
#     all_drug_producing %>%
#       filter(top50_atleastonce == T)  %>%
#       filter(ApplType == "NDA" | ApplType == "BLA" )  %>%
#       group_by(fyear,gvkey) %>%
#       summarise(total_nda_drug = sum(n_drug, na.rm = T)),
#     by = "fyear"
#     
#   )%>%
#   left_join(
#     all_drug_producing %>%
#       filter(top50_atleastonce == T)  %>%
#       filter(ApplType == "NDA" | ApplType == "BLA" )  %>%
#       group_by(fyear, agg_SubmissionClassCode) %>%
#       summarise(total_nda_drug = sum(n_drug, na.rm = T))%>%
#       spread(agg_SubmissionClassCode, value = total_nda_drug),
#     by = c("fyear")
#     
#   )%>%
#   mutate(
#     n_nda_perBillionRDt_5 = (total_nda_drug / (xrd_t_5*1000000))*1000000000,
#     n_nda1_perBillionRDt_5 = (NDA1 / (xrd_t_5*1000000))*1000000000,
#     n_nda1bla1_perBillionRDt_5 = (( NDA1 + BLA1 )/ (xrd_t_5*1000000))*1000000000,
#     n_nda2_perBillionRDt_5 = (NDA2 / (xrd_t_5*1000000))*1000000000,
#     n_nda3_perBillionRDt_5 = (NDA3 / (xrd_t_5*1000000))*1000000000
#   )%>%
#   filter(fyear < 2019)
#   
# rd_drug <- ggplot(perbillionRDT_5drug ,aes( x = fyear)) +
#   geom_line(aes(y = n_nda_perBillionRDt_5, color = "Total NDA"))+
#   geom_line(aes(y = n_nda1_perBillionRDt_5, color = "NDA1"))+
#   geom_line(aes(y = n_nda1bla1_perBillionRDt_5, color = "NDA1+BLA1"))+
#   geom_line(aes(y = n_nda2_perBillionRDt_5, color = "NDA2"))+
#   geom_line(aes(y = n_nda3_perBillionRDt_5, color = "NDA3"))+
#   scale_x_continuous(breaks = seq(1985, 2018, 1)) + 
#   labs(y = "Number of drugs per 1 billion USD RD five years ago ", x = "Years", title = "Number of drugs per 1 billion USD lagged RD ", 
#        subtitle = "All drug producing TOP firms")+ 
#   enes_theme
# 
# pdf("output_figures/rd_drug_TOP.pdf", width = 12.17, height = 7.92)
# print(rd_drug)
# dev.off()
# 
# # ## ======================== Visualize ===================#
# # 
# # desc_table_firm$firm_period <- paste(desc_table_firm$conm, "-",desc_table_firm$period)
# # 
# # desc_table_firm.m <- melt(desc_table_firm[1:80,])
# # desc_table_firm.m <- ddply(desc_table_firm.m, .(variable), transform, rescale = rescale(value))
# # 
# # 
# # ggplot(desc_table_firm.m , aes(as.factor(variable), firm_period, fill = rescale))+
# #   geom_tile(colour = "white")+
# #   scale_fill_gradient(low = "white", high = "steelblue")
# # print(p)