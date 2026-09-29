## ===========================================================================================#
# Pharmaceutical industry:  
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#

## ================================      NFC vs. Pharma   ====================================# 

#Entire Compustat sample
compustat <- read_dta("input_data/compustat_2_11_20.dta")

#NFC - Finance - Pharma classification
compustat$overall_class <- ifelse(
  compustat$sic == 2834 | compustat$sic == 2835 | compustat$sic == 2836, "pharma", 
  ifelse(
    compustat$sic > 4899  & compustat$sic <  4999, "utilities",
  ifelse(
    compustat$sic < 6000 | compustat$sic > 6800, "NFC", "finance"
   )
  )
)

fin_nonpharma_nfc <- compustat %>% 
  dplyr::filter(fyear > 1979) %>% 
  dplyr::filter(!overall_class == "finance") %>% 
  dplyr::filter(!overall_class == "pharma") %>% 
  dplyr::filter(!overall_class == "utilities") %>% 
  dplyr::group_by(fyear) %>%  # Non-pharma NFC vs Pharma
  dplyr::summarise(ib = sum(ib, na.rm = T), #year totals
            ppent = sum(ppent, na.rm = T),
            sale = sum(sale, na.rm = T),
            cogs = sum(cogs, na.rm = T),
            dp = sum(dp, na.rm = T),
            prstkc = sum(prstkc, na.rm = T),
            dv = sum(dv, na.rm = T),
            che = sum(che, na.rm = T),
            recco = sum(recco, na.rm = T),
            ivao = sum(ivao, na.rm = T),
            xrd = sum(xrd, na.rm = T),
            capx = sum(capx, na.rm = T),
            oibdp = sum(oibdp, na.rm = T),
            txt = sum(txt, na.rm = T),
            dltis = sum(dltis, na.rm = T),
            dltr = sum(dltr, na.rm = T),
            txt = sum(txt, na.rm = T),
            seq = sum(seq, na.rm = T),
            dlc = sum(dlc, na.rm = T),
            dltt = sum(dltt, na.rm = T),
            oibdp = sum(oibdp, na.rm = T),
            ni = sum(ni, na.rm = T),
            che = sum(che, na.rm = T),
            rect = sum(rect, na.rm = T),
            aco = sum(aco, na.rm = T),
            ao = sum(ao, na.rm = T),
            mkvalt = sum(mkvalt, na.rm = T),
            at = sum(at, na.rm = T),
            ugi = sum(ugi, na.rm = T),
            prcc_f =  sum(prcc_f, na.rm = T),
            csho = sum(csho, na.rm = T),
            xad = sum(xad, na.rm = T),
            intan = sum(intan, na.rm = T),
            gdwl = sum(gdwl, na.rm = T),
            ivaeq = sum(ivaeq, na.rm = T))  %>%
  mutate(profit = (oibdp - txt) / ppent,
         # ROA = ni / at,
         # Capitalized_ROA = (ni - xrd) / at,
         # roe = ni / (csho * prcc_f),
         # profit_margin = ugi / sale, 
         # total_debt = dlc + dltt, 
         # debt_to_ppent = total_debt / ppent,
         # cashflow_to_debt = (oibdp - txt) / total_debt, 
         # debt_to_asset = total_debt / at, 
         # eps = ni / csho,
         markup = ((sale - cogs)/cogs),
         # markup2 =(sale - cogs - dp)/(cogs + dp),
         # shareholder_dist1 =  (prstkc)/(oibdp-txt),
         # shareholder_dist2 	=(dv)/(oibdp-txt),
         shareholder_dist 	=(prstkc+dv)/(oibdp-txt),
         # shareholder_payments=  (prstkc + dv) / (seq),
         # financial_assets = (che + recco + ivao + ivaeq) / sale,
         # cash_and_shortterm  = che / sale, 
         # intangibles = intan / ppent,
         # goodwill = gdwl/ppent,
         # intangibles_without_gdwl = (intan - gdwl) / (ppent),
         # current_receivables = rect / sale, 
         RD = xrd / sale,
         RD2 = xrd / (oibdp-txt)
         # RDtoAFUND = xrd / ((oibdp -txt) + (dltis - dltr)),
         # INVtoCFLOW = (capx + xrd) / (oibdp - txt),
         # investment = (capx) / ppent, 
         # INVtoAFUND =  (capx + xrd) / ((oibdp -txt) + (dltis - dltr))
         ) %>% mutate(class = "nonpharma_nfc")

fin_pharma <- pharma_crsp %>% 
  dplyr::filter(drug_producer == T) %>% 
  dplyr::filter(top50_atleastonce == T) %>%  
  dplyr:: group_by(fyear) %>%  # Non-pharma NFC vs Pharma
  dplyr::summarise(ib = sum(ib, na.rm = T), #year totals
            ppent = sum(ppent, na.rm = T),
            sale = sum(sale, na.rm = T),
            cogs = sum(cogs, na.rm = T),
            dp = sum(dp, na.rm = T),
            prstkc = sum(prstkc, na.rm = T),
            dv = sum(dv, na.rm = T),
            che = sum(che, na.rm = T),
            recco = sum(recco, na.rm = T),
            ivao = sum(ivao, na.rm = T),
            xrd = sum(xrd, na.rm = T),
            capx = sum(capx, na.rm = T),
            oibdp = sum(oibdp, na.rm = T),
            txt = sum(txt, na.rm = T),
            dltis = sum(dltis, na.rm = T),
            dltr = sum(dltr, na.rm = T),
            txt = sum(txt, na.rm = T),
            seq = sum(seq, na.rm = T),
            dlc = sum(dlc, na.rm = T),
            dltt = sum(dltt, na.rm = T),
            oibdp = sum(oibdp, na.rm = T),
            ni = sum(ni, na.rm = T),
            che = sum(che, na.rm = T),
            rect = sum(rect, na.rm = T),
            aco = sum(aco, na.rm = T),
            ao = sum(ao, na.rm = T),
            mkvalt = sum(mkvalt, na.rm = T),
            at = sum(at, na.rm = T),
            ugi = sum(ugi, na.rm = T),
            prcc_f =  sum(prcc_f, na.rm = T),
            csho = sum(csho, na.rm = T),
            xad = sum(xad, na.rm = T),
            intan = sum(intan, na.rm = T),
            gdwl = sum(gdwl, na.rm = T),
            ivaeq = sum(ivaeq, na.rm = T))  %>%
  dplyr::mutate(profit = (oibdp - txt) / ppent,
         # ROA = ni / at,
         # Capitalized_ROA = (ni - xrd) / at,
         # roe = ni / (csho * prcc_f),
         # profit_margin = ugi / sale, 
         # total_debt = dlc + dltt, 
         # debt_to_ppent = total_debt / ppent,
         # cashflow_to_debt = (oibdp - txt) / total_debt, 
         # debt_to_asset = total_debt / at, 
         # eps = ni / csho,
         markup = ((sale - cogs)/cogs),
         # markup2 =(sale - cogs - dp)/(cogs + dp),
         # shareholder_dist1 =  (prstkc)/(oibdp-txt),
         # shareholder_dist2 	=(dv)/(oibdp-txt),
         shareholder_dist 	=(prstkc+dv)/(oibdp-txt),
         # shareholder_payments=  (prstkc + dv) / (seq),
         # financial_assets = (che + recco + ivao + ivaeq) / sale,
         # cash_and_shortterm  = che / sale, 
         # intangibles = intan / ppent,
         # goodwill = gdwl/ppent,
         # intangibles_without_gdwl = (intan - gdwl) / (ppent),
         # current_receivables = rect / sale, 
         RD = xrd / sale,
         RD2 = xrd / (oibdp-txt)
         # RDtoAFUND = xrd / ((oibdp -txt) + (dltis - dltr)),
         # INVtoCFLOW = (capx + xrd) / (oibdp - txt),
         # investment = (capx) / ppent, 
         # INVtoAFUND =  (capx + xrd) / ((oibdp -txt) + (dltis - dltr))
  ) %>% dplyr::mutate(class = "pharma")


fin_nfc_pharma <- rbind(fin_nonpharma_nfc, fin_pharma)


nfc_pharma <- ggplot(
  data = fin_nfc_pharma %>% select(fyear, 
                                   profit,
                                   # roe,
                                   # ROA,
                                   class,
                                   markup,
                                   RD,
                                   shareholder_dist
  ) %>% dplyr::rename(
    Profitability = profit, 
    Markup_rate = markup, 
    RandD = RD,
    Shareholder_payments = shareholder_dist
  )%>%
    # filter(!size == 1) %>%
    gather(key = variable, 
           value = value, 
           Profitability,
           # roe,
           # ROA,
           Markup_rate,
           RandD,
           Shareholder_payments), 
  aes(x = fyear, y = value)) + 
  facet_wrap(~variable, scales = "free_y")+
  geom_line(aes(linetype = as.factor(class)),size = 1.5) +
  enes_theme+
  labs(x="",y="", linetype="Sector",
       subtitle ="") +
  # geom_dl(aes(label=overall_class), method="last.points") +
  scale_x_continuous(breaks = seq(1980, 2018, 2)) 

pdf("output_figures/nfc_vs_pharma.pdf", width = 12.17, height = 7.92)
print(nfc_pharma)
dev.off()

