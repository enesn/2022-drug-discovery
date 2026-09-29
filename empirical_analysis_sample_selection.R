## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#
library(scales)
library(reshape2)
library(plyr)
library(stargazer)

moving_fun <- function(x, w, FUN, ...) {
  # x: a double vector
  # w: the length of the window, i.e., the section of the vector selected to apply FUN
  # FUN: a function that takes a vector and return a summarize value, e.g., mean, sum, etc.
  # Given a double type vector apply a FUN over a moving window from left to the right, 
  #    when a window boundary is not a legal section, i.e. lower_bound and i (upper bound) 
  #    are not contained in the length of the vector, return a NA_real_
  if (w < 1) {
    stop("The length of the window 'w' must be greater than 0")
  }
  output <- x
  for (i in 1:length(x)) {
    # plus 1 because the index is inclusive with the upper_bound 'i'
    lower_bound <- i - w + 1
    if (lower_bound < 1) {
      output[i] <- NA_real_
    } else {
      output[i] <- FUN(x[lower_bound:i, ...])
    }
  }
  output
}


pharma_crsp$period <- ifelse(pharma_crsp$fyear > 1979 & pharma_crsp$fyear < 1990, 1, 
                             ifelse(pharma_crsp$fyear > 1989 & pharma_crsp$fyear < 2000, 2,
                                    ifelse(pharma_crsp$fyear > 1999 & pharma_crsp$fyear < 2010,3, 4)))

pharma_crsp$period2 <- ifelse(pharma_crsp$fyear > 1979 & pharma_crsp$fyear < 1986, 1, 
                              ifelse(pharma_crsp$fyear > 1985 & pharma_crsp$fyear < 1992, 2,
                                     ifelse(pharma_crsp$fyear > 1991 & pharma_crsp$fyear < 1998,3, 
                                            ifelse(pharma_crsp$fyear > 1997 & pharma_crsp$fyear < 2004,4,
                                                   ifelse(pharma_crsp$fyear > 2003 & pharma_crsp$fyear < 2010,5,6)))))



## ========================================= Balanced Sample 10 ====================================#

sample_balanced10 <- pharma_crsp %>%
  dplyr::filter(drug_producer == T)  %>%
  dplyr::filter(top50_atleastonce == T)  %>%
  dplyr::filter(existing_10years == T) %>%
  # dplyr::filter(emp  > 1)  %>%
  # dplyr::filter(existing_allyears == T)  %>%  # sample 1
  dplyr::filter(!gvkey == 12757)  %>% #inf
  dplyr::filter(!gvkey == 13786)  %>%
  dplyr::filter(!gvkey == 30007)  %>%
  dplyr::filter(!gvkey == 61448)  %>%
  dplyr::filter(!gvkey == 62921)  %>%
  dplyr::filter(!gvkey == 171022)  %>%
  dplyr::filter(!gvkey == 21776)  %>%
  dplyr::filter(!gvkey == 11793)  %>%
  dplyr::filter(!gvkey == 25906)  %>%
  dplyr::filter(!gvkey == 14603)  %>%
  dplyr::filter(!gvkey == 146616)  %>%
  dplyr::filter(!gvkey == 166435)  %>%
  dplyr::filter(!gvkey == 170945)  %>%
  dplyr::filter(!gvkey == 175483)  %>%
  dplyr::filter(!gvkey == 177313)  %>%
  dplyr::filter(!gvkey == 21841)  %>%
  dplyr::filter(!gvkey == 62826)  %>%
  dplyr::filter(!gvkey == 151630)  %>%
  left_join(
    all_drug_producing %>%
      dplyr:: filter(top50_atleastonce == T)  %>%
      dplyr::filter(existing_10years == T) %>%
      dplyr::  filter(drug_class == "NME_NCE" | drug_class == "METOO")  %>%
      dplyr::  group_by(gvkey,conm, fyear) %>%
      dplyr:: summarise(total_nd1_drug = sum(n_drug, na.rm = T)),
    by = c("gvkey","fyear")
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr:: filter(top50_atleastonce == T)  %>%
      dplyr::filter(existing_10years == T) %>%
      dplyr:: filter(ApplType == "NDA" | ApplType == "BLA" )  %>%
      dplyr:: group_by(fyear, gvkey, conm, drug_class) %>%
      dplyr:: summarise(total_drug = sum(n_drug, na.rm = T))%>%
      spread(drug_class, value = total_drug),
    by = c("gvkey","fyear")
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr::  filter(top50_atleastonce == T)  %>%
      dplyr::filter(existing_10years == T) %>%
      dplyr::  group_by(gvkey, conm, fyear)  %>%
      dplyr::  summarise(n_patent = min(n_patent)) %>%
      dplyr::  group_by(fyear, gvkey) %>%
      dplyr::summarise(total_patent = sum(n_patent, na.rm = T)),
    by = c("gvkey","fyear")
  ) %>%
  dplyr:: mutate(profitability = (oibdp - txt) / ppent,
                 liquidity = (dltt + dlc)/ at,
                 roa = ni / at,
                 roe = ni / seq,
                 me = prcc_f * csho, #market value
                 cash = (oibdp-txt) / at,
                 tob_q = (me + lt + pstk)/ at, #tobin's q
                 RDtoSale = xrd / sale,
                 RDtoCV = xrd / (oibdp-txt),
                 shareholderpayoutToCV = (prstkc+dv)/(oibdp-txt)) %>%
  mutate(tob_q = ifelse(is.na(tob_q), 0, tob_q),
         liquidity = ifelse(is.na(liquidity), 0, liquidity)) %>%
  dplyr::  select(
    gvkey, fyear, emp, period, period2, profitability, 
    roa, roe,  tob_q, liquidity, cash, RDtoSale, RDtoCV, shareholderpayoutToCV,
    total_nd1_drug, NME_NCE, METOO, total_patent, sale
  )  %>%  dplyr::arrange(gvkey, fyear)   %>%
  dplyr:: group_by(gvkey)  %>%
  dplyr:: mutate(ma5_pi = moving_fun(profitability, 5 , mean),
                 ma5_roa = moving_fun(roa, 5, mean),
                 ma5_roe = moving_fun(roe, 5, mean),
                 ma5_sales = moving_fun(sale, 5, mean),
                 ma5_cash = moving_fun(cash, 5, mean),
                 ma5_tobq = moving_fun(tob_q, 5, mean),
                 ma5_rd.sale = moving_fun(RDtoSale, 5, mean),
                 ma5_rd.cf = moving_fun(RDtoCV, 5, mean),
                 ma5_liquidity = moving_fun(liquidity, 5, mean),
                 ma5_emp =  moving_fun(emp, 5, mean),
                 ma5_sh.cf = moving_fun(shareholderpayoutToCV, 5, mean),
                 ma3_pi = moving_fun(profitability, 3 , mean),
                 ma3_roa = moving_fun(roa, 3, mean),
                 ma3_roe = moving_fun(roe, 3, mean),
                 ma3_sales = moving_fun(sale, 3, mean),
                 ma3_cash = moving_fun(cash, 3, mean),
                 ma3_tobq = moving_fun(tob_q, 3, mean),
                 ma3_rd.sale = moving_fun(RDtoSale, 3, mean),
                 ma3_rd.cf = moving_fun(RDtoCV, 3, mean),
                 ma3_liquidity = moving_fun(liquidity, 3, mean),
                 ma3_emp =  moving_fun(emp, 3, mean),
                 ma3_sh.cf = moving_fun(shareholderpayoutToCV, 3, mean),
                 ma10_pi = moving_fun(profitability, 10 , mean),
                 ma10_roa = moving_fun(roa, 10, mean),
                 ma10_roe = moving_fun(roe, 10, mean),
                 ma10_sales = moving_fun(sale, 10, mean),
                 ma10_tobq = moving_fun(tob_q, 10, mean),
                 ma10_cash = moving_fun(cash, 10, mean),
                 ma10_rd.sale = moving_fun(RDtoSale, 10, mean),
                 ma10_rd.cf = moving_fun(RDtoCV, 10, mean),
                 ma10_liquidity = moving_fun(liquidity, 10, mean),
                 ma10_emp =  moving_fun(emp, 10, mean),
                 ma10_sh.cf = moving_fun(shareholderpayoutToCV, 10, mean)
                 
  )  %>%  mutate(total_nd1_drug = ifelse(is.na(total_nd1_drug), 0, total_nd1_drug),
                 NME_NCE = ifelse(is.na(NME_NCE), 0, NME_NCE),
                 METOO = ifelse(is.na(METOO), 0, METOO)) %>%
  dplyr::filter(!is.na(ma5_pi)) %>%
  dplyr::arrange(gvkey, fyear)  %>%
  dplyr::group_by(gvkey) %>%
  dplyr:: mutate(
    ma5_pi_diff = ma5_pi - lag(ma5_pi),
    ma5_roa_diff = ma5_roa - lag(ma5_roa),
    ma5_rd.sale_diff = ma5_rd.sale - lag(ma5_rd.sale),
    rd.sale_diff = RDtoSale - lag(RDtoSale),
    ma5_rd.cf_diff = ma5_rd.cf - lag(ma5_rd.cf),
    rd.cf_diff = RDtoCV - lag(RDtoCV),
    ma3_pi_diff = ma3_pi - lag(ma3_pi),
    ma3_roa_diff = ma3_roa - lag(ma3_roa),
    ma3_rd.sale_diff = ma3_rd.sale - lag(ma3_rd.sale),
    ma3_rd.cf_diff = ma3_rd.cf - lag(ma3_rd.cf),
    ma10_pi_diff = ma10_pi - lag(ma10_pi),
    ma10_roa_diff = ma10_roa - lag(ma10_roa),
    ma10_rd.sale_diff = ma10_rd.sale - lag(ma10_rd.sale),
    ma10_rd.cf_diff = ma10_rd.cf - lag(ma10_rd.cf),
    total_nda_diff = total_nd1_drug - lag(total_nd1_drug)
  ) %>%
  dplyr:: group_by(fyear) %>%
  dplyr::  mutate(nda_z = total_nd1_drug/max(total_nd1_drug) ) %>%
  dplyr::  mutate(nda_dummy = ifelse(total_nd1_drug > 0, 1, 0) ) %>%
  dplyr::rename(total_nd2_drug = NME_NCE)

## ========================================= Balanced Sample 20 ====================================#

sample_balanced20 <- pharma_crsp %>%
  dplyr::filter(drug_producer == T)  %>%
  dplyr::filter(top50_atleastonce == T)  %>%
  dplyr::filter(existing_20years == T) %>%
  # dplyr::filter(emp  > 1)  %>%
  # dplyr::filter(existing_allyears == T)  %>%  # sample 1
  dplyr::filter(!gvkey == 12757)  %>% #inf
  dplyr::filter(!gvkey == 13786)  %>%
  dplyr::filter(!gvkey == 30007)  %>%
  dplyr::filter(!gvkey == 61448)  %>%
  dplyr::filter(!gvkey == 62921)  %>%
  dplyr::filter(!gvkey == 171022)  %>%
  dplyr::filter(!gvkey == 21776)  %>%
  dplyr::filter(!gvkey == 11793)  %>%
  dplyr::filter(!gvkey == 25906)  %>%
  dplyr::filter(!gvkey == 14603)  %>%
  dplyr::filter(!gvkey == 146616)  %>%
  dplyr::filter(!gvkey == 166435)  %>%
  dplyr::filter(!gvkey == 170945)  %>%
  dplyr::filter(!gvkey == 175483)  %>%
  dplyr::filter(!gvkey == 177313)  %>%
  dplyr::filter(!gvkey == 21841)  %>%
  dplyr::filter(!gvkey == 62826)  %>%
  dplyr::filter(!gvkey == 151630)  %>%
  left_join(
    all_drug_producing %>%
      dplyr:: filter(top50_atleastonce == T)  %>%
      dplyr::filter(existing_20years == T) %>%
      dplyr::  filter(drug_class == "NME_NCE" | drug_class == "METOO")  %>%
      dplyr::  group_by(gvkey,conm, fyear) %>%
      dplyr:: summarise(total_nd1_drug = sum(n_drug, na.rm = T)),
    by = c("gvkey","fyear")
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr:: filter(top50_atleastonce == T)  %>%
      dplyr::filter(existing_20years == T) %>%
      dplyr:: filter(ApplType == "NDA" | ApplType == "BLA" )  %>%
      dplyr:: group_by(fyear, gvkey, conm, drug_class) %>%
      dplyr:: summarise(total_drug = sum(n_drug, na.rm = T))%>%
      spread(drug_class, value = total_drug),
    by = c("gvkey","fyear")
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr::  filter(top50_atleastonce == T)  %>%
      dplyr::filter(existing_20years == T) %>%
      dplyr::  group_by(gvkey, conm, fyear)  %>%
      dplyr::  summarise(n_patent = min(n_patent)) %>%
      dplyr::  group_by(fyear, gvkey) %>%
      dplyr::summarise(total_patent = sum(n_patent, na.rm = T)),
    by = c("gvkey","fyear")
  ) %>%
  dplyr:: mutate(profitability = (oibdp - txt) / ppent,
                 liquidity = (dltt + dlc)/ at,
                 roa = ni / at,
                 roe = ni / seq,
                 me = prcc_f * csho, #market value
                 cash = (oibdp-txt) / at,
                 tob_q = (me + lt + pstk)/ at, #tobin's q
                 RDtoSale = xrd / sale,
                 RDtoCV = xrd / (oibdp-txt),
                 shareholderpayoutToCV = (prstkc+dv)/(oibdp-txt)) %>%
  mutate(tob_q = ifelse(is.na(tob_q), 0, tob_q),
         liquidity = ifelse(is.na(liquidity), 0, liquidity)) %>%
  dplyr::  select(
    gvkey, fyear, emp, period, period2, profitability, roa, roe,  tob_q, liquidity, cash, RDtoSale, RDtoCV, shareholderpayoutToCV,
    total_nd1_drug, NME_NCE, METOO, total_patent, sale
  )  %>%  dplyr::arrange(gvkey, fyear)   %>%
  dplyr:: group_by(gvkey)  %>%
  dplyr:: mutate(ma5_pi = moving_fun(profitability, 5 , mean),
                 ma5_roa = moving_fun(roa, 5, mean),
                 ma5_roe = moving_fun(roe, 5, mean),
                 ma5_sales = moving_fun(sale, 5, mean),
                 ma5_cash = moving_fun(cash, 5, mean),
                 ma5_tobq = moving_fun(tob_q, 5, mean),
                 ma5_rd.sale = moving_fun(RDtoSale, 5, mean),
                 ma5_rd.cf = moving_fun(RDtoCV, 5, mean),
                 ma5_liquidity = moving_fun(liquidity, 5, mean),
                 ma5_emp =  moving_fun(emp, 5, mean),
                 ma5_sh.cf = moving_fun(shareholderpayoutToCV, 5, mean),
                 ma3_pi = moving_fun(profitability, 3 , mean),
                 ma3_roa = moving_fun(roa, 3, mean),
                 ma3_roe = moving_fun(roe, 3, mean),
                 ma3_sales = moving_fun(sale, 3, mean),
                 ma3_cash = moving_fun(cash, 3, mean),
                 ma3_tobq = moving_fun(tob_q, 3, mean),
                 ma3_rd.sale = moving_fun(RDtoSale, 3, mean),
                 ma3_rd.cf = moving_fun(RDtoCV, 3, mean),
                 ma3_liquidity = moving_fun(liquidity, 3, mean),
                 ma3_emp =  moving_fun(emp, 3, mean),
                 ma3_sh.cf = moving_fun(shareholderpayoutToCV, 3, mean),
                 ma10_pi = moving_fun(profitability, 10 , mean),
                 ma10_roa = moving_fun(roa, 10, mean),
                 ma10_roe = moving_fun(roe, 10, mean),
                 ma10_sales = moving_fun(sale, 10, mean),
                 ma10_tobq = moving_fun(tob_q, 10, mean),
                 ma10_cash = moving_fun(cash, 10, mean),
                 ma10_rd.sale = moving_fun(RDtoSale, 10, mean),
                 ma10_rd.cf = moving_fun(RDtoCV, 10, mean),
                 ma10_liquidity = moving_fun(liquidity, 10, mean),
                 ma10_emp =  moving_fun(emp, 10, mean),
                 ma10_sh.cf = moving_fun(shareholderpayoutToCV, 10, mean)
                 
  )  %>%  mutate(total_nd1_drug = ifelse(is.na(total_nd1_drug), 0, total_nd1_drug),
                 NME_NCE = ifelse(is.na(NME_NCE), 0, NME_NCE),
                 METOO = ifelse(is.na(METOO), 0, METOO)) %>%
  dplyr::filter(!is.na(ma5_pi)) %>%
  dplyr::arrange(gvkey, fyear)  %>%
  dplyr::group_by(gvkey) %>%
  dplyr:: mutate(
    ma5_pi_diff = ma5_pi - lag(ma5_pi),
    ma5_roa_diff = ma5_roa - lag(ma5_roa),
    ma5_rd.sale_diff = ma5_rd.sale - lag(ma5_rd.sale),
    rd.sale_diff = RDtoSale - lag(RDtoSale),
    ma5_rd.cf_diff = ma5_rd.cf - lag(ma5_rd.cf),
    rd.cf_diff = RDtoCV - lag(RDtoCV),
    ma3_pi_diff = ma3_pi - lag(ma3_pi),
    ma3_roa_diff = ma3_roa - lag(ma3_roa),
    ma3_rd.sale_diff = ma3_rd.sale - lag(ma3_rd.sale),
    ma3_rd.cf_diff = ma3_rd.cf - lag(ma3_rd.cf),
    ma10_pi_diff = ma10_pi - lag(ma10_pi),
    ma10_roa_diff = ma10_roa - lag(ma10_roa),
    ma10_rd.sale_diff = ma10_rd.sale - lag(ma10_rd.sale),
    ma10_rd.cf_diff = ma10_rd.cf - lag(ma10_rd.cf),
    total_nda_diff = total_nd1_drug - lag(total_nd1_drug)
  ) %>%
  dplyr:: group_by(fyear) %>%
  dplyr::  mutate(nda_z = total_nd1_drug/max(total_nd1_drug) ) %>%
  dplyr::  mutate(nda_dummy = ifelse(total_nd1_drug > 0, 1, 0) ) %>%
  dplyr::rename(total_nd2_drug = NME_NCE)


## ========================================= Top 50 Sample ====================================#

sample_top50 <- pharma_crsp %>%
  dplyr::filter(drug_producer == T)  %>%
  dplyr::filter(top50_atleastonce == T)  %>%
  # dplyr::filter(emp  > 1)  %>%
  # dplyr::filter(existing_allyears == T)  %>%  # sample 1
  dplyr::filter(!gvkey == 12757)  %>% #inf
  dplyr::filter(!gvkey == 13786)  %>%
  dplyr::filter(!gvkey == 30007)  %>%
  dplyr::filter(!gvkey == 61448)  %>%
  dplyr::filter(!gvkey == 62921)  %>%
  dplyr::filter(!gvkey == 171022)  %>%
  dplyr::filter(!gvkey == 21776)  %>%
  dplyr::filter(!gvkey == 11793)  %>%
  dplyr::filter(!gvkey == 25906)  %>%
  dplyr::filter(!gvkey == 14603)  %>%
  dplyr::filter(!gvkey == 146616)  %>%
  dplyr::filter(!gvkey == 166435)  %>%
  dplyr::filter(!gvkey == 170945)  %>%
  dplyr::filter(!gvkey == 175483)  %>%
  dplyr::filter(!gvkey == 177313)  %>%
  dplyr::filter(!gvkey == 21841)  %>%
  dplyr::filter(!gvkey == 62826)  %>%
  dplyr::filter(!gvkey == 151630)  %>%
  left_join(
    all_drug_producing %>%
      dplyr:: filter(top50_atleastonce == T)  %>%
      dplyr::  filter(drug_class == "NME_NCE" | drug_class == "METOO")  %>%
      dplyr::  group_by(gvkey,conm, fyear) %>%
      dplyr:: summarise(total_nd1_drug = sum(n_drug, na.rm = T)),
    by = c("gvkey","fyear")
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr:: filter(top50_atleastonce == T)  %>%
      dplyr:: filter(ApplType == "NDA" | ApplType == "BLA" )  %>%
      dplyr:: group_by(fyear, gvkey, conm, drug_class) %>%
      dplyr:: summarise(total_drug = sum(n_drug, na.rm = T))%>%
      spread(drug_class, value = total_drug),
    by = c("gvkey","fyear")
  ) %>%
  left_join(
    all_drug_producing %>%
      dplyr::  filter(top50_atleastonce == T)  %>%
      dplyr::  group_by(gvkey, conm, fyear)  %>%
      dplyr::  summarise(n_patent = min(n_patent),
                         n_citations = min(n_citations),
                         total_xi  = min(total_xi)) %>%
      dplyr::  group_by(fyear, gvkey) %>%
      dplyr::summarise(total_patent = sum(n_patent, na.rm = T),
                       total_citations = sum(n_citations, na.rm = T),
                       total_xi = sum(total_xi, na.rm= T)
                       ),
    by = c("gvkey","fyear")
  ) %>%
  dplyr:: mutate(profitability = (oibdp - txt) / ppent,
                 liquidity = (dltt + dlc)/ at,
                 roa = ni / at,
                 roe = ni / seq,
                 me = prcc_f * csho, #market value
                 cash = (oibdp-txt) / at,
                 tob_q = (me + lt + pstk)/ at, #tobin's q
                 RDtoSale = xrd / sale,
                 RDtoCV = xrd / (oibdp-txt),
                 shareholderpayoutToCV = (prstkc+dv)/(oibdp-txt)) %>%
  mutate(tob_q = ifelse(is.na(tob_q), 0, tob_q),
         liquidity = ifelse(is.na(liquidity), 0, liquidity)) %>%
  dplyr::  select(
    gvkey, fyear, emp, period, period2, profitability, roa, roe,  tob_q, liquidity, cash, RDtoSale, RDtoCV, shareholderpayoutToCV,
    total_nd1_drug, NME_NCE, METOO, total_patent, total_citations,total_xi, sale
  )  %>%  dplyr::arrange(gvkey, fyear)   %>%
  dplyr:: group_by(gvkey)  %>%
  dplyr:: mutate(ma5_pi = moving_fun(profitability, 5 , mean),
                 ma5_roa = moving_fun(roa, 5, mean),
                 ma5_roe = moving_fun(roe, 5, mean),
                 ma5_sales = moving_fun(sale, 5, mean),
                 ma5_cash = moving_fun(cash, 5, mean),
                 ma5_tobq = moving_fun(tob_q, 5, mean),
                 ma5_rd.sale = moving_fun(RDtoSale, 5, mean),
                 ma5_rd.cf = moving_fun(RDtoCV, 5, mean),
                 ma5_liquidity = moving_fun(liquidity, 5, mean),
                 ma5_emp =  moving_fun(emp, 5, mean),
                 ma5_sh.cf = moving_fun(shareholderpayoutToCV, 5, mean),
                 ma3_pi = moving_fun(profitability, 3 , mean),
                 ma3_roa = moving_fun(roa, 3, mean),
                 ma3_roe = moving_fun(roe, 3, mean),
                 ma3_sales = moving_fun(sale, 3, mean),
                 ma3_cash = moving_fun(cash, 3, mean),
                 ma3_tobq = moving_fun(tob_q, 3, mean),
                 ma3_rd.sale = moving_fun(RDtoSale, 3, mean),
                 ma3_rd.cf = moving_fun(RDtoCV, 3, mean),
                 ma3_liquidity = moving_fun(liquidity, 3, mean),
                 ma3_emp =  moving_fun(emp, 3, mean),
                 ma3_sh.cf = moving_fun(shareholderpayoutToCV, 3, mean),
                 ma10_pi = moving_fun(profitability, 10 , mean),
                 ma10_roa = moving_fun(roa, 10, mean),
                 ma10_roe = moving_fun(roe, 10, mean),
                 ma10_sales = moving_fun(sale, 10, mean),
                 ma10_tobq = moving_fun(tob_q, 10, mean),
                 ma10_cash = moving_fun(cash, 10, mean),
                 ma10_rd.sale = moving_fun(RDtoSale, 10, mean),
                 ma10_rd.cf = moving_fun(RDtoCV, 10, mean),
                 ma10_liquidity = moving_fun(liquidity, 10, mean),
                 ma10_emp =  moving_fun(emp, 10, mean),
                 ma10_sh.cf = moving_fun(shareholderpayoutToCV, 10, mean)

  )  %>%  mutate(total_nd1_drug = ifelse(is.na(total_nd1_drug), 0, total_nd1_drug),
                 NME_NCE = ifelse(is.na(NME_NCE), 0, NME_NCE),
                 METOO = ifelse(is.na(METOO), 0, METOO)) %>%
  dplyr::filter(!is.na(ma5_pi)) %>%
  dplyr::arrange(gvkey, fyear)  %>%
  dplyr::group_by(gvkey) %>%
  dplyr:: mutate(
    ma5_pi_diff = ma5_pi - lag(ma5_pi),
    ma5_roa_diff = ma5_roa - lag(ma5_roa),
    ma5_rd.sale_diff = ma5_rd.sale - lag(ma5_rd.sale),
    rd.sale_diff = RDtoSale - lag(RDtoSale),
    ma5_rd.cf_diff = ma5_rd.cf - lag(ma5_rd.cf),
    rd.cf_diff = RDtoCV - lag(RDtoCV),
    ma3_pi_diff = ma3_pi - lag(ma3_pi),
    ma3_roa_diff = ma3_roa - lag(ma3_roa),
    ma3_rd.sale_diff = ma3_rd.sale - lag(ma3_rd.sale),
    ma3_rd.cf_diff = ma3_rd.cf - lag(ma3_rd.cf),
    ma10_pi_diff = ma10_pi - lag(ma10_pi),
    ma10_roa_diff = ma10_roa - lag(ma10_roa),
    ma10_rd.sale_diff = ma10_rd.sale - lag(ma10_rd.sale),
    ma10_rd.cf_diff = ma10_rd.cf - lag(ma10_rd.cf),
    total_nda_diff = total_nd1_drug - lag(total_nd1_drug)
  ) %>%
  dplyr:: group_by(fyear) %>%
  dplyr::  mutate(nda_z = total_nd1_drug/max(total_nd1_drug) ) %>%
  dplyr::  mutate(nda_dummy = ifelse(total_nd1_drug > 0, 1, 0) ) %>%
  dplyr::rename(total_nd2_drug = NME_NCE) 

