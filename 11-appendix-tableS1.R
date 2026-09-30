## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#
library(stargazer)
library(dyn)

## =================================  Supplementary models ===================================#
## ============================  Models with traditional variables ===============================#
# One concern with the benchmark results is that 
# they may be vulnerable to the use of different definitions of profitability and R&D expenditures. 
# Motivated by this, we use the commonly adopted measures of profitability such as the return on assets 
# and the return on equity as key variable of interest. And we include R&D expenditures as a share of sales rather than cash flow. 

############## RD ####################

rd_benchmark_t_1_TRAD <- 
  dyn$lm(
    (RDtoSale)~
      lag(roa, 1) +
      lag(tob_q, 1)+
      lag(liquidity, 1)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

rd_benchmark_t_1_TRAD_ROE <- 
  dyn$lm(
    (RDtoSale)~
      lag(roe, 1) +
      lag(tob_q, 1)+
      lag(liquidity, 1)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

############## NDA ####################

nda_benchmark_TRAD <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma5_roa, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

nda_benchmark_TRAD_ROE <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma5_roe, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

############## NME (NDA1) ####################

nda1_benchmark_TRAD <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma5_roa, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

nda1_benchmark_TRAD_ROE <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma5_roe, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

## =========================================== Tables ==========================================#

#supplementary specifications
# stargazer(rd_benchmark_t_1_TRAD,
#           rd_benchmark_t_1_TRAD_ROE,
#           nda_benchmark_TRAD,
#           nda_benchmark_TRAD_ROE,
#           nda1_benchmark_TRAD,
#           nda1_benchmark_TRAD_ROE,
#           float.env = "sidewaystable")

