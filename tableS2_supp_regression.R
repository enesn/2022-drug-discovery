## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#

#call samples 
source(file = "empirical_analysis_sample_selection.R")


## =================================  Supplementary models ===================================#
## ============================  Models based on less unbalanced samples  ==========================#

############## RD ####################

rd_benchmark_t_1_b10 <- 
  dyn$lm(
    (RDtoCV)~
      lag(profitability, 1) +
      lag(tob_q, 1)+
      lag(liquidity, 1)+
      factor(gvkey) +
      factor(fyear),
    sample_balanced10
  )

############## NDA ####################

nda_benchmark_b10 <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma5_pi, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_balanced10
  )

############## NME (NDA1) ####################

nda1_benchmark_b10 <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma5_pi, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_balanced10
  )

## ============================  Models based on balanced samples 20 ===============================#

############## RD ####################

rd_benchmark_t_1_b20 <- 
  dyn$lm(
    (RDtoCV)~
      lag(profitability, 1) +
      lag(tob_q, 1)+
      lag(liquidity, 1)+
      factor(gvkey) +
      factor(fyear),
    sample_balanced20
  )

############## NDA ####################

nda_benchmark_b20 <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma5_pi, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_balanced20
  )

############## NME (NDA1) ####################

nda1_benchmark_b20 <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma5_pi, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_balanced20
  )

## =========================================== Tables ==========================================#

#supplementary specifications
# stargazer(rd_benchmark_t_1_b10,
#           rd_benchmark_t_1_b20,
#           nda_benchmark_b10,
#           nda_benchmark_b20,
#           nda1_benchmark_b10,
#           nda1_benchmark_b20,
#           float.env = "sidewaystable")

