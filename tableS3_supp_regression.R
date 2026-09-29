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
## ============================  Models based with different j and s  ===============================#

############## NDA ####################

nda_benchmark_ma5t10 <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma5_pi, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

nda_benchmark_ma3t10 <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma3_pi, 10) +
      lag(ma3_tobq, 10)+
      lag(ma3_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

nda_benchmark_ma5t15 <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma5_pi, 15) +
      lag(ma5_tobq, 15)+
      lag(ma5_liquidity, 15)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

nda_benchmark_ma5t20 <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma5_pi, 20) +
      lag(ma5_tobq, 20)+
      lag(ma5_liquidity, 20)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

############## NME (NDA1) ####################


nda1_benchmark_ma5t10 <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma5_pi, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

nda1_benchmark_ma3t10 <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma3_pi, 10) +
      lag(ma3_tobq, 10)+
      lag(ma3_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

nda1_benchmark_ma5t15 <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma5_pi, 15) +
      lag(ma5_tobq, 15)+
      lag(ma5_liquidity, 15)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

nda1_benchmark_ma5t20 <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma5_pi, 20) +
      lag(ma5_tobq, 20)+
      lag(ma5_liquidity, 20)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

## =========================================== Tables ==========================================#
# #supplementary specifications
# stargazer(nda_benchmark_ma3t10, 
#           nda_benchmark_ma5t15,
#           nda_benchmark_ma5t20,
#           nda1_benchmark_ma3t10,
#           nda1_benchmark_ma5t15,
#           nda1_benchmark_ma5t20,
#           float.env = "sidewaystable")

