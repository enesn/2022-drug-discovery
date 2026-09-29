## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#
 
library(dyn)
library(stargazer)
library(sjPlot)
#call samples 
source(file = "empirical_analysis_sample_selection.R")

## ====================================  Benchmark models ===================================#

############## RD ####################

rd_benchmark_t_1 <- 
  dyn$lm(
    (RDtoCV)~
      lag(profitability, 1) +
      lag(tob_q, 1)+
      lag(liquidity, 1)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

############## NDA ####################

nda_benchmark <- 
  dyn$lm(
    (total_nd1_drug)~
      lag(ma5_pi, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

# theme_set(theme_sjplot())

# plot_model(nda_benchmark, terms =c("lag(ma5_pi, 10)","lag(ma5_tobq, 10)","lag(ma5_liquidity, 10)") )
# plot_model(nda_benchmark, vline.color = "black",
#            terms = c("factor(fyear)1985",
#                      "factor(fyear)1987",
#                      "factor(fyear)1988",
#                      "factor(fyear)1989",
#                      "factor(fyear)1990",
#                      "factor(fyear)1991",
#                      "factor(fyear)1992",
#                      "factor(fyear)1993",
#                      "factor(fyear)1994",
#                      "factor(fyear)1995",
#                      "factor(fyear)1996",
#                      "factor(fyear)1997",
#                      "factor(fyear)1998",
#                      "factor(fyear)1999",
#                      "factor(fyear)2000",
#                      "factor(fyear)2001",
#                      "factor(fyear)2002",
#                      "factor(fyear)2003",
#                      "factor(fyear)2004",
#                      "factor(fyear)2005",
#                      "factor(fyear)2006",
#                      "factor(fyear)2007",
#                      "factor(fyear)2008",
#                      "factor(fyear)2009",
#                      "factor(fyear)2010",
#                      "factor(fyear)2011",
#                      "factor(fyear)2012",
#                      "factor(fyear)2013",
#                      "factor(fyear)2014",
#                      "factor(fyear)2015",
#                      "factor(fyear)2016",
#                      "factor(fyear)2017",
#                      "factor(fyear)2018"
#            )) + ylim(-1, 1)

############## NME (total_nd2_drug) ####################

total_nd2_drug_benchmark <- 
  dyn$lm(
    (total_nd2_drug)~
      lag(ma5_pi, 10) +
      lag(ma5_tobq, 10)+
      lag(ma5_liquidity, 10)+
      factor(gvkey) +
      factor(fyear),
    sample_top50
  )

# plot_model(total_nd2_drug_benchmark, vline.color = "black",
#            terms = c("factor(fyear)1985",
#                      "factor(fyear)1987",
#                      "factor(fyear)1988",
#                      "factor(fyear)1989",
#                      "factor(fyear)1990",
#                      "factor(fyear)1991",
#                      "factor(fyear)1992",
#                      "factor(fyear)1993",
#                      "factor(fyear)1994",
#                      "factor(fyear)1995",
#                      "factor(fyear)1996",
#                      "factor(fyear)1997",
#                      "factor(fyear)1998",
#                      "factor(fyear)1999",
#                      "factor(fyear)2000",
#                      "factor(fyear)2001",
#                      "factor(fyear)2002",
#                      "factor(fyear)2003",
#                      "factor(fyear)2004",
#                      "factor(fyear)2005",
#                      "factor(fyear)2006",
#                      "factor(fyear)2007",
#                      "factor(fyear)2008",
#                      "factor(fyear)2009",
#                      "factor(fyear)2010",
#                      "factor(fyear)2011",
#                      "factor(fyear)2012",
#                      "factor(fyear)2013",
#                      "factor(fyear)2014",
#                      "factor(fyear)2015",
#                      "factor(fyear)2016",
#                      "factor(fyear)2017",
#                      "factor(fyear)2018"
#                      )) + ylim(-0.5, 0.5)

## =========================================== Tables ==========================================#
#preferred specification
stargazer(rd_benchmark_t_1,
          nda_benchmark,
          total_nd2_drug_benchmark,
          float.env = "sidewaystable")
