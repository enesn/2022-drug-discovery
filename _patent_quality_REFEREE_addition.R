## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#

source("_patentdata_cleaning.R")
source(file = "empirical_analysis_sample_selection.R")

## =================================== Scatterplots ===========================================#
sample_top50 %>% group_by(gvkey, period2) %>% 
  dplyr:: summarise(
    mean.profitability = mean(profitability) #profit_t, where t is 5-year period
  ) %>% 
  left_join(
    sample_top50 %>% group_by(gvkey, period2) %>% 
      dplyr:: summarise(
        mean.rd = mean(RDtoCV),
        total.patent = sum(total_patent, na.rm = T), 
        total.xi = sum(total_xi, na.rm = T),
        total.citations = sum(total_citations, na.rm = T)
      ) %>% 
      mutate(period2 = period2-1,
             avg.cite = total.citations / total.patent,
             avg.xi = total.xi / total.patent) #t+1
  ) %>% 
  filter(
    mean.profitability > 0 & mean.profitability < 1
  ) %>%
  ggplot(
    aes(x = mean.profitability, y = total.patent)
  ) + 
  # geom_vline(xintercept = means$pi, linetype = "dashed", size = 1) + 
  # geom_hline(yintercept = means$rd, linetype = "dashed", size = 1) + 
  geom_point(size = 4) + 
  # geom_smooth()+
  labs(x = "Avg. profitability in the current 5-year period",
       y = "Total patents in the next 5-year period") +
  enes_theme
