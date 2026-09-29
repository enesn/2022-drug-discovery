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


means <- sample_top50 %>% group_by(gvkey) %>% 
  mutate(total_nda_drug = total_nd1_drug + total_nd2_drug) %>%
  dplyr::summarise(
    mean.profit = mean(profitability), 
    total.nda = sum(total_nda_drug),
    mean.rd = mean(RDtoCV, na.rm = T),
    total.nda1 = sum(total_nd1_drug)
  ) %>% 
  filter(
    mean.profit > 0 & mean.profit < 1
  ) %>%
  dplyr::summarise(pi = mean(mean.profit), 
            nda = mean(total.nda),
            rd = mean(mean.rd),
            nda1 = mean(total.nda1))

## =================================== Scatterplots ===========================================#

sample_top50 %>% group_by(gvkey, period2) %>% 
  dplyr::summarise(
    mean.profit = mean(profitability)
  )%>%
  arrange(gvkey)%>%
mutate(
  period2  = period2+1
)%>%
  left_join(
    sample_top50 %>% group_by(gvkey, period2) %>% 
      dplyr::summarise(
        mean.rd = mean(RDtoCV)
      ),        
    by = c("period2","gvkey")
  ) %>% 
  # mutate(
  #   period2  = period2-1
  # ) %>%
  dplyr::rename(mean.profit.t_1 = mean.profit) %>% 
  filter(!is.na(mean.rd)) %>% 
  filter(mean.profit.t_1 < 2 & mean.profit.t_1 > 0   & mean.rd < 1 & mean.rd > 0) %>% 
   ggplot(
    aes(x = mean.profit.t_1, y = mean.rd)
  ) + 
  geom_point(size = 4) + 
  labs(
    x = "Average profitability in the past 5 years",
    y = "Mean R&D in the current 5-year period"
  )+ 

  enes_theme

## =================================== Scatterplots ===========================================#

sample_top50 %>% group_by(gvkey, period) %>% 

  dplyr::summarise(
    mean.profit = mean(profitability)
  )%>%
  arrange(gvkey)%>%
  mutate(
    period  = period+1
  )%>%
  left_join(
    sample_top50 %>% group_by(gvkey, period) %>% 
          mutate(total_nda_drug = total_nd1_drug + total_nd2_drug) %>%
      dplyr::summarise(
        total.nda = sum(total_nda_drug)
      ),        
    by = c("period","gvkey")
  ) %>% 
  # mutate(
  #   period2  = period2-1
  # ) %>%
  dplyr::rename(mean.profit.t_1 = mean.profit) %>% 
  filter(mean.profit.t_1 < 3 & mean.profit.t_1 > -1 & total.nda > 0) %>% 
  ggplot(
    aes(x = mean.profit.t_1, y = total.nda)
  ) + 
  geom_point(size = 4) + 
  labs(x = "Average profitability in the past decade", 
         y = "Total NDA in the current decade")+ 
  enes_theme


## =================================== Scatterplots ===========================================#


sample_top50 %>% group_by(gvkey, period) %>% 
  dplyr::summarise(
    total.patent = mean(total_patent)
  )%>%
  arrange(gvkey)%>%
  mutate(
    period  = period+1
  )%>%
  left_join(
    sample_top50 %>% group_by(gvkey, period) %>% 
          mutate(total_nda_drug = total_nd1_drug + total_nd2_drug) %>%
      dplyr::summarise(
        total.nda = sum(total_nda_drug)
      ),        
    by = c("period","gvkey")
  ) %>% 
  # mutate(
  #   period2  = period2-1
  # ) %>%
  dplyr::rename(total.patent.t_1 = total.patent) %>% 
  filter(!is.na(total.nda) & total.patent.t_1 > 20) %>%
  ggplot(
    aes(x = total.patent.t_1, y = total.nda)
  ) + 
  geom_smooth(method = "lm", color = "black", se = F)+ 
  geom_point(size = 4) + 
  labs(x = "Total patent in the past decade", 
       y = "Total NDA in the current decade")+ 
  enes_theme


## =================================== Scatterplots ===========================================#

sample_top50 %>% group_by(gvkey, period) %>% 
  dplyr::summarise(
    total.patent = mean(total_patent)
  )%>%
  arrange(gvkey)%>%
  mutate(
    period  = period+1
  )%>%
  left_join(
    sample_top50 %>% group_by(gvkey, period) %>% 
      dplyr::summarise(
        total.nda = sum(total_nd1_drug)
      ),        
    by = c("period","gvkey")
  ) %>% 
  # mutate(
  #   period2  = period2-1
  # ) %>%
  dplyr::rename(total.patent.t_1 = total.patent) %>% 
  filter(!is.na(total.nda) & total.patent.t_1 > 20) %>%
  ggplot(
    aes(x = total.patent.t_1, y = total.nda)
  ) + 
  geom_point(size = 4) + 
  geom_smooth(method = "lm", color = "black", se = F)+ 
  labs(x = "Total patent in the past decade", 
       y = "Total NME in the current decade")+ 
  enes_theme
