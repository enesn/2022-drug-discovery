## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#


# means <- sample_top50 %>% group_by(gvkey) %>% 
#   dplyr::summarise(
#     mean.profit = mean(profitability), 
#     total.nda = sum(total_nd1_drug),
#     mean.rd = mean(RDtoCV, na.rm = T),
#     total.nda1 = sum(total_nd2_drug)
#   ) %>% 
#   filter(
#     mean.profit > 0 & mean.profit < 1
#   ) %>%
#   dplyr::summarise(pi = mean(mean.profit), 
#             nda = mean(total.nda),
#             rd = mean(mean.rd),
#             nda1 = mean(total.nda1)
#             )

## =================================== Scatterplots ===========================================#
## Long term relationship between profitability and RD for top firms
fig3a <- sample_top50 %>% group_by(gvkey, period2) %>% 
 dplyr:: summarise(
    mean.profitability = mean(profitability) #profit_t, where t is 5-year period
    ) %>% 
  left_join(
    sample_top50 %>% group_by(gvkey, period2) %>% 
      dplyr:: summarise(
        mean.rd = mean(RDtoCV),
        total.patent = sum(total_patent, na.rm = T), 
        total.citations = sum(total_citations, na.rm = T)
      ) %>% 
      mutate(period2 = period2-1) #RD_t+1
  ) %>% 
  filter(!is.na(mean.rd)) %>%
  dplyr::rename(mean.rd.next_5yr.period = mean.rd) %>% 
  dplyr::rename(citations.granted.next.period = total.citations) %>% 
  filter(
    mean.profitability > 0 & mean.rd.next_5yr.period > 0 &
      mean.rd.next_5yr.period < 1 & mean.profitability < 1
    ) %>%
  ggplot(
    aes(x = mean.profitability, y = mean.rd.next_5yr.period)
    ) + 
  # geom_vline(xintercept = means$pi, linetype = "dashed", size = 1) + 
  # geom_hline(yintercept = means$rd, linetype = "dashed", size = 1) + 
  geom_point(size = 4) + 
  labs(x = "Avg. profitability in the current 5-year period",
       y = "Avg. R&D exp. in the next 5-year period") +
  enes_theme

pdf("figures-included/fig3a.pdf", width = 12.17, height = 7.92)
print(fig3a)
dev.off()

## =================================== Scatterplots ===========================================#
## Long term relationship between profitability and total NDA for top firms
fig3b <- sample_top50 %>% group_by(gvkey, period) %>% 
  dplyr::summarise(
    mean.profit = mean(profitability)
    ) %>% 
  left_join(
    sample_top50 %>% group_by(gvkey, period) %>% 
      dplyr::summarise(
       total.nd1 = sum(total_nd1_drug)
      ) %>% mutate(period = period - 1) %>% 
      dplyr::rename(total.nd1_next.10yr_period = total.nd1)
  ) %>% filter(!is.na(total.nd1_next.10yr_period)) %>%
  filter(
    mean.profit > 0 & mean.profit < 1
  ) %>%
  ggplot(
    aes(x = mean.profit, y = total.nd1_next.10yr_period)
    ) + 
  geom_point(size = 4) + 
  labs(x = "Avg. profitability in the current decade",  y = "#ND1 in the next decade")+
  # geom_smooth(method = "lm", color = "black")+
  # geom_vline(xintercept = means$pi, linetype = "dashed", size = 1) +
  # geom_hline(yintercept = means$nda, linetype = "dashed", size = 1) +
  enes_theme

pdf("figures-included/fig3b.pdf", width = 12.17, height = 7.92)
print(fig3b)
dev.off()

## =================================== Scatterplots ===========================================#
## Long term relationship between profitability and total NDA for top firms
fig3c <-sample_top50 %>% group_by(gvkey, period) %>% 
  dplyr::summarise(
    mean.profit = mean(profitability)
  ) %>% 
  left_join(
    sample_top50 %>% group_by(gvkey, period) %>% 
      dplyr::summarise(
        total.nd2 = sum(total_nd2_drug)
      ) %>% mutate(period = period - 1) %>% 
      dplyr::rename(total.nd2_next.10yr_period = total.nd2)
  ) %>% filter(!is.na(total.nd2_next.10yr_period)) %>%
  filter(
    mean.profit > 0 & mean.profit < 1
  ) %>%
  ggplot(
    aes(x = mean.profit, y = total.nd2_next.10yr_period)
  ) + 
  geom_point(size = 4) + 
  labs(x = "Avg. profitability in the current decade",  y = "#ND2 in the next decade")+
  # geom_smooth(method = "lm", color = "black")+
  # geom_vline(xintercept = means$pi, linetype = "dashed", size = 1) +
  # geom_hline(yintercept = means$nda, linetype = "dashed", size = 1) +
  enes_theme

pdf("figures-included/fig3c.pdf", width = 12.17, height = 7.92)
print(fig3c)
dev.off()
