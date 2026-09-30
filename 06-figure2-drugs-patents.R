## ===========================================================================================#
# Pharmaceutical industry:            
# SESSION INFO
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.4 LTS
# JULY 2020
# EI
## ===========================================================================================#

fig2b  <- ggplot(desc_table_firm, aes( x = fyear)) +
  geom_line(aes(y = total_nd2_drug, linetype = "Total ND2 drugs"), size = 1.5)+
  geom_line(aes(y = total_nd1_drug, linetype = "Total ND1 drugs"), size = 1.5)+
  scale_x_continuous(breaks = seq(1980, 2018, 2)) +
  labs( x = "", y = "")+ 
  geom_smooth(aes(y = total_nd1_drug), color = "black", se = F)+
  geom_smooth(aes(y = total_nd2_drug, color = "black"),se = F)+
  enes_theme


pdf("figures-included/fig2b.pdf", width = 12.17, height = 7.92)
print(fig2b)
dev.off()

fig2a <- ggplot(desc_table_firm, aes( x = fyear)) +
  geom_line(aes(y = n_patent, linetype = "Patent flow"), size = 1.5)+
  geom_line(aes(y = cumulative_patent/23, linetype = "Patent stock"), size = 1.5)+
  scale_x_continuous(breaks = seq(1980, 2018, 2)) + 
  scale_y_continuous(

    # Features of the first axis
    name = "Flow",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*23, name ="Stock")
  )+
  labs(x = "",y = "")+ 
  enes_theme


pdf("figures-included/fig2a.pdf", width = 12.17, height = 7.92)
print(fig2a)
dev.off()

fig2d <- ggplot(desc_table_firm, aes( x = fyear)) +
  geom_line(aes(y = n_nd1_perBillionRD, linetype = "ND1 per billion USD of R&D"), size = 1.5) + 
  geom_line(aes(y = n_nd2_perBillionRD, linetype = "ND2 per billion USD of R&D"), size = 1.5) + 
  # geom_line(aes(y = avg_nda_perBillionRD*45, color = " Average drugs per billion USD"), size = 1.5)+
  scale_x_continuous(breaks = seq(1980, 2018, 2)) + 
  # scale_y_continuous(
  #   
  #   # Features of the first axis
  #   name = "#Drugs per billion USD (flow)",
  #   
  #   # Add a second axis and specify its features
  #   sec.axis = sec_axis(~./45, name ="#Drugs per billion USD /#Firms")
  # )+ 
  labs(x = "",y = "")+ 
  enes_theme

pdf("figures-included/fig2d.pdf", width = 12.17, height = 7.92)
print(fig2d)
dev.off()

fig2c <- ggplot(desc_table_firm, aes( x = fyear)) +
  geom_line(aes(y = nd1_per_cumpatent, linetype = "ND1 per patents (cumulative)"), size = 1.5)+
  geom_line(aes(y = nd2_per_cumpatent, linetype = "ND2 drugs per patents (cumulative)"), size = 1.5)+
  scale_x_continuous(breaks = seq(1980, 2018, 2)) +
  # scale_y_continuous(
  #   
  #   # Features of the first axis
  #   name = "#Cumulative new drug approvals / # Cumulative patents",
  #   
  #   # Add a second axis and specify its features
  #   sec.axis = sec_axis(~./55, name ="#Cumulative new drug approvals / # Cumulative patents /  #Firms")
  # )+ 
  labs(x = "",y = "")+ 
  enes_theme


pdf("figures-included/fig2c.pdf", width = 12.17, height = 7.92)
print(fig2c)
dev.off()
  
