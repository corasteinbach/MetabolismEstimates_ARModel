setwd("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/2_Incrementals")
dofits <- readRDS("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/2_Incrementals/2_MetabolismEstimates_ARModel/gall_ypred.RDS")

ggplot(dofits, aes(x= date, y= oxy_cor))+
  facet_wrap(~site)

site_five <- "5"
dofits %>%
  filter(site == site_five) %>%
  ggplot(aes(x = solar.time)) +
  geom_line(
    aes(y = oxy_cor),
    color = "red1",
    alpha = 0.9,
    linewidth = .9
  ) +
  geom_point(
    aes(y = ypred_50),
    color = "sandybrown",
    alpha = 0.2,
    size = 1
  ) +
  labs(
    title = paste("Observed vs Fitted DO — Site 5"),
    x = "Solar time",
    y = "Dissolved oxygen (mg/L)", 
    caption = "Orange points are predicted DO, red points are observed DO"
  ) +
  theme_minimal(base_size = 13)

site_six <- "6"
dofits %>%
  filter(site == site_six) %>%
  ggplot(aes(x = solar.time)) +
  geom_line(
    aes(y = oxy_cor),
    color = "red1",
    alpha = 0.9,
    linewidth = .9
  ) +
  geom_point(
    aes(y = ypred_50),
    color = "sandybrown",
    alpha = 0.2,
    size = 1
  ) +
  labs(
    title = paste("Observed vs Fitted DO — Site 6"),
    x = "Solar time",
    y = "Dissolved oxygen (mg/L)", 
    caption = "Orange points are predicted DO, red points are observed DO"
  ) +
  theme_minimal(base_size = 13)
