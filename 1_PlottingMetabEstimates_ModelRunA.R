library(ggplot2)
library(dplyr)
library(patchwork)

setwd("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/2_Incrementals")

gall_out <- readRDS("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/2_Incrementals/2_MetabolismEstimates_ARModel/gall_out.RDS")

###ALL MONTHS####################################################################################
library(viridis)

ggplot(gall_out, aes(x = gpp_50, y = er_50, color = date)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(
    slope = -1, intercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ site) +
  scale_color_viridis_c(option = "plasma", direction = -1, # reverses colors
      breaks = seq(
      min(gall_out$date),
      max(gall_out$date),
      by = "1 month"
    ),
    labels = scales::date_format("%b")  # Jan, Feb, Mar, …
  ) +
  labs(
    x = "GPP (g O₂ m⁻² d⁻¹)",
    y = "ER (g O₂ m⁻² d⁻¹)",
    color = "Month"
  ) +
  theme_minimal(base_size = 13)

dofits <- readRDS("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/2_Incrementals/2_MetabolismEstimates_ARModel/gall_ypred.RDS")

ggplot(dofits, aes(x= solar.time, y= oxy_cor))+
         facet_wrap(~site)
####INDIVIDUAL MONTHS####################################################################################
###################SITE 0NE######################################################
#GPP and ER against each other with 1:1 line and colored by month
site_one <- "1"

# Define common x and y limits so graphs have same scale 
x_limits <- c(0, 20)   # Adjust as needed
y_limits <- c(-20, 0)  # Adjust as needed

gall_out %>%
  filter(site == site_one) %>%
ggplot(aes(x = gpp_50, y = er_50, color = date)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(
    slope = -1, intercept = 0, linetype = "dashed", color = "black") +
  scale_color_viridis_c(option = "plasma", direction = -1, # reverses colors
                        breaks = seq(
                          min(gall_out$date),
                          max(gall_out$date),
                          by = "1 month"
                        ),
                        labels = scales::date_format("%b")  # Jan, Feb, Mar, …
  ) +
  theme_minimal(base_family = "Source Sans Pro", base_size = 16) +
  theme( legend.position = "top", legend.direction = "horizontal", legend.text = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"), plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  labs(title = "Site 1 (Teepee Creek)",
    subtitle = "Near Yellowstone National Park boundary", 
    x = "GPP (g O₂ m⁻² d⁻¹)",
    y = "ER (g O₂ m⁻² d⁻¹)",
    color = "Month"
  ) +
   coord_fixed() +  
  xlim(x_limits) + ylim(y_limits)

#GPP and ER estimates against time

month_starts1 <- gall_out %>%
  filter(site == site_one) %>%
  distinct(date) %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  distinct(month) %>%
  pull(month)

gall_out %>%
  filter(site == site_one) %>%
  arrange(date) %>%   
  ggplot(aes(x = date)) +
  
  geom_vline(
    xintercept = month_starts1,
    linetype = "dotted",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = month_starts1 + lubridate::days(15),
    y = 17.5,
    label = lubridate::month(month_starts1, label = TRUE, abbr = FALSE),
    color = "grey30",
    size = 4,
    fontface = "bold",
    family = "source", 
  ) +
  
  # GPP credible interval
  geom_ribbon(
    aes(ymin = gpp_2.5, ymax = gpp_97.5),
    fill = "#23BC47",
    alpha = 0.2
  ) +
  
  # ER credible interval
  geom_ribbon(
    aes(ymin = er_2.5, ymax = er_97.5),
    fill = "#A64B00",
    alpha = 0.2
  ) +
  
  geom_line(aes(y = gpp_50), color = "#23BC47", linewidth = 0.7, alpha = 0.6) +
  geom_line(aes(y = er_50),  color = "#FF7400", linewidth = 0.7, alpha = 0.6) +
  geom_point(
    aes(y = gpp_50),
    color = "#007929",
    alpha = 0.9,
    size = 1
  ) +
  geom_point(
    aes(y = er_50),
    color = "#A64B00",
    alpha = 0.9,
    size = 1
  ) +
  geom_abline(
    slope = 0, intercept = 0, linetype = "solid", color = "black", linewidth= 1) +
  scale_x_date(
    date_labels = "%m-%d",
    breaks = "14 days",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    limits = c(-18, 18),
    breaks = seq(-18, 18, by = 2)
  ) +
  labs(title = "Site 1 (Teepee Creek)",
    subtitle = "Near Yellowstone National Park boundary", 
    x = "Date",
    y = expression(bold("Metabolic Flux (g O"[2]*" m"^-2*" d"^-1*")"))
    ) +
  theme_classic(base_family = "Source Sans Pro", base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.4),
    axis.text.x = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot"
  )


###################SITE TWO######################################################
#GPP and ER against each other with 1:1 line and colored by month
site_two <- "2"

# Define common x and y limits so graphs have same scale 
x_limits <- c(0, 36)   # Adjust as needed
y_limits <- c(-36, 0)  # Adjust as needed

gall_out %>%
  filter(site == site_two) %>%
  ggplot(aes(x = gpp_50, y = er_50, color = date)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(
    slope = -1, intercept = 0, linetype = "dashed", color = "black") +
  scale_color_viridis_c(option = "plasma", direction = -1, # reverses colors
                        breaks = seq(
                          min(gall_out$date),
                          max(gall_out$date),
                          by = "1 month"
                        ),
                        labels = scales::date_format("%b")  # Jan, Feb, Mar, …
  ) +
  theme_minimal(base_family = "Source Sans Pro", base_size = 16) +
  theme( legend.position = "top", legend.direction = "horizontal", legend.text = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"), plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  labs(title = "Site 2 (Taylor Fork)",
       subtitle = "Reach below Taylor Fork tributary", 
       x = "GPP (g O₂ m⁻² d⁻¹)",
       y = "ER (g O₂ m⁻² d⁻¹)",
       color = "Month"
  ) +
  coord_fixed() +  
  xlim(x_limits) + ylim(y_limits)

#GPP and ER estimates against time

month_starts2 <- gall_out %>%
  filter(site == site_two) %>%
  distinct(date) %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  distinct(month) %>%
  pull(month)

gall_out %>%
  filter(site == site_two) %>%
  arrange(date) %>%   
  ggplot(aes(x = date)) +
  
  geom_vline(
    xintercept = month_starts2,
    linetype = "dotted",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = month_starts2 + lubridate::days(15),
    y = 17.5,
    label = lubridate::month(month_starts2, label = TRUE, abbr = FALSE),
    color = "grey30",
    size = 4,
    fontface = "bold",
    family = "source", 
  ) +
  
  # GPP credible interval
  geom_ribbon(
    aes(ymin = gpp_2.5, ymax = gpp_97.5),
    fill = "#23BC47",
    alpha = 0.2
  ) +
  
  # ER credible interval
  geom_ribbon(
    aes(ymin = er_2.5, ymax = er_97.5),
    fill = "#A64B00",
    alpha = 0.2
  ) +
  
  geom_line(aes(y = gpp_50), color = "#23BC47", linewidth = 0.7, alpha = 0.6) +
  geom_line(aes(y = er_50),  color = "#FF7400", linewidth = 0.7, alpha = 0.6) +
  geom_point(
    aes(y = gpp_50),
    color = "#007929",
    alpha = 0.9,
    size = 1
  ) +
  geom_point(
    aes(y = er_50),
    color = "#A64B00",
    alpha = 0.9,
    size = 1
  ) +
  geom_abline(
    slope = 0, intercept = 0, linetype = "solid", color = "black", linewidth= 1) +
  scale_x_date(
    date_labels = "%m-%d",
    breaks = "14 days",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    limits = c(-36, 36),
    breaks = seq(-36, 36, by = 4)
  ) +
  labs(title = "Site 2 (Taylor Fork)",
       subtitle = "Reach below Taylor Fork tributary", 
       x = "Date",
       y = expression(bold("Metabolic Flux (g O"[2]*" m"^-2*" d"^-1*")"))
  ) +
  theme_classic(base_family = "Source Sans Pro", base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.4),
    axis.text.x = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot"
  )

###################SITE THREE########################################################
#GPP and ER against each other with 1:1 line and colored by month
site_three <- "3"

# Define common x and y limits so graphs have same scale 
x_limits <- c(-3, 20)   # Adjust as needed
y_limits <- c(-20, 3)  # Adjust as needed

gall_out %>%
  filter(site == site_three) %>%
  ggplot(aes(x = gpp_50, y = er_50, color = date)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(
    slope = -1, intercept = 0, linetype = "dashed", color = "black") +
  scale_color_viridis_c(option = "plasma", direction = -1, # reverses colors
                        breaks = seq(
                          min(gall_out$date),
                          max(gall_out$date),
                          by = "1 month"
                        ),
                        labels = scales::date_format("%b")  # Jan, Feb, Mar, …
  ) +
  theme_minimal(base_family = "Source Sans Pro", base_size = 16) +
  theme( legend.position = "top", legend.direction = "horizontal", legend.text = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"), plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  labs(title = "Site 3 (Beaver Creek)",
       subtitle = "Meadow section upstream of Big Sky, starts 1mi above Beaver Crk", 
       x = "GPP (g O₂ m⁻² d⁻¹)",
       y = "ER (g O₂ m⁻² d⁻¹)",
       color = "Month"
  ) +
  coord_fixed() +  
  xlim(x_limits) + ylim(y_limits)

#GPP and ER estimates against time

month_starts3 <- gall_out %>%
  filter(site == site_three) %>%
  distinct(date) %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  distinct(month) %>%
  pull(month)

gall_out %>%
  filter(site == site_three) %>%
  arrange(date) %>%   
  ggplot(aes(x = date)) +
  
  geom_vline(
    xintercept = month_starts3,
    linetype = "dotted",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = month_starts3 + lubridate::days(15),
    y = 17.5,
    label = lubridate::month(month_starts3, label = TRUE, abbr = FALSE),
    color = "grey30",
    size = 4,
    fontface = "bold",
    family = "source", 
  ) +
  
  # GPP credible interval
  geom_ribbon(
    aes(ymin = gpp_2.5, ymax = gpp_97.5),
    fill = "#23BC47",
    alpha = 0.2
  ) +
  
  # ER credible interval
  geom_ribbon(
    aes(ymin = er_2.5, ymax = er_97.5),
    fill = "#A64B00",
    alpha = 0.2
  ) +
  
  geom_line(aes(y = gpp_50), color = "#23BC47", linewidth = 0.7, alpha = 0.6) +
  geom_line(aes(y = er_50),  color = "#FF7400", linewidth = 0.7, alpha = 0.6) +
  geom_point(
    aes(y = gpp_50),
    color = "#007929",
    alpha = 0.9,
    size = 1
  ) +
  geom_point(
    aes(y = er_50),
    color = "#A64B00",
    alpha = 0.9,
    size = 1
  ) +
  geom_abline(
    slope = 0, intercept = 0, linetype = "solid", color = "black", linewidth= 1) +
  scale_x_date(
    date_labels = "%m-%d",
    breaks = "14 days",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    limits = c(-20, 20),
    breaks = seq(-20, 20, by = 2)
  ) +
  labs(title = "Site 3 (Beaver Creek)",
       subtitle = "Meadow section upstream of Big Sky, starts 1mi above Beaver Crk", 
       x = "Date",
       y = expression(bold("Metabolic Flux (g O"[2]*" m"^-2*" d"^-1*")"))
  ) +
  theme_classic(base_family = "Source Sans Pro", base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.4),
    axis.text.x = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot"
  )

###################SITE FOUR########################################################
#GPP and ER against each other with 1:1 line and colored by month
site_four <- "4"

# Define common x and y limits so graphs have same scale 
x_limits <- c(-5, 30)   # Adjust as needed
y_limits <- c(-30, 5)  # Adjust as needed

gall_out %>%
  filter(site == site_four) %>%
  ggplot(aes(x = gpp_50, y = er_50, color = date)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(
    slope = -1, intercept = 0, linetype = "dashed", color = "black") +
  scale_color_viridis_c(option = "plasma", direction = -1, # reverses colors
                        breaks = seq(
                          min(gall_out$date),
                          max(gall_out$date),
                          by = "1 month"
                        ),
                        labels = scales::date_format("%b")  # Jan, Feb, Mar, …
  ) +
  theme_minimal(base_family = "Source Sans Pro", base_size = 16) +
  theme( legend.position = "top", legend.direction = "horizontal", legend.text = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"), plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  labs(title = "Site 4 (West Fork Gallatin)",
       subtitle = "Begins downstream of confluence with \n West Fork Gallatin", 
       x = "GPP (g O₂ m⁻² d⁻¹)",
       y = "ER (g O₂ m⁻² d⁻¹)",
       color = "Month") +
coord_fixed() +  
xlim(x_limits) + ylim(y_limits)

#GPP and ER estimates against time

month_starts4 <- gall_out %>%
  filter(site == site_four) %>%
  distinct(date) %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  distinct(month) %>%
  pull(month)

gall_out %>%
  filter(site == site_four) %>%
  arrange(date) %>%   
  ggplot(aes(x = date)) +
  
  geom_vline(
    xintercept = month_starts4,
    linetype = "dotted",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = month_starts4 + lubridate::days(15),
    y = 22,
    label = lubridate::month(month_starts4, label = TRUE, abbr = FALSE),
    color = "grey30",
    size = 4,
    fontface = "bold",
    family = "source", 
  ) +
  
  # GPP credible interval
  geom_ribbon(
    aes(ymin = gpp_2.5, ymax = gpp_97.5),
    fill = "#23BC47",
    alpha = 0.2
  ) +
  
  # ER credible interval
  geom_ribbon(
    aes(ymin = er_2.5, ymax = er_97.5),
    fill = "#A64B00",
    alpha = 0.2
  ) +
  
  geom_line(aes(y = gpp_50), color = "#23BC47", linewidth = 0.7, alpha = 0.6) +
  geom_line(aes(y = er_50),  color = "#FF7400", linewidth = 0.7, alpha = 0.6) +
  geom_point(
    aes(y = gpp_50),
    color = "#007929",
    alpha = 0.9,
    size = 1
  ) +
  geom_point(
    aes(y = er_50),
    color = "#A64B00",
    alpha = 0.9,
    size = 1
  ) +
  geom_abline(
    slope = 0, intercept = 0, linetype = "solid", color = "black", linewidth= 1) +
  scale_x_date(
    date_labels = "%m-%d",
    breaks = "14 days",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    limits = c(-30, 30),
    breaks = seq(-30, 30, by = 4)
  ) +
  labs(title = "Site 4 (West Fork Gallatin)",
       subtitle = "Begins downstream of confluence with \n West Fork Gallatin", 
       x = "Date",
       y = expression(bold("Metabolic Flux (g O"[2]*" m"^-2*" d"^-1*")"))
  ) +
  theme_classic(base_family = "Source Sans Pro", base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.4),
    axis.text.x = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot"
  )

###################SITE FIVE##############################################################
#GPP and ER against each other with 1:1 line and colored by month
site_five <- "5"

# Define common x and y limits so graphs have same scale 
 x_limits <- c(0, 35)   # Adjust as needed
 y_limits <- c(-35, 30)  # Adjust as needed

gall_out %>%
  filter(site == site_five) %>%
  ggplot(aes(x = gpp_50, y = er_50, color = date)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(
    slope = -1, intercept = 0, linetype = "dashed", color = "black") +
  scale_color_viridis_c(option = "plasma", direction = -1, # reverses colors
                        breaks = seq(
                          min(gall_out$date),
                          max(gall_out$date),
                          by = "1 month"
                        ),
                        labels = scales::date_format("%b")  # Jan, Feb, Mar, …
  ) +
  theme_minimal(base_family = "Source Sans Pro", base_size = 16) +
  theme( legend.position = "top", legend.direction = "horizontal", legend.text = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"), plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  labs(title = "Site 5 (Stormcastle Crk)",
       subtitle = "Below all Gallatin Rapids \n starts 1.5mi below Stormcastle Creek", 
       x = "GPP (g O₂ m⁻² d⁻¹)",
       y = "ER (g O₂ m⁻² d⁻¹)",
       color = "Month") +
  coord_fixed() +
  xlim(x_limits) + ylim(y_limits)

#GPP and ER estimates against time

month_starts5 <- gall_out %>%
  filter(site == site_five) %>%
  distinct(date) %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  distinct(month) %>%
  pull(month)

gall_out %>%
  filter(site == site_five) %>%
  arrange(date) %>%   
  ggplot(aes(x = date)) +
  
  geom_vline(
    xintercept = month_starts5,
    linetype = "dotted",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = month_starts5 + lubridate::days(15),
    y = 30,
    label = lubridate::month(month_starts4, label = TRUE, abbr = FALSE),
    color = "grey30",
    size = 4,
    fontface = "bold",
    family = "source", 
  ) +
  
  # GPP credible interval
  geom_ribbon(
    aes(ymin = gpp_2.5, ymax = gpp_97.5),
    fill = "#23BC47",
    alpha = 0.2
  ) +
  
  # ER credible interval
  geom_ribbon(
    aes(ymin = er_2.5, ymax = er_97.5),
    fill = "#A64B00",
    alpha = 0.2
  ) +
  
  geom_line(aes(y = gpp_50), color = "#23BC47", linewidth = 0.7, alpha = 0.6) +
  geom_line(aes(y = er_50),  color = "#FF7400", linewidth = 0.7, alpha = 0.6) +
  geom_point(
    aes(y = gpp_50),
    color = "#007929",
    alpha = 0.9,
    size = 1
  ) +
  geom_point(
    aes(y = er_50),
    color = "#A64B00",
    alpha = 0.9,
    size = 1
  ) +
  geom_abline(
    slope = 0, intercept = 0, linetype = "solid", color = "black", linewidth= 1) +
  scale_x_date(
    date_labels = "%m-%d",
    breaks = "14 days",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    limits = c(-30, 30),
    breaks = seq(-30, 30, by = 4)
  ) +
  labs(title = "Site 5 (Stormcastle Crk)",
       subtitle = "Below all Gallatin Rapids \n starts 1.5mi below Stormcastle Creek",  
       x = "Date",
       y = expression(bold("Metabolic Flux (g O"[2]*" m"^-2*" d"^-1*")"))
  ) +
  theme_classic(base_family = "Source Sans Pro", base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.4),
    axis.text.x = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot"
  )

###################SITE SIX###############
#GPP and ER against each other with 1:1 line and colored by month
site_six <- "6"

# Define common x and y limits so graphs have same scale 
x_limits <- c(0, 35)   # Adjust as needed
y_limits <- c(-35, 30)  # Adjust as needed

gall_out %>%
  filter(site == site_six) %>%
  ggplot(aes(x = gpp_50, y = er_50, color = date)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(
    slope = -1, intercept = 0, linetype = "dashed", color = "black") +
  scale_color_viridis_c(option = "plasma", direction = -1, # reverses colors
                        breaks = seq(
                          min(gall_out$date),
                          max(gall_out$date),
                          by = "1 month"
                        ),
                        labels = scales::date_format("%b")  # Jan, Feb, Mar, …
  ) +
  theme_minimal(base_family = "Source Sans Pro", base_size = 16) +
  theme( legend.position = "top", legend.direction = "horizontal", legend.text = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"), plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold")) +
  labs(title = "Site 6 (Valley Site)",
       subtitle = "Head of Gallatin Valley \n starts .5mi below mouth of canyon", 
       x = "GPP (g O₂ m⁻² d⁻¹)",
       y = "ER (g O₂ m⁻² d⁻¹)",
       color = "Month") +
  coord_fixed() +
  xlim(x_limits) + ylim(y_limits)

#GPP and ER estimates against time

month_starts6 <- gall_out %>%
  filter(site == site_six) %>%
  distinct(date) %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  distinct(month) %>%
  pull(month)

gall_out %>%
  filter(site == site_six) %>%
  arrange(date) %>%   
  ggplot(aes(x = date)) +
  
  geom_vline(
    xintercept = month_starts6,
    linetype = "dotted",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  annotate(
    "text",
    x = month_starts6 + lubridate::days(15),
    y = 22,
    label = lubridate::month(month_starts4, label = TRUE, abbr = FALSE),
    color = "grey30",
    size = 4,
    fontface = "bold",
    family = "source", 
  ) +
  
  # GPP credible interval
  geom_ribbon(
    aes(ymin = gpp_2.5, ymax = gpp_97.5),
    fill = "#23BC47",
    alpha = 0.2
  ) +
  
  # ER credible interval
  geom_ribbon(
    aes(ymin = er_2.5, ymax = er_97.5),
    fill = "#A64B00",
    alpha = 0.2
  ) +
  
  geom_line(aes(y = gpp_50), color = "#23BC47", linewidth = 0.7, alpha = 0.6) +
  geom_line(aes(y = er_50),  color = "#FF7400", linewidth = 0.7, alpha = 0.6) +
  geom_point(
    aes(y = gpp_50),
    color = "#007929",
    alpha = 0.9,
    size = 1
  ) +
  geom_point(
    aes(y = er_50),
    color = "#A64B00",
    alpha = 0.9,
    size = 1
  ) +
  geom_abline(
    slope = 0, intercept = 0, linetype = "solid", color = "black", linewidth= 1) +
  scale_x_date(
    date_labels = "%m-%d",
    breaks = "14 days",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    limits = c(-30, 30),
    breaks = seq(-30, 30, by = 4)
  ) +
  labs(title = "Site 6 (Valley Site)",
       subtitle = "Head of Gallatin Valley \n starts .5mi below mouth of canyon",
       x = "Date",
       y = expression(bold("Metabolic Flux (g O"[2]*" m"^-2*" d"^-1*")"))
  ) +
  theme_classic(base_family = "Source Sans Pro", base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"), axis.title.x = element_text(face = "bold"),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.4),
    axis.text.x = element_text(angle = 75, hjust = .25, vjust = 0.5, color = "gray20"),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot"
  )
