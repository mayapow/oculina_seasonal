
##### Coral Physiology Oculina Seasonal Project#####

#Maya Powell & Jamie Long & Ella Hennessey
#Last edited Sept 23 2026

#install.packages(c("performance", "here"))
#load libraries
library(tidyverse)
library(here)
library(dplyr)
library(car)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rstatix")
library(rstatix)
#install.packages("tidyr")
library(tidyr)
#install.packages("performance")
library(performance)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("SIBER")
library(SIBER)

###Isotopes###

# Function to calculate standard error (SE)
se_fun <- function(x) {
  n <- sum(!is.na(x)) 
  if (n <= 1) return(NA_real_)
  sd(x, na.rm = TRUE) / sqrt(n)
}
# read in data
host <- read.csv(here("Data", "Nov_2024", "oculina_nov2024_isotopes_host.csv"))
algae <- read.csv(here("Data", "Nov_2024", "oculina_nov2024_isotopes_algae.csv"))

# reorder naming
host$depth_sa <- factor(
  host$depth_sa,
  levels = c(
    "Deep Aposymbiotic", "Shallow Aposymbiotic", "Shallow Symbiotic"))
algae$depth_sa <- factor(
  algae$depth_sa,
  levels = c("Deep Aposymbiotic","Shallow Aposymbiotic","Shallow Symbiotic"))

# Assign colors
depth_cols <- c(
  "Deep Aposymbiotic" = "cyan3",
  "Shallow Aposymbiotic" = "hotpink2",
  "Shallow Symbiotic" = "navy"
)

# Host d13C plot
host_summary <- host %>%
  group_by(depth_sa) %>%
  summarise(
    n = sum(!is.na(d13C_host)),
    mean = mean(d13C_host, na.rm = TRUE),
    se = se_fun(d13C_host),
    .groups = "drop"
  )

host_d13C_plot <- ggplot() +
  geom_jitter(
    data = host,
    aes(x = depth_sa, y = d13C_host, color = depth_sa),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  geom_errorbar(
    data = host_summary,
    aes(x = depth_sa, ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = host_summary,
    aes(x = depth_sa, y = mean),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")) +
  coord_cartesian(ylim = c(-25, -10)) +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1, size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 15, 10, 15)
  ) +
  labs(y = expression(delta^{13}*C~("\u2030")))

# Algae d13C plot
algae_summary <- algae %>%
  group_by(depth_sa) %>%
  summarise(
    n = sum(!is.na(d13C_algae)),
    mean = mean(d13C_algae, na.rm = TRUE),
    se = se_fun(d13C_algae),
    .groups = "drop"
  )

algae_d13C_plot <- ggplot() +
  geom_jitter(
    data = algae,
    aes(x = depth_sa, y = d13C_algae, color = depth_sa),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  geom_errorbar(
    data = algae_summary,
    aes(x = depth_sa, ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = algae_summary,
    aes(x = depth_sa, y = mean),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")) +
  coord_cartesian(ylim = c(-25, -10)) +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1, size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 15, 10, 15)
  ) +
  labs(y = expression(delta^{13}*C~("\u2030")))

# Host d15N plot
host_summary <- host %>%
  group_by(depth_sa) %>%
  summarise(
    n = sum(!is.na(d15N_host)),
    mean = mean(d15N_host, na.rm = TRUE),
    se = se_fun(d15N_host),
    .groups = "drop"
  )

host_d15N_plot <- ggplot() +
  geom_jitter(
    data = host,
    aes(x = depth_sa, y = d15N_host, color = depth_sa),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  geom_errorbar(
    data = host_summary,
    aes(x = depth_sa, ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = host_summary,
    aes(x = depth_sa, y = mean),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")) +
  coord_cartesian(ylim = c(0, 10.5)) +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1, size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 15, 10, 15)
  ) +
  labs(y = expression(delta^{15}*N~("\u2030")))

# Algae d15N plot
algae_summary <- algae %>%
  group_by(depth_sa) %>%
  summarise(
    n = sum(!is.na(d15N_algae)),
    mean = mean(d15N_algae, na.rm = TRUE),
    se = se_fun(d15N_algae),
    .groups = "drop"
  )

algae_d15N_plot <- ggplot() +
  geom_jitter(
    data = algae,
    aes(x = depth_sa, y = d15N_algae, color = depth_sa),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  geom_errorbar(
    data = algae_summary,
    aes(x = depth_sa, ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = algae_summary,
    aes(x = depth_sa, y = mean),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")) +
  coord_cartesian(ylim = c(0, 10.5)) +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1, size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 15, 10, 15)
  ) +
  labs(y = expression(delta^{15}*N~("\u2030")))

# Combine into one four-panel!
library(grid)

# Arrange plots
iso_plots <- ggarrange(
  host_d13C_plot,
  algae_d13C_plot,
  host_d15N_plot,
  algae_d15N_plot,
  nrow = 2,
  ncol = 2,
  labels = c("A", "B", "C", "D")
)

# Add host/algae titles
final_plot <- annotate_figure(
  iso_plots,
  top = text_grob(
    "Host                                  Algae",
    face = "bold",
    size = 18
  )
)

final_plot

# Save larger so points and labels are visible
ggsave(
  here("Output", "Nov_2024", "isotope_four_panel_plot.pdf"),
  iso_plots,
  width = 18,
  height = 12,
  dpi = 600
)

# Host isotope scatterplot
# d13C on x-axis, d15N on y-axis

host_CN_plot <- ggplot(
  host,
  aes(
    x = d13C_host,
    y = d15N_host,
    color = depth_sa
  )
) +
  geom_point(
    size = 3,
    alpha = 0.8
  ) +
  
  scale_color_manual(values = depth_cols) +
  
  theme_bw(base_size = 22) +
  
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "Host",
    x = expression(delta^{13}*C~("\u2030")),
    y = expression(delta^{15}*N~("\u2030")),
    color = "Treatment"
  )

host_CN_plot
ggsave(
  filename = here("Output", "Nov_2024", "host_CN_scatterplot.pdf"),
  plot = host_CN_plot,
  width = 6,
  height = 6,
  dpi = 600
)

# Algae isotope scatterplot
# d13C on x-axis, d15N on y-axis

algae_CN_plot <- ggplot(
  algae,
  aes(
    x = d13C_algae,
    y = d15N_algae,
    color = depth_sa
  )
) +
  geom_point(
    size = 3,
    alpha = 0.8
  ) +
  
  scale_color_manual(values = depth_cols) +
  
  theme_bw(base_size = 22) +
  
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "Algae",
    x = expression(delta^{13}*C~("\u2030")),
    y = expression(delta^{15}*N~("\u2030")),
    color = "Treatment"
  )

algae_CN_plot
ggsave(
  filename = here("Output", "Nov_2024", "algae_CN_scatterplot.pdf"),
  plot = algae_CN_plot,
  width = 6,
  height = 6,
  dpi = 600
)

### ella makes ellipses for isoptoes

# check number of samples in each dataset
nrow(host)
nrow(algae)

# check if sample IDs are unique within each dataset
anyDuplicated(host$sample_id)
anyDuplicated(algae$sample_id)

# how many sample IDs occur in both datasets
length(intersect(host$sample_id, algae$sample_id))

# IDs that occur in host but not algae
setdiff(host$sample_id, algae$sample_id)

# IDs that occur in algae but not host
setdiff(algae$sample_id, host$sample_id)

# Create matching IDs for host and algae samples

host <- host %>%
  mutate(pair_id = sub("-Host$", "", sample_id))

algae <- algae %>%
  mutate(pair_id = sub("-Algae$", "", sample_id))

# COMBINE HOST + ALGAE ISOTOPE DATA

# Create host dataframe in common format
host_iso <- host %>%
  transmute(
    pair_id = pair_id,
    colony_id = colony_id,
    depth_sa = depth_sa,
    tissue = "Host",
    d13C = d13C_host,
    d15N = d15N_host
  )

# Create algae dataframe in common format
algae_iso <- algae %>%
  transmute(
    pair_id = pair_id,
    colony_id = colony_id,
    depth_sa = depth_sa,
    tissue = "Algae",
    d13C = d13C_algae,
    d15N = d15N_algae
  )

# Stack host and algae together
isotope_combined <- bind_rows(host_iso, algae_iso)
isotope_combined$tissue <- factor(
  isotope_combined$tissue,
  levels = c("Host", "Algae"))

# Keep treatment groups in order
isotope_combined$depth_sa <- factor(
  isotope_combined$depth_sa,
  levels = c(
    "Deep Aposymbiotic",
    "Shallow Aposymbiotic",
    "Shallow Symbiotic"  ))

# colors
tissue_cols <- c(
  "Host" = "navy",
  "Algae" = "hotpink2")

# HOST + ALGAE d13C vs d15N PLOT

combined_CN_plot <- ggplot(
  isotope_combined,
  aes(
    x = d13C,
    y = d15N,
    color = tissue
  )) +
  
  # Individual isotope measurements
  geom_point(
    aes(shape = tissue),
    size = 3,
    alpha = 0.75
  ) +
  
  # 95% ellipse for Host and Algae
  stat_ellipse(
    aes(group = tissue),
    type = "norm",
    level = 0.95,
    linewidth = 1
  ) +
  
  # Separate the three groups
  facet_wrap(
    ~depth_sa,
    nrow = 1,
    labeller = as_labeller(
      c(
        "Deep Aposymbiotic" = "Deep Aposymbiotic",
        "Shallow Aposymbiotic" = "Shallow Aposymbiotic",
        "Shallow Symbiotic" = "Shallow Symbiotic"))) +
  
  # Host and algae colors
  scale_color_manual(values = tissue_cols) +
  
  # Different point shapes
  scale_shape_manual(
    values = c(
      "Host" = 16,
      "Algae" = 17)) +
  
  # Axis labels w isotope standards
  labs(
    x = expression(delta^{13}*C~("\u2030 VPDB")),
    y = expression(delta^{15}*N~("\u2030 Air")),
    color = "Tissue",
    shape = "Tissue") +
  
  # Formatting
  theme_bw(base_size = 20) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    strip.text = element_text(
      size = 16,
      face = "bold"
    ),
    
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 14),
    
    legend.position = "right" )

# Call plot!
combined_CN_plot

#=======================
#HOST - SYM iso differences

# Pair host and algae from same sample
HS_data <- host %>%
  select(
    pair_id,
    depth_sa,
    d13C_host,
    d15N_host
  ) %>%
  inner_join(
    algae %>%
      select(
        pair_id,
        d13C_algae,
        d15N_algae
      ),
    by = "pair_id"
  )

# Calculate H-S
HS_data <- HS_data %>%
  mutate(
    d13C_HS = d13C_host - d13C_algae,
    d15N_HS = d15N_host - d15N_algae
  )

# Check the new data
head(HS_data)

# Check sample sizes for each group
table(HS_data$depth_sa)


# SUMMARY STATS

# d13C H-S
HS_d13C_summary <- HS_data %>%
  group_by(depth_sa) %>%
  summarise(
    n = sum(!is.na(d13C_HS)),
    mean = mean(d13C_HS, na.rm = TRUE),
    se = se_fun(d13C_HS),
    .groups = "drop"
  )

HS_d13C_summary


# d15N H-S
HS_d15N_summary <- HS_data %>%
  group_by(depth_sa) %>%
  summarise(
    n = sum(!is.na(d15N_HS)),
    mean = mean(d15N_HS, na.rm = TRUE),
    se = se_fun(d15N_HS),
    .groups = "drop"
  )

HS_d15N_summary

# d13C H-S JITTER PLOT

HS_d13C_plot <- ggplot() +
  
  # Individual samples
  geom_jitter(
    data = HS_data,
    aes(
      x = depth_sa,
      y = d13C_HS,
      color = depth_sa
    ),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  
  # Mean +/- SE
  geom_errorbar(
    data = HS_d13C_summary,
    aes(
      x = depth_sa,
      ymin = mean - se,
      ymax = mean + se
    ),
    width = 0.18,
    linewidth = 1
  ) +
  
  # Mean
  geom_point(
    data = HS_d13C_summary,
    aes(
      x = depth_sa,
      y = mean
    ),
    size = 4,
    color = "black"
  ) +
  
  scale_color_manual(values = depth_cols) +
  
  scale_x_discrete(
    labels = c(
      "Deep Apo",
      "Shallow Apo",
      "Shallow Sym"
    )
  ) +
  
  theme_bw(base_size = 22) +
  
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 14
    ),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 15, 10, 15)
  ) +
  
  labs(
    y = expression(delta^{13}*C[H-S]~("\u2030"))
  )

HS_d13C_plot

# d15N H-S JITTER PLOT

HS_d15N_plot <- ggplot() +
  
  # Individual samples
  geom_jitter(
    data = HS_data,
    aes(
      x = depth_sa,
      y = d15N_HS,
      color = depth_sa
    ),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  
  # Mean +/- SE
  geom_errorbar(
    data = HS_d15N_summary,
    aes(
      x = depth_sa,
      ymin = mean - se,
      ymax = mean + se
    ),
    width = 0.18,
    linewidth = 1
  ) +
  
  # Mean
  geom_point(
    data = HS_d15N_summary,
    aes(
      x = depth_sa,
      y = mean
    ),
    size = 4,
    color = "black"
  ) +
  
  scale_color_manual(values = depth_cols) +
  
  scale_x_discrete(
    labels = c(
      "Deep Apo",
      "Shallow Apo",
      "Shallow Sym"
    )
  ) +
  
  theme_bw(base_size = 22) +
  
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 14
    ),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 15, 10, 15)
  ) +
  
  labs(
    y = expression(delta^{15}*N[H-S]~("\u2030"))
  )

HS_d15N_plot

#letters
HS_d13C_letters <- data.frame(
  depth_sa = factor(
    c("Deep Aposymbiotic",
      "Shallow Aposymbiotic",
      "Shallow Symbiotic"),
    levels = levels(HS_data$depth_sa)
  ),
  letter = c("a", "a", "a")
)

HS_d13C_plot <- HS_d13C_plot +
  geom_text(
    data = HS_d13C_letters,
    aes(x = depth_sa, y = Inf, label = letter),
    vjust = 1.5,
    size = 7,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  coord_cartesian(clip = "off")

HS_d15N_letters <- data.frame(
  depth_sa = factor(
    c("Deep Aposymbiotic",
      "Shallow Aposymbiotic",
      "Shallow Symbiotic"),
    levels = levels(HS_data$depth_sa)
  ),
  letter = c("a", "a", "b")
)

HS_d15N_plot <- HS_d15N_plot +
  geom_text(
    data = HS_d15N_letters,
    aes(x = depth_sa, y = Inf, label = letter),
    vjust = 1.5,
    size = 7,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  coord_cartesian(clip = "off")

HS_plots <- ggarrange(
  HS_d13C_plot,
  HS_d15N_plot,
  ncol = 2,
  nrow = 1,
  labels = c("A", "B")
)

HS_plots

ggsave(
  here("Output", "Nov_2024", "host_minus_symbiont_isotopes.png"),
  plot = HS_plots,
  width = 12,
  height = 6
)

# PUT BOTH H-S PLOTS TOGETHER

HS_plots <- ggarrange(
  HS_d13C_plot,
  HS_d15N_plot,
  ncol = 2,
  nrow = 1,
  labels = c("A", "B")
)

HS_plots

# SAVE FIGURE

ggsave(
  here(
    "Output",
    "Nov_2024",
    "host_minus_symbiont_isotopes.pdf"
  ),
  HS_plots,
  width = 12,
  height = 6,
  dpi = 600
)

# ANOVA: HOST - SYMBIONT 

# d13C H-S

HS_d13C_aov <- aov(
  d13C_HS ~ depth_sa,
  data = HS_data
)

# Overall ANOVA
summary(HS_d13C_aov)

# Pairwise comparisons
TukeyHSD(HS_d13C_aov)

#------------------------------------------------------------
# d15N H-S

HS_d15N_aov <- aov(
  d15N_HS ~ depth_sa,
  data = HS_data
)

# Overall ANOVA
summary(HS_d15N_aov)

# Pairwise comparisons
TukeyHSD(HS_d15N_aov)
#======================
# H + S
# Match host and symbiont samples
HplusS_data <- host %>%
  select(pair_id, depth_sa, d13C_host, d15N_host) %>%
  inner_join(
    algae %>%
      select(pair_id, d13C_algae, d15N_algae),
    by = "pair_id"
  ) %>%
  mutate(
    d13C_HplusS = d13C_host + d13C_algae,
    d15N_HplusS = d15N_host + d15N_algae
  )

# View combined data
head(HplusS_data)

# Summary statistics
HplusS_summary <- HplusS_data %>%
  group_by(depth_sa) %>%
  summarise(
    n_d13C = sum(!is.na(d13C_HplusS)),
    mean_d13C = mean(d13C_HplusS, na.rm = TRUE),
    se_d13C = se_fun(d13C_HplusS),
    
    n_d15N = sum(!is.na(d15N_HplusS)),
    mean_d15N = mean(d15N_HplusS, na.rm = TRUE),
    se_d15N = se_fun(d15N_HplusS),
    
    .groups = "drop"
  )

HplusS_summary

HplusS_d13C_plot <- ggplot() +
  geom_jitter(
    data = HplusS_data,
    aes(x = depth_sa, y = d13C_HplusS,
        color = depth_sa),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  geom_errorbar(
    data = HplusS_summary,
    aes(x = depth_sa,
        ymin = mean_d13C - se_d13C,
        ymax = mean_d13C + se_d13C),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = HplusS_summary,
    aes(x = depth_sa, y = mean_d13C),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(
    labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")
  ) +
  labs(
    x = NULL,
    y = expression(delta^{13}*C~"(H + S)")
  ) +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 35, hjust = 1, size = 14
    ),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

HplusS_d13C_plot

HplusS_d15N_plot <- ggplot() +
  geom_jitter(
    data = HplusS_data,
    aes(x = depth_sa, y = d15N_HplusS,
        color = depth_sa),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  geom_errorbar(
    data = HplusS_summary,
    aes(x = depth_sa,
        ymin = mean_d15N - se_d15N,
        ymax = mean_d15N + se_d15N),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = HplusS_summary,
    aes(x = depth_sa, y = mean_d15N),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(
    labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")
  ) +
  labs(
    x = NULL,
    y = expression(delta^{15}*N~"(H + S)")
  ) +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 35, hjust = 1, size = 14
    ),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

HplusS_d15N_plot

# Carbon ANOVA
HplusS_d13C_aov <- aov(
  d13C_HplusS ~ depth_sa,
  data = HplusS_data
)

summary(HplusS_d13C_aov)

TukeyHSD(HplusS_d13C_aov)


# Nitrogen ANOVA
HplusS_d15N_aov <- aov(
  d15N_HplusS ~ depth_sa,
  data = HplusS_data
)

summary(HplusS_d15N_aov)

TukeyHSD(HplusS_d15N_aov)

HplusS_plots <- ggarrange(
  HplusS_d13C_plot,
  HplusS_d15N_plot,
  ncol = 2,
  nrow = 1,
  labels = c("A", "B")
)

HplusS_plots

# SIGNIFICANCE LETTERS FOR H + S

# Carbon letters
HplusS_d13C_letters <- HplusS_data %>%
  group_by(depth_sa) %>%
  summarise(
    y = max(d13C_HplusS, na.rm = TRUE) + 0.3,
    .groups = "drop"
  ) %>%
  mutate(letter = c("a", "a", "a"))

HplusS_d13C_plot <- HplusS_d13C_plot +
  geom_text(
    data = HplusS_d13C_letters,
    aes(x = depth_sa, y = y, label = letter),
    size = 7,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  expand_limits(
    y = max(HplusS_d13C_letters$y) + 0.3
  )


# Nitrogen letters
HplusS_d15N_letters <- HplusS_data %>%
  group_by(depth_sa) %>%
  summarise(
    y = max(d15N_HplusS, na.rm = TRUE) + 0.3,
    .groups = "drop"
  ) %>%
  mutate(
    letter = case_when(
      depth_sa == "Shallow Symbiotic" ~ "b",
      TRUE ~ "a"
    )
  )

HplusS_d15N_plot <- HplusS_d15N_plot +
  geom_text(
    data = HplusS_d15N_letters,
    aes(x = depth_sa, y = y, label = letter),
    size = 7,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  expand_limits(
    y = max(HplusS_d15N_letters$y) + 0.3
  )

ggsave(
  here("Output", "Nov_2024",
       "host_plus_symbiont_isotopes.png"),
  plot = HplusS_plots,
  width = 12,
  height = 6,
  dpi = 600,
  device = function(...)
    grDevices::png(..., type = "quartz")
)

#============================================================
# SIBER ANALYSIS - NOVEMBER 2024 OCULINA
# HOST AND ALGAE

library(SIBER)
library(dplyr)
library(tidyr)

# HOST
# Format host data for SIBER
host_siber <- host %>%
  transmute(
    iso1 = d13C_host,
    iso2 = d15N_host,
    group = case_when(
      depth_sa == "Deep Aposymbiotic" ~ 1,
      depth_sa == "Shallow Aposymbiotic" ~ 2,
      depth_sa == "Shallow Symbiotic" ~ 3
    ),
    community = 1
  ) %>%
  drop_na(iso1, iso2, group)


# Create SIBER object
host_siber_object <- createSiberObject(host_siber)


# Check sample sizes
host_siber_object[["sample.sizes"]]

# HOST PLOT

community.hulls.args <- list(
  col = 1,
  lty = 1,
  lwd = 1
)

group.ellipses.args <- list(
  n = 100,
  p.interval = 0.95,
  lty = 1,
  lwd = 2
)

group.hulls.args <- list(
  lty = 2,
  col = "grey20"
)


plotSiberObject(
  host_siber_object,
  ax.pad = 2,
  hulls = FALSE,
  community.hulls.args = community.hulls.args,
  ellipses = TRUE,
  group.ellipses.args = group.ellipses.args,
  group.hulls = FALSE,
  group.hulls.args = group.hulls.args,
  bty = "L",
  iso.order = c(1,2),
  xlab = expression(delta^{13}*C~("\u2030")),
  ylab = expression(delta^{15}*N~("\u2030"))
)

# HOST TA, SEA, SEAc

host_metrics <- groupMetricsML(host_siber_object)

host_metrics

# Make table

host_metrics_named <- data.frame(
  
  Group = c(
    "Deep Aposymbiotic",
    "Shallow Aposymbiotic",
    "Shallow Symbiotic"
  ),
  
  TA = c(
    host_metrics["TA", "1.1"],
    host_metrics["TA", "1.2"],
    host_metrics["TA", "1.3"]
  ),
  
  SEA = c(
    host_metrics["SEA", "1.1"],
    host_metrics["SEA", "1.2"],
    host_metrics["SEA", "1.3"]
  ),
  
  SEAc = c(
    host_metrics["SEAc", "1.1"],
    host_metrics["SEAc", "1.2"],
    host_metrics["SEAc", "1.3"]
  )
)

host_metrics_named

# HOST ELLIPSE OVERLAP
# Deep Apo vs Shallow Apo

host_deep_vs_shallow_apo <- maxLikOverlap(
  "1.1",
  "1.2",
  host_siber_object,
  p.interval = 0.95,
  n = 100
)

host_deep_vs_shallow_apo

# Deep Apo vs Shallow Sym
host_deep_vs_shallow_sym <- maxLikOverlap(
  "1.1",
  "1.3",
  host_siber_object,
  p.interval = 0.95,
  n = 100
)

host_deep_vs_shallow_sym


# Shallow Apo vs Shallow Sym
host_shallow_apo_vs_sym <- maxLikOverlap(
  "1.2",
  "1.3",
  host_siber_object,
  p.interval = 0.95,
  n = 100
)

host_shallow_apo_vs_sym

# Calculate proportional overlap
host_overlap_proportions <- data.frame(
  
  Comparison = c(
    "Deep Apo vs Shallow Apo",
    "Deep Apo vs Shallow Sym",
    "Shallow Apo vs Shallow Sym"
  ),
  
  Proportional_Overlap = c(
    
    host_deep_vs_shallow_apo[3] /
      (host_deep_vs_shallow_apo[1] +
         host_deep_vs_shallow_apo[2] -
         host_deep_vs_shallow_apo[3]),
    
    host_deep_vs_shallow_sym[3] /
      (host_deep_vs_shallow_sym[1] +
         host_deep_vs_shallow_sym[2] -
         host_deep_vs_shallow_sym[3]),
    
    host_shallow_apo_vs_sym[3] /
      (host_shallow_apo_vs_sym[1] +
         host_shallow_apo_vs_sym[2] -
         host_shallow_apo_vs_sym[3])
  )
)

host_overlap_proportions

#============================================================
# ALGAE
# Format algae data for SIBER

algae_siber <- algae %>%
  transmute(
    iso1 = d13C_algae,
    iso2 = d15N_algae,
    group = case_when(
      depth_sa == "Deep Aposymbiotic" ~ 1,
      depth_sa == "Shallow Aposymbiotic" ~ 2,
      depth_sa == "Shallow Symbiotic" ~ 3
    ),
    community = 1
  ) %>%
  drop_na(iso1, iso2, group)


# Create SIBER object

algae_siber_object <- createSiberObject(algae_siber)


# Check sample sizes

algae_siber_object[["sample.sizes"]]

# ALGAE PLOT

plotSiberObject(
  algae_siber_object,
  ax.pad = 2,
  hulls = FALSE,
  community.hulls.args = community.hulls.args,
  ellipses = TRUE,
  group.ellipses.args = group.ellipses.args,
  group.hulls = FALSE,
  group.hulls.args = group.hulls.args,
  bty = "L",
  iso.order = c(1,2),
  xlab = expression(delta^{13}*C~("\u2030")),
  ylab = expression(delta^{15}*N~("\u2030"))
)

# ALGAE TA, SEA, SEAc

algae_metrics <- groupMetricsML(algae_siber_object)

algae_metrics


# Make readable table

algae_metrics_named <- data.frame(
  
  Group = c(
    "Deep Aposymbiotic",
    "Shallow Aposymbiotic",
    "Shallow Symbiotic"
  ),
  
  TA = c(
    algae_metrics["TA", "1.1"],
    algae_metrics["TA", "1.2"],
    algae_metrics["TA", "1.3"]
  ),
  
  SEA = c(
    algae_metrics["SEA", "1.1"],
    algae_metrics["SEA", "1.2"],
    algae_metrics["SEA", "1.3"]
  ),
  
  SEAc = c(
    algae_metrics["SEAc", "1.1"],
    algae_metrics["SEAc", "1.2"],
    algae_metrics["SEAc", "1.3"]
  )
)

algae_metrics_named

# ALGAE ELLIPSE OVERLAP
# Deep Apo vs Shallow Apo

algae_deep_vs_shallow_apo <- maxLikOverlap(
  "1.1",
  "1.2",
  algae_siber_object,
  p.interval = 0.95,
  n = 100
)

algae_deep_vs_shallow_apo


# Deep Apo vs Shallow Sym
algae_deep_vs_shallow_sym <- maxLikOverlap(
  "1.1",
  "1.3",
  algae_siber_object,
  p.interval = 0.95,
  n = 100
)

algae_deep_vs_shallow_sym


# Shallow Apo vs Shallow Sym
algae_shallow_apo_vs_sym <- maxLikOverlap(
  "1.2",
  "1.3",
  algae_siber_object,
  p.interval = 0.95,
  n = 100
)

algae_shallow_apo_vs_sym


# Calculate proportional overlap

algae_overlap_proportions <- data.frame(
  
  Comparison = c(
    "Deep Apo vs Shallow Apo",
    "Deep Apo vs Shallow Sym",
    "Shallow Apo vs Shallow Sym"
  ),
  
  Proportional_Overlap = c(
    
    algae_deep_vs_shallow_apo[3] /
      (algae_deep_vs_shallow_apo[1] +
         algae_deep_vs_shallow_apo[2] -
         algae_deep_vs_shallow_apo[3]),
    
    algae_deep_vs_shallow_sym[3] /
      (algae_deep_vs_shallow_sym[1] +
         algae_deep_vs_shallow_sym[2] -
         algae_deep_vs_shallow_sym[3]),
    
    algae_shallow_apo_vs_sym[3] /
      (algae_shallow_apo_vs_sym[1] +
         algae_shallow_apo_vs_sym[2] -
         algae_shallow_apo_vs_sym[3])
  )
)

algae_overlap_proportions

# FINAL RESULTS

host_metrics_named
host_overlap_proportions

algae_metrics_named
algae_overlap_proportions

#ISOTOPE ELLIPSE PLOTS

library(ggplot2)
library(dplyr)

depth_cols <- c(
  "Deep Aposymbiotic" = "cyan3",
  "Shallow Aposymbiotic" = "hotpink2",
  "Shallow Symbiotic" = "navy"
)

# HOST ISOTOPE PLOT

host_siber_plot <- ggplot(
  host,
  aes(
    x = d13C_host,
    y = d15N_host,
    color = depth_sa
  )
) +
  
  # Individual samples
  geom_point(
    size = 3,
    alpha = 0.7
  ) +
  
  # 95% ellipses
  stat_ellipse(
    aes(
      group = depth_sa,
      color = depth_sa
    ),
    type = "norm",
    level = 0.95,
    linewidth = 1.2
  ) +
  
  # Group colors
  scale_color_manual(
    values = depth_cols,
    name = NULL
  ) +
  
  # Axis labels WHY NO WORK???
  labs(
    x = bquote(delta^13*C~("\u2030 VPDB")),
    y = bquote(delta^15*N~("\u2030 Air"))
  )
  # formatting
  theme_classic(base_size = 18) +
  
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5),
    
    axis.title = element_text(
      size = 18),
    
    axis.text = element_text(
      size = 14),
    
    legend.text = element_text(
      size = 14),
    
    legend.position = "right")


# Show host plot
host_siber_plot

# ALGAE ISOTOPE PLOT

algae_siber_plot <- ggplot(
  algae,
  aes(
    x = d13C_algae,
    y = d15N_algae,
    color = depth_sa
  )
) +
  
  # Individual samples
  geom_point(
    size = 3,
    alpha = 0.7
  ) +
  
  # 95% ellipses
  stat_ellipse(
    aes(
      group = depth_sa,
      color = depth_sa
    ),
    type = "norm",
    level = 0.95,
    linewidth = 1.2
  ) +
  
  # Group colors
  scale_color_manual(
    values = depth_cols,
    name = NULL
  ) +
  
  # Axis labels???
  labs(
    x = expression(delta^{13}*C~"(\u2030 VPDB)"),
    y = expression(delta^{15}*N~"(\u2030 Air)")
  ) +
  
  # Clean formatting
  theme_classic(base_size = 18) +
  
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5
    ),
    
    axis.title = element_text(
      size = 18
    ),
    
    axis.text = element_text(
      size = 14
    ),
    
    legend.text = element_text(
      size = 14
    ),
    
    legend.position = "right"
  )


# Show algae plot
algae_siber_plot

# COMBINE HOST + ALGAE SIBER PLOTS

library(ggpubr)

combined_siber_plot <- ggarrange(
  host_siber_plot,
  algae_siber_plot,
  
  ncol = 2,
  nrow = 1,
  
  # Add panel labels
  labels = c("A", "B"),
  
  # Use one shared legend
  common.legend = TRUE,
  legend = "bottom"
)

# Show & save combined figure
combined_siber_plot

ggsave(
  filename = "host_algae_SIBER_isotopes.png",
  plot = combined_siber_plot,
  width = 12,
  height = 6,
  dpi = 600,
  device = function(...) grDevices::png(..., type = "quartz")
)




#####Ash Free Dry Weight#####

#read in metadata
phys_meta <- read.csv(here("Data", "Nov_2024", "oculina_nov24_chla.csv"))
#load data
DW <- read.csv(here("Data", "Physiology", "Dry_Weight_Oki2025.csv"))

DW <- DW %>% left_join(phys_meta, by = "frag_ID")

#calculate dry weight
DW <- DW %>%
  mutate(dw_g_mL = dry_pan - pan_weight,
         dw_total_g = dw_g_mL*blastate_vol_mL,
         dw_g_cm2 = dw_total_g/SA_cm2,
         dw_mg_cm2 = dw_g_cm2*1000)

#average across the reps
avg_DW <- DW %>%
  group_by(frag_ID) %>%
  summarise(dw_g_mL = mean(dw_g_mL),
            dw_total_g = mean(dw_total_g),
            dw_g_cm2 = mean(dw_g_cm2),
            dw_mg_cm2 = mean(dw_mg_cm2)) %>%
  left_join(phys_meta, by = "frag_ID")

write.csv(avg_DW, here("Data", "Physiology", "Average_Dry_Weight.csv"), row.names = FALSE)
avg_DW <- read.csv(here("Data", "Average_Dry_Weight.csv"))

##boxplot by species
#and add facet by morphology or stress (sensitive/tolerant)
dw_box <- ggplot(avg_DW, aes(x = species, y=dw_mg_cm2, fill = species))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Coral Species", y = "Dry Weight (mg/cm2)")+
  theme(legend.position = "none") +
  #facet_wrap(.~stress, scales = "free")+
  ylim(25,325)
#scale_fill_manual(values = c("tan","brown"))
dw_box

ggsave(here("Output", "Physiology", "dryweight_box.pdf"), dw_box, h = 5, w = 8)

#summarize mean, sd, se
avg_DW_spp <- avg_DW %>% 
  group_by(species) %>% 
  summarise(avg_dw_mg_cm2 = mean(dw_mg_cm2),
            sd_dw_mg_cm2 = sd(dw_mg_cm2),
            se_dw_mg_cm2 = sd(dw_mg_cm2)/sqrt(n())
  )

#quickly see if there is dif between species
DW.mod.spp <- lm(dw_mg_cm2~species, data = avg_DW)
Anova(DW.mod.spp)
summary(DW.mod.spp)

#and sensitivity
DW.mod.stress <- lm(dw_mg_cm2~stress, data = avg_DW)
Anova(DW.mod.stress)
summary(DW.mod.stress)
#based on graph, looks like Peyd and Tfro should be switched
#we'll see what happens with the rest of the TPC data!


##### read in Chlorophyll a NOV 24#####

#sym_chla <- read.csv(here("Data", "Nov_2024", "oculina_nov24_chla.csv"))
#meta <- read.csv(here("Data", "Nov_2024", "oculina_nov24_metadata.csv"))

#remove samples that have no data
#sym_chla <- sym_chla %>% 
#  left_join(meta, by = "sample_ID")
#sym_chla <- sym_chla %>% filter(full_ID != "A12-1124") 

#checks do see if data is still in sample set
sym_chla %>% 
  filter(full_ID == "A12-1124")
#here A12 from Nov 24 has no surface area yet so we can't use it

#you can check the classes of the variables using this as needed:
sapply(sym_chla, class)

#convert any variables to correct classes as needed - see example here
sym_chla <- sym_chla %>% mutate_at(c('ug_chla_cm'), as.numeric)



#########

#read in SEASONAL DATAFRAME & remove samples w/ no data
sym_chla <- read.csv(here("Data", "Seasonal", "sym_chla_oculina_seasonal.csv"))
meta <- read.csv(here("Data", "Seasonal", "oculina_seasonal_metadata.csv"))

sym_chla <- sym_chla %>% 
  left_join(meta, by = "full_ID")
sym_chla <- sym_chla %>% filter(full_ID != "A12-1124") 

#checks do see if data is still in sample set
sym_chla %>% 
  filter(full_ID == "A12-1124")
#here A12 from Nov 24 has no surface area yet so we can't use it

sym_chla <- sym_chla %>%
  filter(full_ID != "A12-1124") %>%
  mutate(timepoint = factor(timepoint.y, levels = c("Nov_23", "May_24", "Nov_24")))


#reorder variables so the timepoints go in chronological order
sym_chla$timepoint <- factor(sym_chla$timepoint, levels=c('Nov_23', 'May_24', 'Nov_24'))


#making timepoints
sym_chla <- sym_chla %>%
  mutate(timepoint = case_when(
    month == "November" & year == 2023 ~ "Nov_23",
    month == "May" & year == 2024 ~ "May_24",
    month == "November" & year == 2024 ~ "Nov_24"
  ))
str(sym_chla$month)

###Seasonal Timepoint Chla cm^2 box plot
chla_time_box <- ggplot(sym_chla, aes(x = timepoint, y = ug_chla_cm, fill = timepoint)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.8, width = 0.2) +
  theme_classic(base_size = 22) +
  theme(legend.position = "none") +
  labs(x = "Timepoint", y = "ug chla per cm2") +
  scale_fill_manual(values = c("navy", "pink", "cyan3"))

chla_time_box

##Ginormous 3 panel plot -- sym density, chla/cm2, chla/symbiont

library(dplyr)
library(ggplot2)
library(patchwork)

# Ensure correct timepoint order
sym_chla$timepoint <- factor(sym_chla$timepoint,
                             levels = c("Nov_23", "May_24", "Nov_24"))

# title font theme
my_theme <- theme(
  legend.position = "none",
  plot.title = element_text(face = "bold", hjust = 0.5)
)

p1 <- ggplot(sym_chla, aes(x = timepoint, y = sym_cm2, fill = timepoint)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "black", alpha = 1, size = 1.8, width = 0.2) +
  scale_fill_manual(values = c("navy", "pink", "cyan3")) +
  labs(
    title = "Symbiont density",
    x = "Timepoint",
    y = expression(cells~cm^{-2}),
    fill = "Timepoint"
  ) +
  theme_classic(base_size = 18) +
  my_theme

# Panel 2: Chlorophyll a per cm2
p2 <- ggplot(sym_chla, aes(x = timepoint, y = ug_chla_cm, fill = timepoint)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "black", alpha = 1, size = 1.8, width = 0.2) +
  scale_fill_manual(values = c("navy", "pink", "cyan3")) +
  labs(
    title = expression(bold("Chl " * italic(a))),
    y = expression(mu*g~cm^{-2}),
    fill = "Timepoint"
  ) +
  theme_classic(base_size = 18) +
  my_theme

# Panel 3: Chlorophyll a per symbiont
p3 <- ggplot(sym_chla, aes(x = timepoint, y = pg_chla_sym, fill = timepoint)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "black", alpha = 1, size = 1.8, width = 0.2) +
  scale_fill_manual(values = c("navy", "pink", "cyan3")) +
  labs(
    title = expression(bold("Chl " * italic(a) * " per symbiont")),
    x = "Timepoint",
    y = expression(pg~sym^{-1}),
    fill = "Timepoint"
  ) +
  theme_classic(base_size = 18) +
  my_theme

# Combining panels
three_panel <- p2 + p1 + p3
#Call plot
three_panel
#save plot
ggsave(three_panel, file = "three_panel.pdf", h=6, w=6) #change height and width and resave as needed

###STATS FOR 3 PANEL
#sym density
sym_aov <- aov(sym_cm2 ~ timepoint, data = sym_chla)
summary(sym_aov)

TukeyHSD(sym_aov)
#chla per cm2
chla_aov <- aov(ug_chla_cm ~ timepoint, data = sym_chla)
summary(chla_aov)

TukeyHSD(chla_aov)
#chl a per symbiont
chla_sym_aov <- aov(pg_chla_sym ~ timepoint, data = sym_chla)
summary(chla_sym_aov)

TukeyHSD(chla_sym_aov)
#info about variables - add notes that are helpful to you!
#sa_colony = aposymbiotic/symbiotic original designation of the colony
#timepoint = month/year combo for when data was taken
#ug_chla_cm = micrograms of chlorophyll a per cm2 of surface area
#sym_cm2 = symbiont density for cm2 of surface area
#pg_chla_sym = picograms of chlorophyll a per symbiont

#look at outliers - commented out bc not doing rn @Ella
#sym_cm
# density_out <- sym_chla %>%
#   #group_by(sa_colony) %>%
#   identify_outliers(sym_cm2)
# density_out
# 
# #chla_cm
# chla_out <- sym_chla %>%
#   group_by(sa_colony) %>%
#   identify_outliers(ug_chla_cm)
# chla_out
# 
# #chla_sym
# chla_sym_out <- sym_chla %>%
#   group_by(sym_apo) %>%
#   identify_outliers(chla_sym)
# chla_sym_out


###PLOTS###
#USING FOR 395
#dot plot of chlorophyll a looking at all samples split and colored by apo_sym
#also looking at the different time points
chla_sa_point <- ggplot(sym_chla, aes(x = sample_ID.x, y=ug_chla_cm, color = sa_colony.x))+
  geom_point(alpha=0.8, size = 3)+
  theme_classic(base_size = 22)+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(sa_colony.x~timepoint, scales = "free")
chla_sa_point

#@Ella - here's how to save a plot - it will save in your working directory folder
#go through and save the rest of them by editing this line of code for each plot!
ggsave(chla_sa_point, file = "chla_sa_point.pdf", h=6, w=6) #change height and width and resave as needed

#now look at it without apo & sym just seasonal
chla_time_point <- ggplot(sym_chla, aes(x = sample_ID.x, y=ug_chla_cm, color = timepoint))+
  geom_point(alpha=0.8, size= 3)+
  theme_classic(base_size = 22)+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(~timepoint, scales = "free")
chla_time_point

ggsave(chla_time_point, file = "chla_time_point.jpg", h=6, w=6)
#boxplot of chla grouped by sym/apo and facet by time
#this is interesting but u can see the original designation of the colonies as apo/sym doesn't seem to hold - look at it over time too
chla_sa_box <- ggplot(sym_chla, aes(x = sa_colony, y=ug_chla_cm, fill = sa_colony))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Symbiotic state - between colony", y = "ug chla per cm2")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("tan","brown")) #ella change the colors here as you'd like
chla_sa_box

ggsave(chla_sa_box, file = "chla_sa_box.jpg", h=6, w=6)
#boxplot of chla grouped by sym/apo and facet by time
#within timepoint the apo/sym seems important for Nov 2023 and maybe even may 2024, but by Nov 24 - all colonies are pretty much apo again
#also remember for Nov 24 we are looking at averages of sym & apo branches within colony
chla_sa_time_box <- ggplot(sym_chla, aes(x = sa_colony, y=ug_chla_cm, fill = sa_colony))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  facet_wrap(~timepoint)+
  labs(x = "Symbiotic state", y = "ug chla per cm2")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("tan","brown")) #ella change the colors here as you'd like
chla_sa_time_box

ggsave(chla_sa_time_box, file = "chla_sa_time_box.jpg", h=6, w=6)
#boxplot of chla grouped by time only
#just timepoint seems like it's a pretty effective predictor of chla in my opinion!
#really cool to see the data like this and see that your Nov samples are matching Jamie's pretty well even with the split colonies
chla_time_box <- ggplot(sym_chla, aes(x = timepoint, y=ug_chla_cm, fill = timepoint))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  theme(legend.position = "none")+
  labs(x = "Timepoint", y = "ug chla per cm2") +
  scale_fill_manual(values = c("navy","pink", "cyan3")) #ella change the colors here as you'd like
chla_time_box

ggsave("chla_time_box.pdf", chla_time_line, h = 10, w = 15)


#now tracking colonies over time

#make sure time is ordered and a factor - just in case/if you didn't already
time_levels <- sym_chla %>% distinct(timepoint) %>% pull(timepoint)
#reorder so that the timepoints go in chronological order
sym_chla$timepoint <- factor(sym_chla$timepoint, levels=c('Nov_23', 'May_24', 'Nov_24'))

#add missing data combinations where frags don't have data so lines break across gaps
sym_chla_time <- sym_chla %>%
  group_by(sample_ID.y) %>%
  complete(timepoint = time_levels) %>%
  ungroup()

#chla over time for each colony
#eeee I can't wait to see the rest of this data filled in it's going to be so cool!!!
#this plot is a bit chaotic but I think important to show how individual colonies change over time
chla_time_line <- ggplot(sym_chla_time, aes(x = timepoint, y = ug_chla_cm, group = sample_ID.y)) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(aes(fill = timepoint), size = 3, shape = 21, color = "black", stroke = 0.8) +  scale_color_manual(values = c("navy", "pink", "cyan3")) +
  scale_fill_manual(values = c("navy", "pink", "cyan3")) +
  labs(x = "Timepoint", y = "ug chla per cm2", color = "Sample ID") +
  theme_classic(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "right")

chla_time_line
#this will say its removing rows with values outside the scale range
#that's just bc it's removing the rows without data so it's totally fine!!

chla_time_line_sa <- chla_time_line +
  facet_wrap(~sa_colony.x)
chla_time_line_sa

ggsave("chla_time_line_colony_oculina_seasonal.pdf", chla_time_line, h = 10, w = 15)

ggsave("chla_time_line_colony_sa_oculina_seasonal.pdf", chla_time_line_sa, h = 10, w = 15)

#####Symbiont Density #####
sym <- read.csv(here("Data", "Nov_2024", "oculina_nov24_sym_density.csv"))

sym <- sym %>% left_join(phys_meta, by = "sample_ID")

#calculate dry weight
DW <- DW %>%
  mutate(dw_g_mL = dry_pan - pan_weight,
         dw_total_g = dw_g_mL*blastate_vol_mL,
         dw_g_cm2 = dw_total_g/SA_cm2,
         dw_mg_cm2 = dw_g_cm2*1000)

#average across the reps
avg_DW <- DW %>%
  group_by(frag_ID) %>%
  summarise(dw_g_mL = mean(dw_g_mL),
            dw_total_g = mean(dw_total_g),
            dw_g_cm2 = mean(dw_g_cm2),
            dw_mg_cm2 = mean(dw_mg_cm2)) %>%
  left_join(phys_meta, by = "frag_ID")

write.csv(avg_DW, here("Data", "Physiology", "Average_Dry_Weight.csv"), row.names = FALSE)
avg_DW <- read.csv(here("Data", "Average_Dry_Weight.csv"))


####STATS####

#sometimes you need to edit variables to make them be factors for the stats to work - see example here:
#color_avg_nox$month <- as.factor(color_avg_nox$month)

#look to see if means of different groups are the same/different
#chla_cm
chla_as_summ <- sym_chla %>% group_by(sa_colony.x) %>% get_summary_stats(ug_chla_cm, type = "mean_sd")
chla_as_summ #huge sd - probs no difference in stats
chla_time_summ <- sym_chla %>% group_by(timepoint.x) %>% get_summary_stats(ug_chla_cm, type = "mean_sd")
chla_time_summ #looks like def a difference between may/the novs, maybe all 3
chla_as_time_summ <- sym_chla %>% group_by(sa_colony.x, timepoint.x) %>% get_summary_stats(ug_chla_cm, type = "mean_sd")
chla_as_time_summ #yes differences here too but not sure if the interaction matters or if time just matters more
#you can report these values in your slide along with stats (see below)

#run ANOVA to look at differences - @Ella we will do better stats later that are more complex and fit the data better!
#this is so you can report something for now for the surf :)
###SEASONAL STATS TEST
chla_time_sa_aov <- aov(ug_chla_cm ~ sa_colony.x*timepoint.x, data=sym_chla)
Anova(chla_time_sa_aov)
summary(chla_time_sa_aov)
#Ella report results from here to say what is significant and what is not!
#this will tell you the results for sym/apo, time, and the interactive effect
#say here what is significant or not and then pair that with your box plots!
#let's text about this tomorrow when you get here

#check qq plot - another way to look at normality
ggplot(sym_chla, aes(sample = ug_chla_cm) ) + geom_qq() + geom_qq_line() + facet_grid(sa_colony ~ .)
ggplot(sym_chla, aes(sample = ug_chla_cm) ) + geom_qq() + geom_qq_line() + facet_grid(. ~ timepoint)
ggplot(sym_chla, aes(sample = ug_chla_cm) ) + geom_qq() + geom_qq_line() + facet_grid(sa_colony ~ timepoint)
#nov 2023 apo looks veeery bad, others probs okay enough - this is just for me checking rn - will edit later
