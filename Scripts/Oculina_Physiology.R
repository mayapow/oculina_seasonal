
##### Coral Physiology Oculina Seasonal Project#####

#Maya Powell & Jamie Long & Ella Hennessey
#Last edited Oct 14 2026

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

#===Isotopes, 4 plots: Host C and N, Algae C and N====
#organizing data
# function to calculate standard error (SE)
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

# assign colors
depth_cols <- c(
  "Deep Aposymbiotic" = "cyan3",
  "Shallow Aposymbiotic" = "hotpink2",
  "Shallow Symbiotic" = "navy"
)

#===HOST d13C by depth/sym or apo PLOT====
host_d13C_summary <- host %>%
  #separates corals into groups
  group_by(depth_sa) %>%
  summarise(
    #count samples as valid measurements
    n = sum(!is.na(d13C_host)),
    #tells R to ignore missing values when calculating mean
    mean = mean(d13C_host, na.rm = TRUE),
    #calculate SE
    se = se_fun(d13C_host),
    .groups = "drop")
    # .groups = "drop" says we've finished grouping the data, return to normal dataframe

# creates plot points
host_d13C_plot <- ggplot() +
  geom_jitter(
    data = host,
    aes(x = depth_sa, y = d13C_host, color = depth_sa),
    width = 0.35,
    alpha = 0.65,
    size = 2.5
  ) +
  #adds errorbar
  geom_errorbar(
    data = host_d13C_summary,
    aes(x = depth_sa, ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 1
  ) +
  #adds average for each treatment, middle of errorbar
  geom_point(
    data = host_d13C_summary,
    aes(x = depth_sa, y = mean),
    size = 4,
    color = "black"
  ) +
  #assign treatment color
  scale_color_manual(values = depth_cols) +
  #shortens treatment names
  scale_x_discrete(labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")) +
  # sets y axis limits
  coord_cartesian(ylim = c(-24, -19), expand = FALSE, clip = "off") +
  # sets theme of graph & font size
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
  #labels y axis
  labs(y = expression(delta^{13}*C~("\u2030")))

host_d13C_plot

#===ALGAE d13C depth/sym or apo PLOT====
algae_d13C_summary <- algae %>%
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
    data = algae_d13C_summary,
    aes(x = depth_sa, ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = algae_d13C_summary,
    aes(x = depth_sa, y = mean),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")) +
  coord_cartesian(ylim = c(-24, -19), expand = FALSE, clip = "off") +
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

algae_d13C_plot

#===HOST d15N depth/sym or apo PLOT====
host_d15N_summary <- host %>%
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
    data = host_d15N_summary,
    aes(x = depth_sa, ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = host_d15N_summary,
    aes(x = depth_sa, y = mean),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")) +
  coord_cartesian(ylim = c(4, 12.5), expand = FALSE, clip = "off") +
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

#===ALGAE d15N depth/sym or apo PLOT====
algae_d15N_summary <- algae %>%
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
    data = algae_d15N_summary,
    aes(x = depth_sa, ymin = mean - se, ymax = mean + se),
    width = 0.18,
    linewidth = 1
  ) +
  geom_point(
    data = algae_d15N_summary,
    aes(x = depth_sa, y = mean),
    size = 4,
    color = "black"
  ) +
  scale_color_manual(values = depth_cols) +
  scale_x_discrete(labels = c("Deep Apo", "Shallow Apo", "Shallow Sym")) +
  coord_cartesian(ylim = c(4, 12.5), expand = FALSE, clip = "off") +
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


#===ANOVA + TUKEY significance letters====

#install.packages("multcompView")
#library(multcompView)

# Function to calculate significance letters
get_letters <- function(data, isotope_column) {
  
  # run ANOVA
  model <- aov(
    as.formula(paste(isotope_column, "~ depth_sa")),
    data = data
  )
  
  # Show ANOVA and Tukey results
  print(summary(model))
  print(TukeyHSD(model))
  
  # Convert Tukey comparisons into letters
  letters <- multcompLetters4(model, TukeyHSD(model))
  
  data.frame(
    depth_sa = names(letters$depth_sa$Letters),
    letter = letters$depth_sa$Letters
  )
}

# Calculate letters
host_d13C_letters <- get_letters(host, "d13C_host")

algae_d13C_letters <- get_letters(algae, "d13C_algae")

host_d15N_letters <- get_letters(host, "d15N_host")

algae_d15N_letters <- get_letters(algae, "d15N_algae")


#===add significance letters to plots====

# Function to position letters above each treatment
add_letters <- function(plot, data, summary_data,
                        isotope_column, letters_data) {
  
  positions <- data %>%
    group_by(depth_sa) %>%
    summarise(
      highest_point = max(.data[[isotope_column]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      summary_data %>%
        select(depth_sa, mean, se),
      by = "depth_sa"
    ) %>%
    mutate(
      top = pmax(highest_point, mean + se)
    ) %>%
    left_join(letters_data, by = "depth_sa")
  
  # Add a small vertical gap
  gap <- diff(range(data[[isotope_column]], na.rm = TRUE)) * 0.06
  
  positions <- positions %>%
    mutate(y_position = top + gap)
  
  plot +
    geom_text(
      data = positions,
      aes(x = depth_sa, y = y_position, label = letter),
      inherit.aes = FALSE,
      size = 7,
      fontface = "bold"
    )
}

# Host carbon
host_d13C_plot <- add_letters(
  host_d13C_plot, host, host_d13C_summary,
  "d13C_host", host_d13C_letters
)

# Algae carbon
algae_d13C_plot <- add_letters(
  algae_d13C_plot, algae, algae_d13C_summary,
  "d13C_algae", algae_d13C_letters
)

# Host nitrogen
host_d15N_plot <- add_letters(
  host_d15N_plot, host, host_d15N_summary,
  "d15N_host", host_d15N_letters
)

# Algae nitrogen
algae_d15N_plot <- add_letters(
  algae_d15N_plot, algae, algae_d15N_summary,
  "d15N_algae", algae_d15N_letters
)


#=== Combine into one four-panel!====
library(grid)

# Arrange plots
iso_plots <- ggarrange(host_d13C_plot, algae_d13C_plot, host_d15N_plot, algae_d15N_plot,
  nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))

# Add host/algae titles
final_plot <- annotate_figure(
  iso_plots,
  top = text_grob("Host                                  Algae",face = "bold", size = 18))

final_plot

# Save larger so points and labels are visible
ggsave(
  here("Output", "Nov_2024","isotopes","isotope_four_panel_plot.jpg"),
  final_plot,
  width = 18,
  height = 12,
  dpi = 600
)


#===Host 15N/13C isotope scatter plot====

#create scatterplot
host_CN_plot <- ggplot(host,
  aes(
    x = d13C_host,y = d15N_host,color = depth_sa)) +
  geom_point(
    size = 3,alpha = 0.8) +
  #assign colors defined earlier
  scale_color_manual(values = depth_cols) +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "right",
    #remove grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  #titles and axis
  labs(
    title = "Host",
    x = expression(delta^{13}*C~("\u2030")),
    y = expression(delta^{15}*N~("\u2030")),
    color = "Treatment")

#call plot and save
host_CN_plot
ggsave(
  filename = here("Output", "Nov_2024", "host_CN_scatterplot.pdf"),
  plot = host_CN_plot,
  width = 6,
  height = 6,
  dpi = 600)

#===Algae 15N/13C isotope scatter plot====
algae_CN_plot <- ggplot(
  algae,
  aes(x = d13C_algae,y = d15N_algae,color = depth_sa)) +
  geom_point(size = 3,alpha = 0.8) +
  
  scale_color_manual(values = depth_cols) +
  
  theme_bw(base_size = 22) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank() ) +
  
  labs(
    title = "Algae",
    x = expression(delta^{13}*C~("\u2030")),
    y = expression(delta^{15}*N~("\u2030")),
    color = "Treatment")

algae_CN_plot
ggsave(
  filename = here("Output", "Nov_2024", "algae_CN_scatterplot.pdf"),
  plot = algae_CN_plot,
  width = 6,
  height = 6,
  dpi = 600)

#===NOT USING overall host/algae ISOTOPE SIBER PLOTS====

library(ggplot2)
library(dplyr)

depth_cols <- c(
  "Deep Aposymbiotic" = "cyan3",
  "Shallow Aposymbiotic" = "hotpink2",
  "Shallow Symbiotic" = "navy"
)

# HOST ISOTOPE PLOT

host_siber_plot <- ggplot(
  host, aes(x = d13C_host,y = d15N_host,color = depth_sa)) +
  
  # plot individual samples
  geom_point(
    size = 3,
    alpha = 0.7) +
  
  # 95% ellipses
  stat_ellipse(
    aes(group = depth_sa,color = depth_sa ),
    type = "norm",level = 0.95,linewidth = 1.2) +
  
  # group colors
  scale_color_manual(values = depth_cols,
    name = NULL) +
  
  # axis labels
  labs(
    x = expression(delta^{13}*C~"(\u2030 VPDB)"),
    y = expression(delta^{15}*N~"(\u2030 Air)")) +
  
  # formatting
  theme_classic(base_size = 18) +
  
  theme(
    plot.title = element_text(
      size = 20,face = "bold",hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "right")

host_siber_plot

# ALGAE ISOTOPE PLOT

algae_siber_plot <- ggplot(
  algae,
  aes(x = d13C_algae,y = d15N_algae,color = depth_sa)) +
  
  # individual samples
  geom_point(size = 3,alpha = 0.7) +
  
  # 95% ellipses
  stat_ellipse(
    aes(group = depth_sa,color = depth_sa),
    type = "norm",level = 0.95,linewidth = 1.2) +
  
  # group colors
  scale_color_manual(values = depth_cols,
    name = NULL) +
  
  # axis labels
  labs(
    x = expression(delta^{13}*C~"(\u2030 VPDB)"),
    y = expression(delta^{15}*N~"(\u2030 Air)")) +
  
  # formatting
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(
      size = 20,face = "bold",hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "right")

algae_siber_plot

#===NOT USING combine host/algae siber plots====

#library(ggpubr)

combined_siber_plot <- ggarrange(
  host_siber_plot,
  algae_siber_plot, ncol = 2,nrow = 1,
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


#===SEPARATE HOST + ALGAE ISOTOPE PLOTS BY TREATMENT====

isotope_combined <- bind_rows(
  host %>%
    transmute(depth_sa,d13C = d13C_host,d15N = d15N_host,tissue = "Host"),
  algae %>%
    transmute(depth_sa,d13C = d13C_algae,d15N = d15N_algae,tissue = "Algae"))

# Colors for host and algae
tissue_cols <- c(
  "Host" = "navy",
  "Algae" = "hotpink2")


# Function to make the same plot for each treatment
make_isotope_plot <- function(group_name) {
  group_data <- isotope_combined %>%
    filter(depth_sa == group_name)
  
  ggplot(
    group_data,
    aes(x = d13C, y = d15N, color = tissue)
  ) +
    
    # Individual isotope measurements
    geom_point(
      aes(shape = tissue), size = 3.5,alpha = 0.75) +
    
    # Separate 95% ellipses for Host and Algae
    stat_ellipse(
      aes(group = tissue),type = "norm",level = 0.95,linewidth = 1.2) +
    
    # Colors and shapes
    scale_color_manual(values = tissue_cols) +
    scale_shape_manual(
      values = c("Host" = 16, "Algae" = 17)) +
    
    # Same axis limits for all three plots
    coord_cartesian(
      xlim = range(isotope_combined$d13C, na.rm = TRUE) + c(-0.5, 0.5),
      ylim = range(isotope_combined$d15N, na.rm = TRUE) + c(-0.5, 0.5)) +
    
    # Labels
    labs(
      title = group_name,
      x = expression(delta^{13}*C~("\u2030 VPDB")),
      y = expression(delta^{15}*N~("\u2030 Air")),
      color = "Tissue",
      shape = "Tissue") +
    
    # formatting
    theme_bw(base_size = 20) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 22,face = "bold",hjust = 0.5),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 14),
      legend.position = "right")}


#===CREATE THREE SEPARATE PLOTS====

deep_apo_CN_plot <- make_isotope_plot("Deep Aposymbiotic")

shallow_apo_CN_plot <- make_isotope_plot("Shallow Aposymbiotic")

shallow_sym_CN_plot <- make_isotope_plot("Shallow Symbiotic")

# display plots
deep_apo_CN_plot
shallow_apo_CN_plot
shallow_sym_CN_plot

#===save plots individually====

output_dir <- here("Output", "Nov_2024", "isotopes")

ggsave(
  filename = file.path(output_dir, "deep_apo_CN.jpg"),
  plot = deep_apo_CN_plot,
  width = 8,
  height = 7,
  dpi = 600,
)

ggsave(
  filename = file.path(output_dir, "shallow_apo_CN.jpg"),
  plot = shallow_apo_CN_plot,
  width = 8,
  height = 7,
  dpi = 600,
)

ggsave(
  filename = file.path(output_dir, "shallow_sym_CN.jpg"),
  plot = shallow_sym_CN_plot,
  width = 8,
  height = 7,
  dpi = 600,
)

#===save plots as one figure====
# want one axis: panels B and C -- removing y title & tick labels
shallow_apo_CN_plot <- shallow_apo_CN_plot +
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

shallow_sym_CN_plot <- shallow_sym_CN_plot +
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

# All panels, removing the individual x titles
deep_apo_CN_plot    <- deep_apo_CN_plot    + theme(axis.title.x = element_blank())
shallow_apo_CN_plot <- shallow_apo_CN_plot + theme(axis.title.x = element_blank())
shallow_sym_CN_plot <- shallow_sym_CN_plot + theme(axis.title.x = element_blank())

# put together
three_panel_CN <- ggarrange(
  deep_apo_CN_plot, shallow_apo_CN_plot, shallow_sym_CN_plot,
  ncol = 3, nrow = 1,
  labels = c("A", "B", "C"),
  common.legend = TRUE,
  legend = "bottom",
  align = "hv"
)

# One shared x label
three_panel_CN <- annotate_figure(
  three_panel_CN,
  bottom = text_grob(expression(delta^{13}*C~("\u2030 VPDB")), size = 20)
)

three_panel_CN

#save
ggsave(
  here("Output", "Nov_2024", "isotopes", "host_algae_CN_by_treatment.jpg"),
  three_panel_CN, width = 18, height = 6.5,dpi = 300
)



#===SIGNIFICANCE TESTS, for siber, host vs algae by treatment====

#===function for SEAc of 95%=====
seac95 <- function(x, y, level = 0.95) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  n <- length(x)
  pi * qchisq(level, 2) * sqrt(det(cov(cbind(x, y)))) * (n - 1) / (n - 2)
}

seac_tab <- isotope_combined %>%
  group_by(tissue, depth_sa) %>%
  summarise(n = sum(complete.cases(d13C, d15N)),
            SEAc_95 = seac95(d13C, d15N),
            .groups = "drop")
seac_tab
#===calculate ellipse overlap====

#library(SIBER)

siber_data <- bind_rows(
  host  %>% transmute(iso1 = d13C_host,  iso2 = d15N_host,
                      group = as.character(depth_sa), community = "Host"),
  algae %>% transmute(iso1 = d13C_algae, iso2 = d15N_algae,
                      group = as.character(depth_sa), community = "Algae")) %>%
  filter(!is.na(iso1), !is.na(iso2)) %>%
  as.data.frame()

siber_obj <- createSiberObject(siber_data)

#means "compare these ellipses"
maxLikOverlap("Host.Deep Aposymbiotic", "Algae.Deep Aposymbiotic",
              siber_obj, p.interval = 0.95, n = 100)

#treatments
trts <- c("Deep Aposymbiotic", "Shallow Aposymbiotic", "Shallow Symbiotic")

#lapply() runs the function once for each treatment
overlap_tab <- lapply(trts, function(trt) {
  o <- maxLikOverlap(paste0("Host.", trt), paste0("Algae.", trt),
                     siber_obj, p.interval = 0.95, n = 100)
  #building new data frame per treatment
  data.frame(treatment = trt,
             overlap = o[["overlap"]],
             host_area = o[["area.1"]],
             algae_area = o[["area.2"]]) 
}) %>%
  bind_rows() %>%
  #adding percentage columns
  mutate(pct_of_host  = 100 * overlap / host_area,
         pct_of_algae = 100 * overlap / algae_area,
         pct_of_union = 100 * overlap / (host_area + algae_area - overlap))

overlap_tab

#===DEEP APOSYMBIOTIC t test====
# stitches host and algae tables together, matching rows by frag_id and depth_sa
paired_isotopes <- inner_join(host, algae, by = c("frag_id", "depth_sa"))
deep_apo <- paired_isotopes %>%
  #keeps rows where depth_sa equals Deep Apo
  filter(depth_sa == "Deep Aposymbiotic")

# Carbon
t.test(
  # means 'the column d13C_host from object deep_apo'
  deep_apo$d13C_host,
  deep_apo$d13C_algae,
  #takes host and algae value from same row bc they belong to same fragment
  paired = TRUE
)

# Nitrogen
t.test(
  deep_apo$d15N_host,
  deep_apo$d15N_algae,
  paired = TRUE
)

#===SHALLOW APOSYMBIOTIC t test=====

shallow_apo <- paired_isotopes %>%
  filter(depth_sa == "Shallow Aposymbiotic")

# Carbon
t.test(
  shallow_apo$d13C_host,
  shallow_apo$d13C_algae,
  paired = TRUE
)

# Nitrogen
t.test(
  shallow_apo$d15N_host,
  shallow_apo$d15N_algae,
  paired = TRUE
)

#===SHALLOW SYMBIOTIC t test====

shallow_sym <- paired_isotopes %>%
  filter(depth_sa == "Shallow Symbiotic")

# Carbon
t.test(
  shallow_sym$d13C_host,
  shallow_sym$d13C_algae,
  paired = TRUE
)

# Nitrogen
t.test(
  shallow_sym$d15N_host,
  shallow_sym$d15N_algae,
  paired = TRUE
)


#======Ash Free Dry Weight ========

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

sym_chla <- read.csv(here("Data", "Nov_2024", "oculina_nov24_chla.csv"))
meta <- read.csv(here("Data", "Nov_2024", "oculina_nov24_metadata.csv"))

#remove samples that have no data
sym_chla <- sym_chla %>% 
  left_join(meta, by = "sample_ID")
sym_chla <- sym_chla %>% filter(full_ID != "A12-1124") 

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

#========
#3 panel plot w sym density, chla/cm2, chla/symbiont

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
#============

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
