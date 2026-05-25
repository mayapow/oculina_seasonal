#Maya Powell
#coral isotope SIBER analysis

#install.packages("SIBER")
library(SIBER)
library(tidyverse)
library(rjags)
library(ggplot2)
library(here)
library(lubridate)
#help(SIBER)

#nov 2024 data

iso_nov24<-read.csv(here("Data/Nov_2024/oculina_nov2024_isotopes.csv"))
iso_nov24 <- iso_nov24 %>%
  filter(sample_id !="A5-A-1124-Algae", sample_id !="S11-S-new-1124-Algae") %>% #remove sample IDs that are not paired algae & host
  filter(algae_host != "Filter") #remove filter samples for now

iso_nov24_host <- read.csv(here("Data/Nov_2024/oculina_nov2024_isotopes_host.csv")) 
iso_nov24_host <- iso_nov24_host %>%
  select(-sample_id,-algae_host,-weight_mg,-total_C_ug,-pct_C,-pct_N,-total_N_ug)
iso_nov24_algae<-read.csv(here("Data/Nov_2024/oculina_nov2024_isotopes_algae.csv"))
iso_nov24_algae <- iso_nov24_algae %>%
  select(-sample_id,-algae_host,-weight_mg,-total_C_ug,-pct_C,-pct_N,-total_N_ug)
iso_nov24_holo <- iso_nov24_algae %>%
  left_join(iso_nov24_host) %>%
  mutate(d15N_holo = d15N_host - d15N_algae,
         d13C_holo = d13C_host - d13C_algae)

iso_d13C_depth_sa <- ggplot(iso_nov24, aes(x = algae_host, y=d13C, fill = algae_host))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Timepoint", y = expression(paste(delta^{13}, "C (‰, V-PDB)")))+
  theme(legend.position = "none") +
  facet_wrap(~depth_sa)
iso_d13C_depth_sa

iso_d15N_depth_sa <- ggplot(iso_nov24, aes(x = algae_host, y=d15N, fill = algae_host))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Timepoint", y = expression(paste(delta^{15}, "N (‰, air)")))+
  theme(legend.position = "none")+
  facet_wrap(~depth_sa)
iso_d15N_depth_sa

#anova c13
mod_d13C <- aov(d13C ~ depth_sa*algae_host, data = iso_nov24)
summary(mod_d13C)
Anova(mod_d13C)

#pairwise c13
emm_d13C <- emmeans::emmeans(mod_d13C, ~depth_sa*algae_host)
pairs(emm_d13C)

#depth_sa*algae_host
# Deep Aposymbiotic Algae - Deep Aposymbiotic Host        -0.7747 0.1330 142  -5.819 <0.0001
# Deep Aposymbiotic Algae - Shallow Aposymbiotic Host     -1.0073 0.1150 142  -8.738 <0.0001
# Deep Aposymbiotic Algae - Shallow Symbiotic Host        -0.9570 0.1150 142  -8.301 <0.0001
# Shallow Aposymbiotic Algae - Deep Aposymbiotic Host     -0.6799 0.1160 142  -5.864 <0.0001
# Shallow Aposymbiotic Algae - Shallow Aposymbiotic Host  -0.9126 0.0949 142  -9.613 <0.0001
# Shallow Aposymbiotic Algae - Shallow Symbiotic Host     -0.8623 0.0949 142  -9.082 <0.0001
# Shallow Symbiotic Algae - Deep Aposymbiotic Host        -0.6027 0.1160 142  -5.198 <0.0001
# Shallow Symbiotic Algae - Shallow Aposymbiotic Host     -0.8354 0.0949 142  -8.799 <0.0001
# Shallow Symbiotic Algae - Shallow Symbiotic Host        -0.7850 0.0949 142  -8.269 <0.0001

#anova n15
mod_d15N <- aov(d15N ~ depth_sa, data = iso_nov24)
summary(mod_d15N)
Anova(mod_d15N)

#pairwise n15
emm_d15N <- emmeans::emmeans(mod_d15N, ~depth_sa)
pairs(emm_d15N)
#depth_sa*algae_host
#Deep Aposymbiotic Algae - Shallow Symbiotic Algae        1.7769 0.257 142   6.919 <0.0001
#Shallow Aposymbiotic Algae - Shallow Symbiotic Algae     1.2817 0.212 142   6.044 <0.0001
#Shallow Symbiotic Algae - Deep Aposymbiotic Host        -1.6549 0.257 142  -6.444 <0.0001
#Shallow Symbiotic Algae - Shallow Aposymbiotic Host     -1.2409 0.210 142  -5.901 <0.0001
#Shallow Symbiotic Algae - Shallow Symbiotic Host        -1.0946 0.210 142  -5.205 <0.0001

#depth_sa - N15 signatures are different regardless of host/algae
# Deep Aposymbiotic - Shallow Aposymbiotic    0.455 0.196 145   2.325  0.0555
# Deep Aposymbiotic - Shallow Symbiotic       1.159 0.196 145   5.925 <0.0001
# Shallow Aposymbiotic - Shallow Symbiotic    0.704 0.161 145   4.385 <0.0001

iso_d13C_d15N <- ggplot(iso_nov24, aes(x = d13C, y=d15N, color = algae_host))+
  geom_point() +
  theme_gray(base_size = 22)+
  labs(x = expression(paste(delta^{13}, "C (‰, V-PDB)")), y = expression(paste(delta^{15}, "N (‰, air)")))
  #facet_wrap(~depth_sa)
iso_d13C_d15N


#Read in and clean up data
iso<-read.csv(here("Data/Seasonal/isotopes_all_oculina_seasonal.csv"))
iso <- iso %>%
  mutate(date = mdy(timepoint)) %>%
  filter(algae_host != "Filter") %>% #remove filter samples for now
  filter(date != "2023-07-01") #remove july 2023 data bc only a few points that troye tested, not useful
  

iso_d13C_time <- ggplot(iso, aes(x = timepoint, y=d13C, fill = timepoint))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Timepoint", y = expression(paste(delta^{13}, "C (‰, V-PDB)")))+
  theme(legend.position = "none") +
  facet_wrap(~algae_host)
iso_d13C_time

iso_d15N_time <- ggplot(iso, aes(x = timepoint, y=d15N, fill = timepoint))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Timepoint", y = expression(paste(delta^{15}, "N (‰, air)")))+
  theme(legend.position = "none")+
  facet_wrap(~algae_host)
iso_d15N_time

iso_d13C_d15N_time <- ggplot(iso, aes(x = d13C, y=d15N, color = algae_host))+
  geom_point() +
  theme_gray(base_size = 22)+
  labs(x = expression(paste(delta^{13}, "C (‰, V-PDB)")), y = expression(paste(delta^{15}, "N (‰, air)")))+
  facet_wrap(~timepoint, nrow = 1, ncol = 5)
iso_d13C_d15N_time

###Load in all data

#iso_v2 <- iso[grep("S", iso$Sample_ID), ] #only sym
# create columns for "group" and "community" (other columns required for createSiberObject)
iso <- iso %>%
  mutate(group = case_when(
    algae_host == "Algae" ~ "1", #algae
    algae_host == "Host" ~ "2", #coral host
    TRUE ~ NA_character_
  ))

iso$group <- iso$algae_host
iso$community <- iso$a_s

iso <- iso %>%
  mutate(community = case_when(
    a_s_colony == "Symbiotic" ~ "1",
    a_s_colony == "Aposymbiotic" ~ "2",
    TRUE ~ NA_character_
  ))

iso <- iso %>%
  mutate(tp = case_when(
    grepl("2024-11-01", date) ~ "4",
    grepl("2024-05-01", date) ~ "3",
    grepl("2023-11-01", date) ~ "2",
    grepl("2023-09-01", date) ~ "1",
    TRUE ~ NA_character_
  ))

#separate july and september and november dataframes
#iso_jul23 <- subset(iso, timepoint == "1") #4 data points
iso_sept23 <- subset(iso, date == "2023-09-01") #32 data points
iso_nov23 <- subset(iso, date == "2023-11-01") #12 data points
iso_may24 <- subset(iso, date == "2024-05-01") #36 data points
iso_nov24 <- subset(iso, date == "2024-11-01") #58 data points

# take a subset of the data so that it only includes the columns required for createSiberObject
#subset_isov_jul23 <- subset(iso_jul23, select = c("δ13C", "δ15N", "group", "community"))
subset_isov_sept23 <- subset(iso_sept23, select = c("d13C", "d15N", "algae_host", "a_s_colony"))
subset_isov_nov23 <- subset(iso_nov23, select = c("d13C", "d15N", "algae_host", "a_s_colony"))
subset_isov_may24 <- subset(iso_may24, select = c("d13C", "d15N", "algae_host", "a_s_colony"))
subset_isov_nov24<- subset(iso_nov24, select = c("d13C", "d15N", "algae_host", "a_s_colony"))
#subset_isov_nov23 <- subset(iso_nov23, select = c("δ13CVPDB (‰)", "δ15NAir (‰)", "group", "community"))
#time is a dataset with group as algae vs host and community as time instead of apo/sym
#subset_isov_time <- subset(iso_time, select = c("δ13C", "δ15N", "algae_host", "timepoint"))

# rename the columns so they match the example data exactly or else you cant create a siberobject
#colnames(subset_isov_jul23) <- c("iso1", "iso2", "group", "community")
colnames(subset_isov_sept23) <- c("iso1", "iso2", "group", "community")
colnames(subset_isov_nov23) <- c("iso1", "iso2", "group", "community")
colnames(subset_isov_may24) <- c("iso1", "iso2", "group", "community")
colnames(subset_isov_nov24) <- c("iso1", "iso2", "group", "community")

#for jamie - for sept make 2 separate graphs with just community 1 = sym & community 2 = apo
subset_isov_sept23_S <- subset(subset_isov_sept23, community == "1")
subset_isov_sept23_alg <- subset(subset_isov_sept23, group == "1")
subset_isov_sept23_A <- subset(subset_isov_sept23, community == "2")
#and for time - sept grouped vs nov grouped nov = 3, sept = 2
subset_isov_time_sept <- subset(subset_isov_time, community == "2")
subset_isov_time_nov <- subset(subset_isov_time, community == "3")

# create Siber object
#siberobj_jul23 <- createSiberObject(subset_isov_jul23) 
#warning message for july because it has less than 5 observations (4) - can't analyze this as its own
siberobj_sept23 <- createSiberObject(subset_isov_sept23)
#now sym & apo for sept
siberobj_sept23_S <- createSiberObject(subset_isov_sept23_S)
siberobj_sept23_A <- createSiberObject(subset_isov_sept23_A)
siberobj_sept23_alg <- createSiberObject(subset_isov_sept23_alg)

siberobj_sept23 <- createSiberObject(subset_isov_sept23)
siberobj_nov23 <- createSiberObject(subset_isov_nov23) #warning less than 5
siberobj_may24 <- createSiberObject(subset_isov_may24)
siberobj_nov24 <- createSiberObject(subset_isov_nov24)

#warning message because less than 5 observations
siberobj_time <- createSiberObject(subset_isov_time) 
#now sept & nov all samples apo & sym together
siberobj_time_sept <- createSiberObject(subset_isov_time_sept)
siberobj_time_nov <- createSiberObject(subset_isov_time_nov)

#july algae 1 & host 3 samples
#sept algae 16 & host 16 samples
#nov algae 7 & host 5 samples
#excluding july in time samples then bc they are too few!
siberobj_sept23[["sample.sizes"]]
#for nov data - not enough apo & sym comparisons to do it so have to not look at that either
#so the 2 comparisons that we can do rn are:
#1. Just Sept data, 2. comparison of Sept vs Nov data

# plot
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hulls.args     <- list(lty = 2, col = "grey20")

par(mfrow=c(1,1))

#siberobj_sept23
#group 1 is algae & group 2 is coral host layer
#community 1 is apo community 2 is sym
plotSiberObject(siberobj_sept23,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                y.limits = c(0,10),
                x.limits = c(-24,-18)
)
#nov 2023
plotSiberObject(siberobj_nov23,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                #y.limits = c(0,10),
                #x.limits = c(-24,-18)
)

#may 2024
plotSiberObject(siberobj_may24,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                #y.limits = c(0,10),
                #x.limits = c(-24,-18)
)
#nov 2024
plotSiberObject(siberobj_nov24,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                #y.limits = c(0,10),
                #x.limits = c(-24,-18)
)


#september sym
plotSiberObject(siberobj_sept23_alg,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                y.limits = c(0,10),
                x.limits = c(-24,-18)
)

#september apo
plotSiberObject(siberobj_sept23_A,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                y.limits = c(0,10),
                x.limits = c(-24,-18)
)


#siberobj_time
plotSiberObject(siberobj_time,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                #y.limits = c(0,15),
                #x.limits = c(-24,-18)
)

#time split by sept and nov
#all apo & sym grouped together
sept_plot <- plotSiberObject(siberobj_time_sept,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                y.limits = c(0,15),
                x.limits = c(-24,-18)
                )

plotSiberObject(siberobj_time_nov,
                ax.pad = 2, 
                hulls = F, community.hulls.args = community.hulls.args, 
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = T, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C),
                ylab = expression({delta}^15*N),
                cex = 0.5,
                y.limits = c(0,15),
                x.limits = c(-24,-18)
)

# group (colors) = algae (1) vs host (2)
# community (shapes) = apo (1) vs sym (2)

##making nicer plots with colors etc using this code:

sept23_as <- ggplot(data = subset_isov_sept23, 
                     aes(x = iso1, 
                         y = iso2)) + 
  geom_point(aes(color = community, shape = group), size = 5) +
  ylab(expression(paste(delta^{15}, "N"))) +
  xlab(expression(paste(delta^{13}, "C"))) + 
  theme(text = element_text(size=16)) + 
  theme_classic(base_size = 22) +
  ylim(0,15)+
  xlim(-25,-18)+
  scale_color_manual(name = NULL,values = c("#222255","#77aadd"))+
  #scale_shape_manual(name = NULL,values = c(17,16), labels = c("Algae", "Host"))+
  stat_ellipse(aes(group = interaction(group, community), 
                   color = community), 
               alpha = 0, 
               level = 0.95,
               type = "norm",
               geom = "polygon")
sept23_as
#jamie colors:
"#bf2da9" = apo, "#4eb5e9" = sym
#blues
"#222255","#77aadd"

ggsave(sept23_as, file = "sept23_as_isotopes.pdf", w = 8, h = 6)


#may 2024

may24_as <- ggplot(data = subset_isov_may24, 
                    aes(x = iso1, 
                        y = iso2)) + 
  geom_point(aes(color = community, shape = group), size = 5) +
  ylab(expression(paste(delta^{15}, "N"))) +
  xlab(expression(paste(delta^{13}, "C"))) + 
  theme(text = element_text(size=16)) + 
  theme_classic(base_size = 22) +
  #ylim(0,15)+
  #xlim(-25,-18)+
  scale_color_manual(name = NULL,values = c("#77aadd","#222255"))+
  #scale_shape_manual(name = NULL,values = c(17,16), labels = c("Algae", "Host"))+
  stat_ellipse(aes(group = interaction(group, community), 
                   color = community), 
               alpha = 0, 
               level = 0.95,
               type = "norm",
               geom = "polygon")
may24_as

ggsave(may24_as, file = "may24_as_isotopes.pdf", w = 8, h = 6)


#time
time_plot <- ggplot(data = subset_isov_time, 
                    aes(x = iso1, 
                        y = iso2)) + 
  geom_point(aes(color = community, shape = group), size = 5) +
  ylab(expression(paste(delta^{15}, "N"))) +
  xlab(expression(paste(delta^{13}, "C"))) + 
  theme(text = element_text(size=16)) + 
  theme_classic(base_size = 22) +
  #ylim(0,15)+
  #xlim(-25,-18)+
  #scale_color_manual(name = NULL,values = c("#222255","#77aadd"))+
  #scale_shape_manual(name = NULL,values = c(17,16), labels = c("Algae", "Host"))+
  stat_ellipse(aes(group = interaction(group, community), 
                   color = community), 
               alpha = 0, 
               level = 0.95,
               type = "norm",
               geom = "polygon")
time_plot

ggsave(time_plot, file = "timepoint_isotopes.pdf", w = 8, h = 6)

## summary stats
par(mfrow=c(1,1))

community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

# Calculate summary statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siberobj_may24)
print(group.ML)

#Nov 2023 - 3 = nov, .1 = algae, .2 = host
#           3.2      3.1
# TA   0.934500 7.040400
# SEA  0.940002 5.206962
# SEAc 1.253336 6.248355

#Sept 2023 - 2 = sept, .1 = algae, .2 = host
#           2.1       2.2
# TA   8.462200 1.5531500
# SEA  3.494395 0.5748374
# SEAc 3.743994 0.6158972

#Sept 2023 apo= 1, sym = 2, .1 = algae, .2 = host
#            1.2      1.1      2.1       2.2
# TA   1.0715000 2.475650 2.460300 0.1892000
# SEA  0.4296941 1.551834 1.968449 0.1481441
# SEAc 0.4834059 1.745814 2.460561 0.1851801

#May 2024
#      Aposymbiotic.Host Aposymbiotic.Algae Symbiotic.Host Symbiotic.Algae
# TA           1.2240500          10.525250      0.7103500       19.434350
# SEA          0.9056866           7.524421      0.3975202        9.146843
# SEAc         1.0868239           9.029306      0.4416891       10.163159

# You can add more ellipses by directly calling plot.group.ellipses()
# Add an additional p.interval % prediction ellilpse
plotGroupEllipses(siberobj_sept23, n = 100, p.interval = 0.95,
                  lty = 1, lwd = 2)

# or you can add the XX% confidence interval around the bivariate means
# by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siberobj_sept23, n = 100, p.interval = 0.95, ci.mean = T,
                  lty = 2, lwd = 2)
# dashed lines are confidence interval means

## Ellipses overlap
# The first ellipse is referenced using a character string representation where 
# in "x.y", "x" is the community, and "y" is the group within that community.
# So in this example: community 1, group 1
#for sept & may comparisons between apo & sym -
ellipse1 <- "Symbiotic.Algae" 

# Ellipse two is similarly defined: community 1, group2
ellipse2 <- "Symbiotic.Host"

# The overlap of the maximum likelihood fitted standard ellipses are 
# estimated using
sea.overlap <- maxLikOverlap(ellipse1, ellipse2, siberobj_may24, 
                             p.interval = NULL, n = 100)
sea.overlap

# the overlap between the corresponding 95% prediction ellipses is given by:
# apo host vs apo algae
overlap95 <- maxLikOverlap(ellipse1, ellipse2, siberobj_may24, 
                           p.interval = 0.95, n = 100)
overlap95

# so in this case, the overlap as a proportion of the non-overlapping area of 
# the two ellipses, would be
# apo agl vs apo chl overlap 
prop.95.over <- overlap95[3] / (overlap95[2] + overlap95[1] - overlap95[3])

prop.95.over

#overlaps
#timepoint comparison
#Nov 2023 = 0.187004 
#Sept 2023 = 0.1325736 
#within timepoint, apo vs sym comparison
#Sept 2023 sym = 0.2138308  
#Sept 2023 apo = 0.0752593 
#May 2024 sym = 3.688232e-19 #basically zero
#May 2024 apo = 2.016263e-17 # basically zero

# # Example from SIBER package
# generateSiberData()
# #createSiberObject()
# data("demo.siber.data")
# 
# siber.example <- createSiberObject(demo.siber.data)
# 
# community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
# group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
# group.hulls.args     <- list(lty = 2, col = "grey20")
# 
# par(mfrow=c(1,1))
# plotSiberObject(siber.example,
#                 ax.pad = 2, 
#                 hulls = F, community.hulls.args = community.hulls.args, 
#                 ellipses = T, group.ellipses.args = group.ellipses.args,
#                 group.hulls = T, group.hulls.args = group.hulls.args,
#                 bty = "L",
#                 iso.order = c(1,2),
#                 xlab = expression({delta}^13*C),
#                 ylab = expression({delta}^15*N)
# )

