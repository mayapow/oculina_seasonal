#Oculina seasonal project symbiont density & chlorophyll a
#Authors: Maya Powell, Ella Hennessey
#Last updated: November 2025

#Maya Powell & Jamie Long & Ella Hennessey
#Last edited November 28th 2025

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

###READ IN AND CLEAN DATA####

#read in data
#metadata
phys_meta <- read.csv(here("Data", "Nov_2024","oculina_nov24_metadata.csv"))
#sym density
sym <- read.csv(here("Data", "Nov_2024", "oculina_nov24_sym_density.csv"))
#merge sym density with metadata
sym <- sym %>% left_join(phys_meta, by = "sample_ID")
#read in chla data
sym_chla <- read.csv(here("Data", "Nov_2024", "oculina_nov24_chla.csv"))
#merge with chla data as well
sym_chla <- sym_chla %>%
  left_join(sym, by = "sample_ID")

#remove samples that have no data, weird Chla ones tha have only Ro
sym_chla <- sym_chla %>% filter(sample_ID != "A12-S" & sample_ID != "A12-A") 
#here A12 from Nov 24 has no surface area yet so we can't use it - removed both A and S

#calculate symbiont density

#the 1x1mm grid should hold 0.1uL
#we want cells/mL so 10,000 x our counts (1000uL in a mL/0.1uL = x 10,000)
#resuspension volume column to normalize to volume
#then use surface area to normalize

sym_chla <- sym_chla %>%
  rowwise() %>% #first average counts across your count rows
  mutate(avg_count = mean(c_across(c(count1, count2, count3, count4, count5)), na.rm = TRUE)) %>%
  mutate(sym_mL = avg_count*10000) %>% #this gives us cells per mL based on the volume counted
  mutate(sym_total = sym_mL*resuspension_vol) %>% #total symbionts using our resuspension volume
  mutate(sym_cm2 = sym_total/sa_cm2) #and normalized to surface area

#CHECK YOUR COUNTS - SYM SHOULD BE BETWEEN 2x10^6 to 6x10^6 - apo will be much lower
#@Maya HOW DO I CHECK THE COUNTS? this returns 'NA' for me
sym_chla %>%
group_by(sa_frag) %>%
mean(sym_cm2)

#calculate chla per symbiont
#this tells us about the photosynthetic capacity of EACH symbiont
#so e.g. if deep corals only had a few symbionts but they were SO CRAZY FULL OF chlorophyll a they could be fine
#convert from ug chla to pg chla
sym_chla <- sym_chla %>%
  mutate(chla_pg_sym = (ug_chla_total/sym_total)*1000000)
#makes physiology folder
dir.create(here("Data", "Physiology"))
write.csv(sym, here("Data", "Physiology", "oculina_nov24_physio.csv"), row.names = FALSE)

#you can check the classes of the variables using this as needed:
sapply(sym_chla, class)

#convert any variables to correct classes as needed - see example here:
sym_chla <- sym_chla %>% mutate_at(c('ug_chla_cm'), as.numeric)

#info about variables - add notes that are helpful to you!
#sa_colony = aposymbiotic/symbiotic original designation of the colony
#ug_chla_cm = micrograms of chlorophyll a per cm2 of surface area
#sym_cm2 = symbiont density for cm2 of surface area
#chla_pg_sym = picograms of chlorophyll a per symbiont

###PLOTS###
#ELLA - EDIT THIS TO JUST LOOK AT SAMPLES SPLIT AND COLORED BY APO SYM OF FRAG
#TAKE OUT ANY COMPARISONS THAT USE THE APO/SYM OF COLONY!!
#ALSO THEN PLEASE PLOT CHLA, SYM DENSITY, AND CHLA PER SYMBIONT

#dot plot of chlorophyll a looking at all samples split and colored by apo_sym of frag instead of colony
chla_nov_sa_point <- ggplot(sym_chla, aes(x = sample_ID, y=ug_chla_cm, color = sa_frag))+
  geom_point(alpha=0.8, size = 3)+
  theme_classic(base_size = 22)+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(~sa_colony, scales = "free")
chla_nov_sa_point

#SAMPLES SPLIT BY A/S FRAG
chla_nov_sa_frag_point <- ggplot(sym_chla, aes(x = sample_ID, y = ug_chla_cm, color = sa_frag)) +
  geom_point(alpha = 0.8, size = 3) +
  theme_classic(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Sample ID",
    y = expression(paste("Chlorophyll a (", mu, "g cm"^-2, ")")),
    color = "Fragment symbiotic state"
  )
chla_nov_sa_frag_point

## CHL A PER SYMBIONT ##
#POINT#
chla_sym_nov_sa_point <- ggplot(sym_chla, aes(x = sample_ID, y = chla_pg_sym, color = sa_frag)) +
  geom_point(alpha = 0.8, size = 3) +
  theme_classic(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Sample ID",
    y = "Chlorophyll a (pg/symbiont)",
    color = "Fragment symbiotic state"
  )
chla_sym_nov_sa_point
#ggsave(chla_sym_nov_sa_point, file = "chla_sym_nov_sa_point.pdf", h = 10, w = 10)


# Chl a per symbiont by sa_frag BOX
chla_sym_nov_sa_box <- ggplot(sym_chla, aes(x = sa_frag, y = chla_pg_sym, fill = sa_frag)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.8, width = 0.2) +
  theme_classic(base_size = 22) +
  labs(
    x = "Fragment symbiotic state",
    y = "Chlorophyll a (pg per symbiont)"
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("lightyellow", "brown"))
chla_sym_nov_sa_box
#ggsave(chla_sym_nov_sa_box, file = "chla_sym_nov_sa_box.pdf", h = 10, w = 10)

##Sym density##
sym_nov_sa_point <- ggplot(sym_chla, aes(x = sample_ID, y = sym_cm2, color = sa_frag)) +
  geom_point(alpha = 0.8, size = 3) +
  theme_classic(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Sample ID",
    y = expression(paste("Symbiont density (cells cm"^-2, ")")),
    color = "Fragment symbiotic state"
  )
sym_nov_sa_point
#ggsave(sym_nov_sa_point, file = "sym_nov_sa_point.pdf", h = 10, w = 10)

#BOX
sym_nov_sa_box <- ggplot(sym_chla, aes(x = sa_frag, y = sym_cm2, fill = sa_frag)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.8, width = 0.2) +
  theme_classic(base_size = 22) +
  labs(
    x = "Fragment symbiotic state",
    y = expression(paste("Symbiont density (cells cm"^-2, ")"))
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("lightyellow", "brown"))
sym_nov_sa_box
#ggsave(sym_nov_sa_box, file = "sym_nov_sa_box.pdf", h = 10, w = 10)


#@Ella - here's how to save a plot - it will save in your working directory folder
#go through and save the rest of them by editing this line of code for each plot!
ggsave(chla_nov_sa_point, file = "chla_nov_sa_point.jpg", h=10, w=10) #change height and width and resave as needed

#boxplot of chla grouped by sym/apo
#this is interesting but u can see the original designation of the colonies as apo/sym doesn't seem to hold - look at it over time too
#@MAYA i don't know how to look at it over time
chla_nov_sa_box <- ggplot(sym_chla, aes(x = sa_frag, y=ug_chla_cm, fill = sa_frag))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Symbiotic state - within colony", y = "ug chla per cm2")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("lightyellow","brown")) #ella change the colors here as you'd like
chla_nov_sa_box
ggsave(chla_nov_sa_box, file = "chla_nov_sa_box.pdf", h=10, w=10) #change height and width and resave as needed
#this is interesting and you can show it next to the other plot to show
#difs in season symbiotic state (other plot) as well as within-colony symbiotic state (this plot)
#this also reinforces the seasonal aspect as more important

#boxplot of chla grouped by sym/apo of frag and facet by sym/apo of colony
#here the colony designation doesn't seem to matter at all!
#this makes sense given your data from the other analysis that shows that things look more similar within timepoint than between timepoint
chla_nov_sa_sa_box <- ggplot(sym_chla, aes(x = sa_frag, y=ug_chla_cm, fill = sa_frag))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  facet_wrap(~sa_colony)+
  labs(x = "Symbiotic state", y = "ug chla per cm2")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("tan","brown")) #ella change the colors here as you'd like
chla_nov_sa_sa_box

####STATS####

###ELLA EDIT THIS TO EXAMINE THE DIFFERENCES OF CHLA AND SYM DENSITY USING ANOVAS
#HERE YOU HAVE AN EXAMPLE OF CHLA - ADD FOR SYM DENSITY AND CHLA/SYMBIONT AS WELL!!
#THESE ARE JUST T-TESTS HERE BECAUSE THEY ARE JUST COMPARING BETWEEN ONE VARIABLE: SYM vs APO


#sometimes you need to edit variables to make them be factors for the stats to work - see example here:
#color_avg_nox$month <- as.factor(color_avg_nox$month)

#look to see if means of different groups are the same/different
#chla_cm
chla_as_frag_summ <- sym_chla %>% group_by(sa_frag) %>% get_summary_stats(ug_chla_cm, type = "mean_sd")
chla_as_frag_summ #ya difference but still fairly small
#you can report these values in your slide along with stats (see below)

#run ANOVA to look at differences
chla_sa_aov <- aov(ug_chla_cm ~ sa_colony*sa_frag, data=sym_chla)
Anova(chla_sa_aov)

# 1) Chl a 
chla_frag_aov <- aov(ug_chla_cm ~ sa_frag, data = sym_chla)
summary(chla_frag_aov)

# Df Sum Sq Mean Sq F value   Pr(>F)    
#sa_frag      1  27.00  27.001   46.82 2.28e-09 ***
#Residuals   71  40.95   0.577      

# 2) Symbiont density
sym_frag_aov <- aov(sym_cm2 ~ sa_frag, data = sym_chla)
summary(sym_frag_aov)

#Df    Sum Sq   Mean Sq F value   Pr(>F)    
#sa_frag      1 3.726e+12 3.726e+12   12.54 0.000707 ***
#Residuals   71 2.109e+13 2.970e+11                


# 3) Chl a per symbiont
chla_sym_frag_aov <- aov(chla_pg_sym ~ sa_frag, data = sym_chla)
summary(chla_sym_frag_aov)

#Df Sum Sq Mean Sq F value   Pr(>F)    
#sa_frag      1   74.8   74.80   16.68 0.000115 ***
#Residuals   71  318.3    4.48               

#Ella report results from here to say what is significant and what is not!
#this will tell you the results for sym/apo, time, and the interactive effect
#say here what is significant or not and then pair that with your box plots!
#let's text about this tomorrow when you get here

#SIGNIFICANCE YAY
#Maya here we go
#Sym frags have significantly higher chlorophyll a per cm2 than apo frags(p = 2.28e-09 )
#Sym frags have higher sym densities, apo frags have very low densities(p = 0.000707)
#S/A frags differ in how much chl a each symbiont contains? (p=0.00015)
#maybe bc apo frags contain fewer sym, sym are stressed → fewer Chl a per cell?