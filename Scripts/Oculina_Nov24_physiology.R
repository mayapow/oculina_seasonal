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
#install.packages("emmeans")
library(emmeans)

###READ IN AND CLEAN DATA####

##COMING BACK TO THIS SCRIPT @ELLA - JUST READ IN DATA AT LINE 87 - no need to do setup stuff again - I commented it all out

# #read in data
# #metadata
# phys_meta <- read.csv(here("Data", "Nov_2024","oculina_nov24_metadata.csv"))
# #sym density
# sym <- read.csv(here("Data", "Nov_2024", "oculina_nov24_sym_density.csv"))
# #merge sym density with metadata
# sym <- sym %>% left_join(phys_meta, by = "sample_ID")
# #read in chla data
# sym_chla <- read.csv(here("Data", "Nov_2024", "oculina_nov24_chla.csv"))
# #merge with chla data as well
# sym_chla <- sym_chla %>%
#   left_join(sym, by = "sample_ID")
# 
# #remove samples that have no data, weird Chla ones tha have only Ro
# sym_chla <- sym_chla %>% filter(sample_ID != "A12-S" & sample_ID != "A12-A") 
# #here A12 from Nov 24 has no surface area yet so we can't use it - removed both A and S
# 
# #calculate symbiont density
# 
# #the 1x1mm grid should hold 0.1uL
# #we want cells/mL so 10,000 x our counts (1000uL in a mL/0.1uL = x 10,000)
# #resuspension volume column to normalize to volume
# #then use surface area to normalize
# 
# sym_chla <- sym_chla %>%
#   rowwise() %>% #first average counts across your count rows
#   mutate(avg_count = mean(c_across(c(count1, count2, count3, count4, count5)), na.rm = TRUE)) %>%
#   mutate(sym_mL = avg_count*10000) %>% #this gives us cells per mL based on the volume counted
#   mutate(sym_total = sym_mL*resuspension_vol) %>% #total symbionts using our resuspension volume
#   mutate(sym_cm2 = sym_total/sa_cm2) #and normalized to surface area
# 
# #CHECK YOUR COUNTS - SYM SHOULD BE BETWEEN 2x10^6 to 6x10^6 - apo will be much lower
# #@Maya HOW DO I CHECK THE COUNTS? this returns 'NA' for me
# sym_chla %>%
#   group_by(sa_frag) %>%
#   summarise_at(vars(sym_cm2), list(name = mean))
# #this should return values for apo and sym now @ella
# 
# #calculate chla per symbiont
# #this tells us about the photosynthetic capacity of EACH symbiont
# #so e.g. if deep corals only had a few symbionts but they were SO CRAZY FULL OF chlorophyll a they could be fine
# #convert from ug chla to pg chla
# sym_chla <- sym_chla %>%
#   mutate(chla_pg_sym = (ug_chla_total/sym_total)*1000000)
# 
# #ADD column to look at deep vs shallow separately 
# #and deep apo vs shallow apo vs shallow sym since we want to compare this with stats too
# sym_chla <- sym_chla %>%
#   mutate(depth = case_when(grepl("D", sample_ID) ~ "Deep",
#                            grepl("-", sample_ID) ~"Shallow")) %>%
#   mutate(depth_sa = paste(depth,sa_frag))
# 
# #makes physiology folder
# #dir.create(here("Data", "Physiology")) 
# #oh oops sorry, this doesn't need to be in new phys directory
# #I'm moving it back into the Nov 2024 folder @ella
# write.csv(sym_chla, here("Data", "Nov_2024", "oculina_nov24_physio.csv"), row.names = FALSE)

##READ IN DATA HERE!!
sym_chla <- read.csv(here("Data", "Nov_2024", "oculina_nov24_physio.csv"))
#you can check the classes of the variables using this as needed:
#sapply(sym_chla, class)

#convert any variables to correct classes as needed - see example here:
#sym_chla <- sym_chla %>% mutate_at(c('ug_chla_cm'), as.numeric)

#info about variables - add notes that are helpful to you!
#sa_colony = aposymbiotic/symbiotic original designation of the colony
#sa_frag = aposymbiotic/symbiotic designation of the FRAGMENT
#ug_chla_cm = micrograms of chlorophyll a per cm2 of surface area
#sym_cm2 = symbiont density for cm2 of surface area
#chla_pg_sym = picograms of chlorophyll a per symbiont
#depth = deep (30ft), shallow (15ft)

###PLOTS###
#ELLA - EDIT THIS TO JUST LOOK AT SAMPLES SPLIT AND COLORED BY APO SYM OF FRAG
#TAKE OUT ANY COMPARISONS THAT USE THE APO/SYM OF COLONY!!
#ALSO THEN PLEASE PLOT CHLA, SYM DENSITY, AND CHLA PER SYMBIONT
#@ELLA - I EDITED THIS TO TAKE OUT THE DEEP SAMPLES!! 
#SO IT'S ACTUALLY JUST COMPARING WITHIN COLONY

sym_chla_shallow <- sym_chla %>% filter(depth == "Shallow")

#CHLA FRAG POINT
chla_nov_sa_frag_point <- ggplot(sym_chla_shallow, aes(x = sample_ID, y = ug_chla_cm, color = sa_frag)) +
  geom_point(alpha = 0.8, size = 3) +
  theme_classic(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Sample ID",
    y = expression(paste("Chlorophyll a (", mu, "g cm"^-2, ")")),
    color = "Symbiotic state - within colony"
  )
chla_nov_sa_frag_point
ggsave(here("Output", "Nov_2024","chla_nov_sa_frag_point.jpg"), device = "jpg", h = 6, w = 10, chla_nov_sa_frag_point)

#CHLA FRAG BOX
chla_nov_sa_box <- ggplot(sym_chla_shallow, aes(x = sa_frag, y = ug_chla_cm, fill = sa_frag)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.8, width = 0.2) +
  theme_classic(base_size = 22) +
  labs(
    x = "Symbiotic state - within colony",
    y = "Chlorophyll a (ug per cm2)"
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("lightyellow", "brown"))
chla_nov_sa_box
ggsave(here("Output", "Nov_2024","chla_nov_sa_box.jpg"), device = "jpg", h = 6, w = 10, chla_nov_sa_box)

## CHL A PER SYMBIONT ##
#POINT#
chla_sym_nov_sa_point <- ggplot(sym_chla_shallow, aes(x = sample_ID, y = chla_pg_sym, color = sa_frag)) +
  geom_point(alpha = 0.8, size = 3) +
  theme_classic(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Sample ID",
    y = "Chlorophyll a (pg/symbiont)",
    color = "Symbiotic state - within colony"
  )
chla_sym_nov_sa_point
ggsave(here("Output", "Nov_2024","chla_sym_nov_sa_point.jpg"), device = "jpg", h = 6, w = 10, chla_sym_nov_sa_point)

# Chl a per symbiont by sa_frag BOX
chla_sym_nov_sa_box <- ggplot(sym_chla_shallow, aes(x = sa_frag, y = chla_pg_sym, fill = sa_frag)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.8, width = 0.2) +
  theme_classic(base_size = 22) +
  labs(
    x = "Symbiotic state - within colony",
    y = "Chlorophyll a (pg per symbiont)"
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("lightyellow", "brown"))
chla_sym_nov_sa_box
ggsave(here("Output", "Nov_2024","chla_sym_nov_sa_box.jpg"), device = "jpg", h = 6, w = 10, chla_sym_nov_sa_box)

##Sym density## POINT
sym_nov_sa_point <- ggplot(sym_chla_shallow, aes(x = sample_ID, y = sym_cm2, color = sa_frag)) +
  geom_point(alpha = 0.8, size = 3) +
  theme_classic(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Sample ID",
    y = expression(paste("Symbiont density (cells cm"^-2, ")")),
    color = "Symbiotic state - within colony"
  )
sym_nov_sa_point
ggsave(here("Output", "Nov_2024","sym_nov_sa_point.jpg"), device = "jpg", h = 6, w = 10, sym_nov_sa_point)

#SYM DENSITY BOX
sym_nov_sa_box <- ggplot(sym_chla_shallow, aes(x = sa_frag, y = sym_cm2, fill = sa_frag)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.8, width = 0.2) +
  theme_classic(base_size = 22) +
  labs(
    x = "Symbiotic state - within colony",
    y = expression(paste("Symbiont density (cells cm"^-2, ")"))
  ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("lightyellow", "brown"))
sym_nov_sa_box
ggsave(here("Output", "Nov_2024","sym_nov_sa_box.jpg"), device = "jpg", h = 6, w = 10, sym_nov_sa_box)

#scatterplot of apo vs sym shallow samples
sym_chla_shallow_scatter <- ggplot(sym_chla_shallow, aes(x = sym_cm2, y = ug_chla_cm, fill = depth_sa)) +
  geom_point(color='black', shape = 21) +
  scale_fill_manual(values = c("lightyellow","brown"))+
  theme_bw(base_size = 22) +
  geom_smooth(aes(x = sym_cm2, y = ug_chla_cm, group = 1),
              method = "lm", se = TRUE, color = "black", linewidth = 1.1) +
  labs(x = "Symbiont density (cells per cm2)", 
       y = "Chlorophyll a (ug per cm2)", 
       fill = "Symbiotic state - within colony")
sym_chla_shallow_scatter
ggsave(here("Output", "Nov_2024","sym_chla_shallow_scatter.jpg"), device = "jpg", h = 6, w = 10, sym_nov_sa_box)

#looks like we have 2 outliers that are driving these trends

###@ELLA THESE GRAPHS BELOW ARE LOOKING AT THE THREE GROUPS - Including apo/sym and deep group!
###@ELLA - I ADDED LETTER LABELS HERE!!
#chlorophyll a
chla_nov_depth_sa_box <- ggplot(sym_chla, aes(x = depth_sa, y=ug_chla_cm, fill = depth_sa))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Fragment symbiotic state", y = "Chlorophyll a (ug per cm2)")+
  stat_summary(data = sym_chla, aes(x = depth_sa, y = ug_chla_cm), geom = "text", fun = max, vjust = -0.5, size = 8,
               label = c("a", "a", "b"))+ 
  #EDIT LETTERS FOR EACH GRAPH - MAKE SURE THEY MATCH WITH STATS!!
  #SAME LETTERS = SAME (not sig dif from eachother)
  #DIF LETTERS = SIGNFICANTLY DIFFERENT FROM EACHOTHER
  theme(legend.position = "none")+
  ylim(-0.2,7)+ #ELLA - EDIT Y LIMIT IF IT CUTS OFF LETTERS ON THE TOP
  #MAKE SURE YOU DON'T GET THE WARNING THAT IT REMOVES ANY DATA (here, when I put Y=0, it got rid of a few)
  scale_fill_manual(values = c("white","lightyellow","brown")) #ella change the colors here as you'd like
chla_nov_depth_sa_box
ggsave(here("Output", "Nov_2024","chla_nov_depth_sa_box.jpg"), device = "jpg", h = 6, w = 10, chla_nov_depth_sa_box)

#chla per symbiont
chla_sym_nov_depth_sa_box <- ggplot(sym_chla, aes(x = depth_sa, y=chla_pg_sym, fill = depth_sa))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Fragment symbiotic state", y = "Chlorophyll a (pg per symbiont cell)")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("white","lightyellow","brown")) #ella change the colors here as you'd like
chla_sym_nov_depth_sa_box
ggsave(here("Output", "Nov_2024","chla_sym_nov_depth_sa_box.jpg"), device = "jpg", h = 6, w = 10, chla_sym_nov_depth_sa_box)

#symbiont density
sym_nov_depth_sa_box <- ggplot(sym_chla, aes(x = depth_sa, y=sym_cm2, fill = depth_sa))+
  geom_boxplot()+
  geom_jitter(alpha=0.8, width=0.2)+
  theme_classic(base_size = 22)+
  labs(x = "Fragment symbiotic state", y = "Symbiont density (cells per cm2)")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("white","lightyellow","brown")) #ella change the colors here as you'd like
sym_nov_depth_sa_box
ggsave(here("Output", "Nov_2024","sym_nov_depth_sa_box.jpg"), device = "jpg", h = 6, w = 10, sym_nov_depth_sa_box)

#linear plot comparing symbiont density vs chlorophyll a across all 3 groups
sym_chla_scatter <- ggplot(sym_chla, aes(x = sym_cm2, y = ug_chla_cm, fill = depth_sa)) +
  geom_point(color='black', shape = 21) +
  scale_fill_manual(values = c("white","lightyellow","brown"))+
  theme_bw(base_size = 22) +
  geom_smooth(aes(x = sym_cm2, y = ug_chla_cm, group = 1),
              method = "lm", se = TRUE, color = "black", linewidth = 1.1) +
  labs(x = "Symbiont density (cells per cm2)", 
       y = "Chlorophyll a (ug per cm2)", 
       fill = "Fragment symbiotic state")
sym_chla_scatter
#ggsave(here("Output", "Nov_2024","sym_chla_scatter.jpg"), device = "jpg", h = 6, w = 10, sym_chla_scatter)
ggsave(
  filename = here("Output", "Nov_2024", "sym_chla_scatter.jpg"),
  plot     = sym_chla_scatter,
  device   = "jpg",
  h        = 6,
  w        = 10
)
#looks like we have 2 outliers that are driving these trends - this is fine to present for now

####STATS####

###ELLA EDIT THIS TO EXAMINE THE DIFFERENCES OF CHLA AND SYM DENSITY USING ANOVAS
#HERE YOU HAVE AN EXAMPLE OF CHLA - ADD FOR SYM DENSITY AND CHLA/SYMBIONT AS WELL!!
#THESE ARE JUST T-TESTS HERE BECAUSE THEY ARE JUST COMPARING BETWEEN ONE VARIABLE: SYM vs APO


#sometimes you need to edit variables to make them be factors for the stats to work - see example here:
#color_avg_nox$month <- as.factor(color_avg_nox$month)

# #look to see if means of different groups are the same/different
# #chla_cm
# chla_as_frag_summ <- sym_chla_shallow %>% group_by(sa_frag) %>% get_summary_stats(ug_chla_cm, type = "mean_sd")
# chla_as_frag_summ #ya difference but still fairly small
# #you can report these values in your slide along with stats (see below)

##@ELLA - RERUN THESE USING ONLY THE SHALLOW DATA TO DISCUSS WITHIN-FRAG DIFS!
#I edited to use the sym_chla_shallow data here instead of sym_chla
#AND copied results in! rerun just to make sure

# 1) Chl a 
chla_frag_aov <- aov(ug_chla_cm ~ sa_frag, data = sym_chla_shallow)
summary(chla_frag_aov)

# Df Sum Sq Mean Sq F value   Pr(>F)    
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# sa_frag      1  19.84  19.844   27.44 2.53e-06 ***
# Residuals   56  40.49   0.723  

# 2) Symbiont density
sym_frag_aov <- aov(sym_cm2 ~ sa_frag, data = sym_chla_shallow)
summary(sym_frag_aov)

#             Df    Sum Sq   Mean Sq F value Pr(>F)  
# sa_frag      1 1.294e+12 1.294e+12    3.85 0.0547 .
# Residuals   56 1.883e+13 3.362e+11        

# 3) Chl a per symbiont
chla_sym_frag_aov <- aov(chla_pg_sym ~ sa_frag, data = sym_chla_shallow)
summary(chla_sym_frag_aov)

#             Df Sum Sq Mean Sq F value  Pr(>F)   
# sa_frag      1   54.2   54.20    9.67 0.00294 **
# Residuals   56  313.9    5.61                             

#Ella report results from here to say what is significant and what is not!
#say here what is significant or not and then pair that with your box plots!
#let's text about this tomorrow when you get here

#SIGNIFICANCE YAY
#Maya here we go
#Sym frags have significantly higher chlorophyll a per cm2 than apo frags(p = 2.28e-09 )
#Sym frags have higher sym densities, apo frags have very low densities(p = 0.000707)
#S/A frags differ in how much chl a each symbiont contains? (p=0.00015)
#maybe bc apo frags contain fewer sym, sym are stressed → fewer Chl a per cell?
#yes this is great! I really like these explanations
#@Ella only thing you will need to edit now is that sym are

###Maya running stats to look at differences between 3 groups!

# 1) Chl a 
chla_frag_aov <- aov(ug_chla_cm ~ depth_sa, data = sym_chla)
summary(chla_frag_aov)

#             Df Sum Sq Mean Sq F value   Pr(>F)    
# depth_sa     2  27.45  13.727   23.73 1.36e-08 ***
# Residuals   70  40.49   0.578   

# 1) Chla pairwise comparisons
chla_emm <- emmeans::emmeans(chla_frag_aov, ~depth_sa)
pairs(chla_emm)

# contrast                                 estimate    SE df t.ratio p.value
# Deep Aposymbiotic - Shallow Aposymbiotic   -0.214 0.242 70  -0.886  0.6512
# Deep Aposymbiotic - Shallow Symbiotic      -1.384 0.242 70  -5.722  <.0001
# Shallow Aposymbiotic - Shallow Symbiotic   -1.170 0.200 70  -5.857  <.0001

# 2) Symbiont density
sym_frag_aov <- aov(sym_cm2 ~ depth_sa, data = sym_chla)
summary(sym_frag_aov)

#             Df    Sum Sq   Mean Sq F value   Pr(>F)    
# depth_sa     2 5.983e+12 2.992e+12   11.12 6.39e-05 ***
# Residuals   70 1.883e+13 2.690e+11                                 

# 2) Sym density pairwise comparisons
sym_emm <- emmeans::emmeans(sym_frag_aov, ~depth_sa)
pairs(sym_emm)

# contrast                                 estimate     SE df t.ratio p.value
# Deep Aposymbiotic - Shallow Aposymbiotic  -477874 165000 70  -2.897  0.0138
# Deep Aposymbiotic - Shallow Symbiotic     -776647 165000 70  -4.708  <.0001
# Shallow Aposymbiotic - Shallow Symbiotic  -298773 136000 70  -2.194  0.0793

###THIS IS REALLY INTERESTING!!
##I THINK THAT THIS IS DRIVEN BY THE FACT THAT DEEP CORALS HAVE VERY VERY FEW SYM

# 3) Chl a per symbiont
chla_sym_frag_aov <- aov(chla_pg_sym ~ depth_sa, data = sym_chla)
summary(chla_sym_frag_aov)

#             Df Sum Sq Mean Sq F value   Pr(>F)    
# depth_sa     2   76.4   38.18   8.436 0.000522 ***
# Residuals   70  316.8    4.53                           

# 3) Chla per sym pairwise comparisons
chla_sym_emm <- emmeans::emmeans(chla_sym_frag_aov, ~depth_sa)
pairs(chla_sym_emm)

##HERE - SHALLOW APO HAVE VERY LOW CHLA PER SYM EVEN THO THEY HAVE SYM
##THIS FOLLOWS YOUR CLAIM REALLY NICELY ABOVE ABOUT SHALLOW APO = STRESSED -> low chla per sym
# contrast                                 estimate    SE df t.ratio p.value
# Deep Aposymbiotic - Shallow Aposymbiotic   -0.397 0.677 70  -0.586  0.8280
# Deep Aposymbiotic - Shallow Symbiotic      -2.330 0.677 70  -3.444  0.0028
# Shallow Aposymbiotic - Shallow Symbiotic   -1.933 0.559 70  -3.461  0.0026

