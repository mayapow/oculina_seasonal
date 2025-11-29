
##### Coral Physiology Oculina Seasonal Project#####

#Maya Powell & Jamie Long & Ella Hennessey
#Last edited November 28th 2025

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


#read in metadata
phys_meta <- read.csv(here("Data", "Nov_2024", "oculina_nov24_chla.csv"))

#####Ash Free Dry Weight#####
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

#quickly see if there is a dif between species
DW.mod.spp <- lm(dw_mg_cm2~species, data = avg_DW)
Anova(DW.mod.spp)
summary(DW.mod.spp)


#and sensitivity
DW.mod.stress <- lm(dw_mg_cm2~stress, data = avg_DW)
Anova(DW.mod.stress)
summary(DW.mod.stress)
#based on graph, looks like Peyd and Tfro should be switched
#we'll see what happens with the rest of the TPC data!



##### Chlorophyll a#####


#read in dataframe
sym_chla <- read.csv(here("Data", "Nov_2024", "oculina_nov24_chla.csv"))
meta <- read.csv(here("Data", "Nov_2024", "oculina_nov24_metadata.csv"))

#remove samples that have no data
sym_chla <- sym_chla %>% 
  left_join(meta, by = "sample_ID")
sym_chla <- sym_chla %>% filter(full_ID != "A12-1124") 

sym_chla %>% 
  filter(full_ID == "A12-1124")
#here A12 from Nov 24 has no surface area yet so we can't use it

#you can check the classes of the variables using this as needed:
sapply(sym_chla, class)

#convert any variables to correct classes as needed - see example here
sym_chla <- sym_chla %>% mutate_at(c('ug_chla_cm'), as.numeric)

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

#reorder variables so that the timepoints go in chronological order
sym_chla$timepoint <- factor(sym_chla$timepoint, levels=c('Nov_23', 'May_24', 'Nov_24'))

###PLOTS###
#USING FOR 395
#dot plot of chlorophyll a looking at all samples split and colored by apo_sym
#also looking at the different time points
chla_sa_point <- ggplot(sym_chla, aes(x = sample_ID, y=ug_chla_cm, color = sa_colony))+
  geom_point(alpha=0.8, size = 3)+
  theme_classic(base_size = 22)+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(sa_colony~timepoint, scales = "free")
chla_sa_point

#@Ella - here's how to save a plot - it will save in your working directory folder
#go through and save the rest of them by editing this line of code for each plot!
ggsave(chla_sa_point, file = "chla_sa_point.pdf", h=6, w=6) #change height and width and resave as needed

#now look at it without apo & sym just seasonal
chla_time_point <- ggplot(sym_chla, aes(x = sample_ID, y=ug_chla_cm, color = timepoint))+
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
  group_by(sample_ID) %>%
  complete(timepoint = time_levels) %>%
  ungroup()

#chla over time for each colony
#eeee I can't wait to see the rest of this data filled in it's going to be so cool!!!
#this plot is a bit chaotic but I think important to show how individual colonies change over time
chla_time_line <- ggplot(sym_chla_time, aes(x = timepoint, y = ug_chla_cm, group = sample_ID, color = sample_ID)) +
  geom_line(na.rm = FALSE, linewidth = 0.7, alpha = 0.7) +
  geom_point(data = sym_chla_time, size = 3) +
  labs(x = "Timepoint", y = "ug chla per cm2", color = "Sample ID")+
  theme_classic(base_size = 22) +
  theme(axis.text.x = element_text(angle = 90),legend.position = "right")
chla_time_line
#this will say its removing rows with values outside the scale range
#that's just bc it's removing the rows without data so it's totally fine!!

chla_time_line_sa <- chla_time_line +
  facet_wrap(~sa_colony)
chla_time_line_sa

ggsave("chla_time_line_colony_oculina_seasonal.pdf", chla_time_line, h = 10, w = 15)

ggsave("chla_time_line_colony_sa_oculina_seasonal.pdf", chla_time_line_sa, h = 10, w = 15)

#####Symbiont Density #####
sym <- read.csv(here("Data", "Physiology", "oculina_nov24_sym_density.csv"))

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
chla_as_summ <- sym_chla %>% group_by(sa_colony) %>% get_summary_stats(ug_chla_cm, type = "mean_sd")
chla_as_summ #huge sd - probs no difference in stats
chla_time_summ <- sym_chla %>% group_by(timepoint) %>% get_summary_stats(ug_chla_cm, type = "mean_sd")
chla_time_summ #looks like def a difference between may/the novs, maybe all 3
chla_as_time_summ <- sym_chla %>% group_by(sa_colony, timepoint) %>% get_summary_stats(ug_chla_cm, type = "mean_sd")
chla_as_time_summ #yes differences here too but not sure if the interaction matters or if time just matters more
#you can report these values in your slide along with stats (see below)

#run ANOVA to look at differences - @Ella we will do better stats later that are more complex and fit the data better!
#this is so you can report something for now for the surf :)
chla_time_sa_aov <- aov(ug_chla_cm ~ sa_colony*timepoint, data=sym_chla)
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
