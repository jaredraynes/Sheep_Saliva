library(tidyverse)
library(lmerTest)
library(emmeans)

Cortisol <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Cortisol")

#Cortisol wrangling

Cortisol <- Cortisol[1:5] #removing all columns that aren't needed
names(Cortisol) <- c("Treatment.Group", "ID", "1", "2", "3")
Cortisol_long <- Cortisol %>% 
  gather(key = "Time.point", value = "Cortisol_nmol/L", "1", "2", "3")
Cortisol_long$Treatment.Group <- factor(Cortisol_long$Treatment.Group)
Cortisol_long$ID <- factor(Cortisol_long$ID)
Cortisol_long$Time.point <- factor(Cortisol_long$Time.point)

#Cortisol initial look

ggplot(Cortisol_long, aes(x = Treatment.Group, y = `Cortisol_nmol/L`, colour = Time.point))+
  geom_boxplot() +
  scale_y_log10()

##highest cortisol after kill(time.point3), drop in cortisol after stress
##looks like no influence for treatment group (maybe combine)
