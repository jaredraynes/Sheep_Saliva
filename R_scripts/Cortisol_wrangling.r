library(tidyverse)
library(readxl)
library(lmerTest)
library(emmeans)

Cortisol <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Cortisol")

#Cortisol wrangling

Cortisol <- Cortisol[1:5] #removing all columns that aren't needed
names(Cortisol) <- c("Treatment.Group", "ID", "1", "2", "3")
Cortisol <- Cortisol[-c(13),] #removing datapoint 15 as was replaced by 64
Cortisol <- mutate(Cortisol, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))

Cortisol_long <- Cortisol %>% 
  gather(key = "Time.point", value = "Cortisol_nmol/L", "1", "2", "3")
Cortisol_long$Treatment.Group <- factor(Cortisol_long$Treatment.Group)
Cortisol_long$ID <- factor(Cortisol_long$ID)
Cortisol_long$Time.point <- factor(Cortisol_long$Time.point)


#Cortisol initial look

ggplot(Cortisol_long, aes(x = Treatment.Group, y = `Cortisol_nmol/L`, colour = Time.point, shape = Stress))+
  geom_jitter()
  scale_y_log10()

##highest cortisol after kill(time.point3), drop in cortisol after stress
##looks like no influence for treatment group (maybe combine)

########### modelling ##############

lm1 <- lm(`Cortisol_nmol/L` ~ Treatment.Group, data = Cortisol_long)
anova(lm1)
summary(lm1)
#no difference between treatments when timepoint not considered 

lm2 <- lm(`Cortisol_nmol/L` ~Treatment.Group*Time.point, data = Cortisol_long)
anova(lm2)
summary(lm2)

lm3 <- lm(`Cortisol_nmol/L` ~Treatment.Group+Time.point, data = Cortisol_long)
anova(lm3)
summary(lm3)

lmer1 <- lmer(`Cortisol_nmol/L` ~ Treatment.Group*Time.point + (1|Feed) + (1|Stress), data = Cortisol_long)
anova(lmer1)
summary(lmer1)

emmeans(lm2, pairwise~Treatment.Group|Time.point)
plot(lmer1, which=1)
