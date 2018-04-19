library(tidyverse)
library(lmerTest)
library(emmeans)

pH <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "pH")

#pH wrangling

pH$Brand[13] = 64
pH <- pH[1:4]
names(pH) <- c("Treatment.Group", "ID", "M.semitendinosus", "M.longissimus.dorsi")
pH <- mutate(pH, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))
pH_long <- pH %>% 
  gather(key = "Muscle.type", value = "pH", "M.semitendinosus", "M.longissimus.dorsi")

pH_long$Treatment.Group <- factor(pH$Treatment.Group)
pH_long$ID <- factor(pH$ID)
pH_long$Muscle.type <- factor(pH_long$Muscle.type)
pH_long$Feed <- factor(pH_long$Feed)
pH_long$Stress <- factor(pH_long$Stress)
pH_long$Feed <- relevel(pH_long$Feed, ref= 'TRUE')



#pH initial look

ggplot(pH_long, aes(x = Treatment.Group, y = pH, colour = Muscle.type))+
  geom_boxplot()

######### looks like there is definitely an effect of stress on the pH
#### of both muscle types. M. Semit also consistantly higher pH. no effect
### on feeding

#######modelling

lm.pH <- lm(pH ~ Treatment.Group, data = pH_long)
anova(lm.pH)
summary(lm.pH)

lm.pH2 <- lm(pH ~ Treatment.Group*Muscle.type, data = pH_long)
anova(lm.pH2)
summary(lm.pH2)
emmeans(lm.pH2, pairwise ~ Treatment.Group*Muscle.type)

lm.pH3 <- lm(pH ~ Stress + Feed, data = pH_long)
emmeans(lm.pH3, pairwise ~ Stress + Feed)

lm.pH4 <- lm(pH ~ Stress*Feed + Muscle.type, data = pH_long)
summary(lm.pH4)
emmeans(lm.pH4, pairwise ~ Stress + Feed)

lm.pH5 <- lm(pH ~ Muscle.type, data = pH_long)
summary(lm.pH5)

lmer.pH1 <- lmer(pH ~ Treatment.Group*Muscle.type + (1|Feed) + (1|Stress), data = pH_long)
summary(lmer.pH1)

