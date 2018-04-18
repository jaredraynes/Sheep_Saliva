library(tidyverse)
library(lmerTest)
library(emmeans)

pH <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "pH")

#pH wrangling

pH$Brand[13] = 64
pH <- pH[1:4]
names(pH) <- c("Treatment.Group", "ID", "M.semitendinosus", "M.longissimus.dorsi")

pH_long <- pH %>% 
  gather(key = "Muscle.type", value = "pH", "M.semitendinosus", "M.longissimus.dorsi")

pH_long$Treatment.Group <- factor(pH$Treatment.Group)
pH_long$ID <- factor(pH$ID)    

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


