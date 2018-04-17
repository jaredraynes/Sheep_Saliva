library(tidyverse)
library(lmerTest)
library(emmeans)

pH <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "pH")

#pH wrangling

names(pH) <- c("Treatment.Group", "ID", "M.semitendinosus", "M.longissimus.dorsi", "mistakes")

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
