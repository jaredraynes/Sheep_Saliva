library(tidyverse)
library(readxl)
library(lmerTest)
library(factoextra)
library(emmeans)

Weight_gain <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Weight gain")

###Weight Gain data wrangling

Weight_gain <- Weight_gain[1:8]
Weight_gain <- Weight_gain[-c(61),]
names(Weight_gain) <- c("Treatment.Group", "ID", "-1", "0", "1", "2", "3", "4")
Weight_gain <- mutate(Weight_gain, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))

Weight_gain_long <- Weight_gain %>% 
  gather(key = "Week", value = "Weight.kg", "-1", "0", "1", "2", "3", "4")
Weight_gain_long$Treatment.Group <- factor(Weight_gain_long$Treatment.Group)
Weight_gain_long$ID <- factor(Weight_gain_long$ID)
Weight_gain_long$Week <- factor(Weight_gain_long$Week)

##initial look at weight gain

ggplot(Weight_gain_long, aes(x = Week, y = Weight.kg, colour = Treatment.Group))+
  geom_boxplot()

## treatments no difference at birth, restricted feed lowers weight,
## stress lowers weight
