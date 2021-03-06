library(tidyverse)
library(lmerTest)
library(emmeans)

Glycogen_biopsy <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Glycogen biopsy")

### Glycogen_biopsy data wrangling

Glycogen_biopsy <- Glycogen_biopsy[1:4]
Glycogen_biopsy <- Glycogen_biopsy[-c(12:14),]

Glycogen_biopsy$Treatment.Group <- c(1,1,1,1,1,2,2,2,2,2,2)
Glycogen_biopsy_long <- Glycogen_biopsy %>% 
  gather(key = "Measure", value = "mg/g", "Glycogen mg/g", "Lactate mg/g", "Corrected Glycogen")
names(Glycogen_biopsy_long) <- c("ID", "Treatment.Group", "Measure", "mg/g")
Glycogen_biopsy_long$Treatment.Group <- factor(Glycogen_biopsy_long$Treatment.Group)
Glycogen_biopsy_long$ID <- factor(Glycogen_biopsy_long$ID)
Glycogen_biopsy_long$Measure <- factor(Glycogen_biopsy_long$Measure)
Glycogen_biopsy_sel <- filter(Glycogen_biopsy_long, Measure == "Corrected Glycogen")
head(Glycogen_biopsy_long)
#Glycogen_ Biopsy initial look

ggplot(Glycogen_biopsy_sel, aes(x = Treatment.Group, y = `mg/g`))+
  geom_boxplot() 

##stress causes lower amount of glycogen 

lm.Glybiops1 <- lm(`mg/g` ~ Treatment.Group, data = Glycogen_biopsy_sel)