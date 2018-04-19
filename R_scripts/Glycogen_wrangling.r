library(tidyverse)
library(readxl)
library(lmerTest)
library(emmeans)

Glycogen <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Glycogen")

### Glycogen data wrangling
Glycogen <- Glycogen[-c(31),] ## remove error message
G1 <- Glycogen[1:5] #the treatment groups are in different columns this time
G2 <- Glycogen[6:10] #need to have identical names to use row_bind
names(G2) <- c("Treatment group", "Brand", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen")
G2$`Treatment group` <- as.character(G2$`Treatment group`) 
Glycogen_comb <- bind_rows(G1, G2) #combining the columns together
Glycogen_long <- Glycogen_comb %>% 
  gather(key = "Measure", value = "mg/g", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen")
names(Glycogen_long) <- c("Treatment.Group", "ID", "Measure", "mg/g")
Glycogen_long$Treatment.Group <- factor(Glycogen_long$Treatment.Group)
Glycogen_long$ID <- factor(Glycogen_long$ID)

#Glycogen initial look

ggplot(Glycogen_long, aes(x = Treatment.Group, y = `mg/g`, colour = Measure))+
  geom_boxplot() 

##looks like stress (treatments 2 and 4) causes lower amount of glycogen, feed
# maybe has an influence

