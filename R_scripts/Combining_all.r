#Load libraries

library(tidyverse)
library(readxl)
library(lmerTest)
library(factoextra)
library(emmeans)

#Import all the sheets individually as tibbles
pH <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "pH")
Glycogen <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Glycogen")
Weight_gain <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Weight gain")
Cortisol <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Cortisol")


#pH
pH$Brand[13] = 64
pH <- pH[1:4]
names(pH) <- c("Treatment.Group", "ID", "M.semitendinosus", "M.longissimus.dorsi")
pH <- mutate(pH, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))

#Glycogen
Glycogen <- Glycogen[-c(31),] ## remove error message
G1 <- Glycogen[1:5] #the treatment groups are in different columns this time
G2 <- Glycogen[6:10] #need to have identical names to use row_bind
names(G2) <- c("Treatment group", "Brand", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen")
G2$`Treatment group` <- as.character(G2$`Treatment group`) 
Glycogen_comb <- bind_rows(G1, G2) #combining the columns together
names(Glycogen_comb) <- c("Treatment.Group", "ID", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen")
Glycogen_comb <- mutate(Glycogen_comb, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))
remove(Glycogen, G1, G2)
Glycogen_comb$ID[13] = 64

#Weight Gain
Weight_gain <- Weight_gain[1:8]
Weight_gain <- Weight_gain[-c(61),]
names(Weight_gain) <- c("Treatment.Group", "ID", "-1", "0", "1", "2", "3", "4")
Weight_gain <- mutate(Weight_gain, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))
Weight_gain$ID[13] = 64

#Cortisol
Cortisol <- Cortisol[1:5] #removing all columns that aren't needed
names(Cortisol) <- c("Treatment.Group", "ID", "1", "2", "3")
Cortisol <- Cortisol[-c(13),] #removing datapoint 15 as was replaced by 64
Cortisol <- mutate(Cortisol, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))

###Combine all into one sheet

comb <- left_join(pH, Cortisol, by = "ID") %>% 
  left_join(Glycogen_comb, by = "ID") %>% 
  left_join(Weight_gain, by = "ID") %>% 
  left_join(Cortisol, by = "ID")

pca_comb <- prcomp(comb[1:33], scale=TRUE)
summary(pca_comb)


