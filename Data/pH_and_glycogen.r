library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(ggrepel)


#pH
pH <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "pH")
pH$Brand[13] = 64
pH <- pH[1:4]
names(pH) <- c("Treatment.Group", "ID", "M.semitendinosus", "M.longissimus.dorsi")
pH <- mutate(pH, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))
pH <- pH[-51,] ##There were two measurements for brand 51 so removed one as redundent for pH measurement

#Glycogen
Glycogen <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Glycogen")
Glycogen <- Glycogen[-c(31),] ## remove error message
G1 <- Glycogen[1:5] #the treatment groups are in different columns this time
G2 <- Glycogen[6:10] #need to have identical names to use row_bind
names(G2) <- c("Treatment group", "Brand", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen")
G2$`Treatment group` <- as.character(G2$`Treatment group`) 
Glycogen_comb <- bind_rows(G1, G2) #combining the columns together
Glycogen_comb <- mutate(Glycogen_comb, Feed = ifelse(`Treatment group` <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(`Treatment group` == 2|`Treatment group` == 4, TRUE, FALSE)) #added Stress column
names(Glycogen_comb) <- c("Treatment.Group", "ID", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen", "Feed", "Stress")
#Glycogen_long <- Glycogen_comb %>% 
#  gather(key = "Measure", value = "mg/g", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen")
#names(Glycogen_long) <- c("Treatment.Group", "ID", "Feed", "Stress", "Measure", "mg/g")
#Glycogen_long[1:5] <- lapply(Glycogen_long[1:5], as.factor)
#Glycogen_sel <- filter(Glycogen_long, Measure == "Corrected glycogen")
#Glycogen_long

###JOINING

Gly_pH <- pH %>% 
  select(ID, M.semitendinosus) %>% 
  right_join(Glycogen_comb, by = "ID")

#Modelling
fit1 <- lm(`Corrected glycogen` ~ `M.semitendinosus`, data = Gly_pH)
summary(fit1)


#PLotting

ggplot(data = Gly_pH, aes(x = `M.semitendinosus`
                          , y = log(`Corrected glycogen`))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)



