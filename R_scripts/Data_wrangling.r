library(tidyverse)
library(lmerTest)
library(emmeans)


#Excel spreadsheet has mulitple sheets so check the names of them

excel_sheets("Data/Copy of 20180413_DataSchool_Glycogen.xlsx")

#Import all the sheets individually as tibbles
pH <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "pH")
Glycogen <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Glycogen")
Glycogen_biopsy <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Glycogen biopsy")
Weight_gain <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Weight gain")
Cortisol <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Cortisol")
Saliva <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Saliva")

#Wrangle all individual sheets

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

### Glycogen_biopsy data wrangling

Glycogen_biopsy <- Glycogen_biopsy[1:4]
Glycogen_biopsy <- Glycogen_biopsy[-c(12:14),]

Glycogen_biopsy$Treatment.Group <- c(1,1,1,1,1,2,2,2,2,2,2)
Glycogen_biopsy_long <- Glycogen_biopsy %>% 
  gather(key = "Measure", value = "mg/g", "Glycogen mg/g", "Lactate mg/g", "Corrected Glycogen")
names(Glycogen_biopsy_long) <- c("ID", "Treatment.Group", "Measure", "mg/g")
Glycogen_biopsy_long$Treatment.Group <- factor(Glycogen_biopsy_long$Treatment.Group)
Glycogen_biopsy_long$ID <- factor(Glycogen_biopsy_long$ID)

#Glycogen_ Biopsy initial look

ggplot(Glycogen_biopsy_long, aes(x = Treatment.Group, y = `mg/g`, colour = Measure))+
  geom_boxplot() 

##stress causes lower amount of glycogen 


###Weight Gain data wrangling

Weight_gain <- Weight_gain[1:8]
Weight_gain <- Weight_gain[-c(61),]
names(Weight_gain) <- c("Treatment.Group", "ID", "-1", "0", "1", "2", "3", "4")
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

