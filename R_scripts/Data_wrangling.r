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


