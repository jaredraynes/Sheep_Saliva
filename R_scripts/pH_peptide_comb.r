library(tidyverse)
library(readxl)
library(lmerTest)
library(emmeans)
library(FactoMineR)
library(factoextra)

pH <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "pH")

#pH wrangling
pH$Brand[13] = 64
pH <- pH[1:4]
names(pH) <- c("Treatment.Group", "ID", "M.semitendinosus", "M.longissimus.dorsi")
pH <- mutate(pH, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))

##filter to find the dark cutters (pH >5.7)
filt_pH <- filter(pH, `M.semitendinosus` >= 5.7)


##Peptide data wrangling
peptide <- read_xlsx("Data/Copy of Data Matrix_Peptide Peak Area.xlsx")

peptide_T <- t(peptide) #transpose
peptide_T <- rownames_to_column(data.frame(peptide_T), 'ID')
peptide_T$ID[1] <- "ID"
peptide_T[] <- lapply(peptide_T, as.character)
peptide_T$X2[1] <- "Nutrition"
peptide_T$X1[1] <- "Treatment.Group"
colnames(peptide_T) <- peptide_T[1, ]
peptide_T <- peptide_T[-1 ,]
peptide_T$ID <- as.numeric(peptide_T$ID)

##Join the pH data and the peptide
peptide_pH <- pH %>% 
  select(ID, M.semitendinosus) %>% 
  right_join(peptide_T, by = "ID") %>% 
###add columns for stress and dark cutting
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE)) %>% 
  mutate(Darkcutting = ifelse(M.semitendinosus >= 5.7, TRUE, FALSE)) %>% 
  select(Stress, everything()) %>% 
  select(Darkcutting, everything()) 
  

peptide_pH[7:2120] <- lapply(peptide_pH[7:2120], as.numeric)
##make the rest factors
peptide_pH[c(1,2,4,5,6)] <- lapply(peptide_pH[c(1,2,4,5,6)], as.factor)

#Make Sheep ID into the Row names
peptide_pH <- peptide_pH[-c(40),]
peptide_pH <- column_to_rownames(peptide_pH, var="ID")


######PCA#######
pca1 <- prcomp(peptide_pH[6:2119], scale=TRUE)
summary(pca1)

quali.sup <- peptide_pH[, 1:5]
head(quali.sup)

fviz_eig(pca1)

###Stress
fviz_pca_ind(pca1, 
             habillage = peptide_pH$Stress) +
  theme_minimal()

###Nutrition
fviz_pca_ind(pca1, 
             habillage = peptide_pH$Nutrition) +
  theme_minimal()

###Treatment Group
fviz_pca_ind(pca1, 
             habillage = peptide_pH$Treatment.Group) +
  theme_minimal()

###Darkcutting
fviz_pca_ind(pca1, 
             habillage = peptide_pH$Darkcutting, geom.ind = "text") +
  theme_minimal()

###Nutrition with ellipses
fviz_pca_ind(pca1, 
             habillage = peptide_pH[1:48, 4], addEllipses = TRUE, 
             ellipse.level = 0.68, 
             title ="Saliva Peptide Analysis",
             repel = TRUE)+
  theme_minimal()

