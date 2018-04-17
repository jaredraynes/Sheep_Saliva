library(tidyverse)
library(lmerTest)
library(emmeans)
library(FactoMineR)




peptide <- read_xlsx("Data/Copy of Data Matrix_Peptide Peak Area.xlsx")

peptide_T <- t(peptide) #transpose
peptide_T <- rownames_to_column(data.frame(peptide_T), 'ID')
peptide_T$ID[1] <- "ID"
peptide_T[] <- lapply(peptide_T, as.character)
peptide_T$X2[1] <- "Nutrition"
peptide_T$X1[1] <- "Treatment.Group"
colnames(peptide_T) <- peptide_T[1, ]
peptide_T <- peptide_T[-1 ,]

peptide_long <- gather(peptide_T, key = "peptide", value = peak.area, -ID, -Nutrition, -Treatment.Group)

ggplot(data = peptide_long, aes(x=Treatment.Group, y = peak.area, colour = peptide)) +
  geom_point()

