library(tidyverse)
library(readxl)
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
peptide_long$ID <- factor(peptide_long$ID)
peptide_long$Treatment.Group <- factor(peptide_long$Treatment.Group)
peptide_long$Nutrition <- factor(peptide_long$Nutrition)
peptide_long$peptide <- factor(peptide_long$peptide)
peptide_long$peak.area <- as.numeric(peptide_long$peak.area)

select1 <- filter(peptide_long, Treatment.Group == 1:2)

## try a PCA


#### need to convert all to numbers as currently chars *****

### try tapply or lapply
##http://r.789695.n4.nabble.com/Apply-as-factor-or-as-numeric-etc-to-multiple-columns-td893777.html
peptide_T[4:2117] <- lapply(peptide_T[4:2117], as.numeric)
##make the rest factors
peptide_T[1:3] <- lapply(peptide_T[1:3], as.factor)

###add column for stress
peptide_T <- mutate(peptide_T, Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))

peptide_T <- select(peptide_T, Stress, everything())
peptide_T$Stress <- factor(peptide_T$Stress)

######PCA#######
pca1 <- prcomp(na.omit(peptide_T[5:2114]), scale=TRUE)
summary(pca1)
#PC1 - PC 14 are dimensions (not our columns)
#And it is the Cumulative Proportion that shows us the percentage of the total variation
#(ie, important dimenions are where the portion of variance is a significant value,
#     different schools of thought (SD < 1, Portion of Vaiance < 0.1 or Cum P <- 75%))

###factoextra
library(factoextra)

quali.sup <- peptide_T[1:48, 1:4]
head(quali.sup)


fviz_eig(pca1)

fviz_pca_ind(pca1,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping
###Stress
fviz_pca_ind(pca1, 
             habillage = peptide_T[1:48, 1]) +
  theme_minimal()

###Treatment Group
fviz_pca_ind(pca1, 
             habillage = peptide_T[1:48, 3]) +
  theme_minimal()

###Nutrition
fviz_pca_ind(pca1, 
             habillage = peptide_T[1:48, 4], addEllipses = TRUE, ellipse.level = 0.68) +
  theme_minimal()




library(ggfortify)



autoplot(pca1, data = peptide_T, colour = 'Stress',
         loadings = FALSE, loadings.colour = 'blue',
         loadings.label = FALSE, loadings.label.size = 3)

autoplot(pca1, data = peptide_T, colour = 'Nutrition',
         loadings = FALSE, loadings.colour = 'blue',
         loadings.label = FALSE, loadings.label.size = 3)

autoplot(pca1, data = peptide_T, colour = 'Treatment.Group',
         loadings = FALSE, loadings.colour = 'blue',
         loadings.label = FALSE, loadings.label.size = 3)


###factorminR

#example

#res.pca = PCA(decathlon[,1:12], scale.unit=TRUE, ncp=5, quanti.sup=c(11: 12), graph=T)

pep.PCA <- PCA(peptide_T[,5:2118], scale.unit=TRUE, ncp=5, quali.sup=c(1:4), graph=T)

pep.PCA2 <- PCA(peptide_T[,5:2118], scale.unit=TRUE, ncp=5, quali.sup=1, graph=T)               

plot.PCA(pep.PCA2, axes=c(1, 2), choix="var", habillage=13)              



