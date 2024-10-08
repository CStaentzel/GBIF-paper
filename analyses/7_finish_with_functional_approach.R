#' @title
#' Finish with functional approach

#' @description 
#' We created six functional groups

#' @mail cybill.staentzel@engees.unistra.fr

#' @date 2023/01/24

#' Macroinvertebrates were split by Insects, Crustaceans, and Molluscs, to represent differences in dispersal capabilities, 
which is an important determinant of sensitivity to habitat modifications 

#For each level (from 6 to 12), we created functional groups (xeno1_2_level12 is the file where we have all the observations (xenodiversity) at level 12)
mol <- grep("^Mol", names(xeno1_2_level12), value = TRUE)
cru <- grep("^Arth_Cru", names(xeno1_2_level12), value = TRUE)
ins <- grep("^Arth_Ins", names(xeno1_2_level12), value = TRUE)

#Example with Molluscs
library(vegan)
xeno1_2_level12$SR_MOLexotic <- specnumber(xeno1_2_level12[mol])
xeno1_2_level12$Occ_MOLexotic <- rowSums(dataframe[mol])

#Fish were split into three modalities focusing on their spawning habitat: (i) euryoparous: generalists, with no clear spawning habitat preferences, 
(ii) limnoparous: preference to spawn in slow or stagnant waters, and (iii) rheoparous: preference to spawn in running waters
