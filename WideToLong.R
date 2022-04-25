library(tidyr)
#================try with our data=====================================
library(readr)
data_final_wide <- read_csv("//l2export/iss02.cenir/analyse/meeg/BETAPARK/code/R_SCRIPTS/Rscripts_M2analysis/data/data_final.csv")
data_final_wide$medianBeta_mainSeule<-as.double(data_final_wide$medianBeta_mainSeule)
data_final_wide$medianBeta_pendule<-as.double(data_final_wide$medianBeta_pendule)

gathercols <- c("logPendule", "logMainSeule", "logMainIllusion")
keycol <- "condition"
valuecol <- "log ERD median"
data_final_wide$num_sujet <- factor(data_final_wide$num_sujet)

dataERD_long<-gather_(data_final_wide, keycol, valuecol, gathercols)

#rename correctely
# Rename factor names from "logPendule"to "pendule" and "second"
dataERD_long$condition<-as.factor(dataERD_long$condition)
levels(dataERD_long$condition)[levels(dataERD_long$condition)=="logPendule"] <- "pendule"
levels(dataERD_long$condition)[levels(dataERD_long$condition)=="logMainSeule"] <- "main"
levels(dataERD_long$condition)[levels(dataERD_long$condition)=="logMainIllusion"] <- "mainIllusion"

# Sort by subject first, then by condition
#data_long <- data_long[order(data_long$subject, data_long$condition), ]
#data_long

# on peut essayer pivot_longer
#il faut specifier les colonnes a rassembler
# newData <- data_final_wide %>%
#   pivot_longer(
#     cols = -c(num_sujet,age,genre,freq_vib_illusion,a_euFBvibration_mainIllusion,nbDeFBVibration,'2FB_testes','3FBtestes',seuilERD), names_to = c("Type", "Measure"),
#     names_sep = "[^[:alnum:]]+",
#     values_drop_na = FALSE)
# newData

#on peut dire celle qu'on ne veut pas rassembler avec le -

#essayons col par col deja
 colonnesRassemblees<-c("logERDmedian","agencySelfMoy","AgencyOtherMoy","AmplitudeMvtMoy",
                        "nbCyclesFB","aEuFBvisuel","nbEssaisFB","medianBeta","EmbodimentMoy","SuccessMoy",
                        "DifficultyMoy","SatisfactionMoy","easinessFB","learnabilityFB")
newData <- data_final_wide %>%
  pivot_longer(
    cols = logERD, names_to = c("condition"), values_to = "logERDmedian")
newData$condition<-as.factor(newData$condition)
levels(newData$condition)[levels(newData$condition)=="logPendule"] <- "pendule"
levels(newData$condition)[levels(newData$condition)=="logMainSeule"] <- "main"
levels(newData$condition)[levels(newData$condition)=="logMainIllusion"] <- "mainIllusion"
utils::View(newData)
newDataERD<-newData
utils::View(newDataERD)

newData <- newData %>%
  pivot_longer(
    cols =agencySelf, names_to = c("condition"), values_to = "agencySelfMoy",
    names_repair="minimal")#minimal allows duplicate
    
newData$condition<-as.factor(newData$condition)
levels(newData$condition)[levels(newData$condition)==agencySelf[1]] <- "pendule"
levels(newData$condition)[levels(newData$condition)==agencySelf[2]] <- "main"
levels(newData$condition)[levels(newData$condition)==agencySelf[3]] <- "mainIllusion"
utils::View(newData)
#pour supprimer la colonne # , -1
newDataERDAgency<-newData





# #try to iterate over this
colonnesRassemblees<-c("logERDmedian","agencySelfMoy","AgencyOtherMoy","AmplitudeMvtMoy",
                       "nbCyclesFB","aEuFBvisuel","nbEssaisFB","medianBeta","SuccessMoy",#"EmbodimentMoy"
                       "DifficultyMoy","SatisfactionMoy","easinessFB","learnabilityFB"
                      )

logERD <- c("logPendule","logMainSeule","logMainIllusion")
agencySelf <- c("agencySelfMoy_pendule","agencySelfMoy_mainSeule","agencySelfMoy_mainIllusion")
agencyOther <- c("agencyOtherMoy_pendule","agencyOtherMoy_mainSeule","agencyOtherMoy_mainIllusion")
meanMvt<-c("meanMvt_pendule","meanMvt_mainSeule","meanMvt_mainIllusion")
nbCyclesFB<-c("nbTotal_FBvisuel_pendule","nbTotal_FBvisuel_mainSeule","nbTotal_FBvisuel_mainIllusion")
a_EUFBvisuel<-c("a_euFBvisuel_pendule","a_euFBvisuel_mainSeule","a_euFBvisuel_mainIllusion")
nbEssaisFB<-c("nbTotalEssais_FBvisuel_pendule","nbTotalEssais_FBvisuel_mainSeule","nbTotalEssais_FBvisuel_mainIllusion")
medianBeta<-c("medianBeta_pendule","medianBeta_mainSeule","medianBeta_mainIllusion")
embodiment<-c("embodimentMoy_mainSeule","embodimentMoy_mainIllusion")#2 niveaux
success<-c("successMoy_pendule","successMoy_mainSeule","successMoy_mainIllusion")
difficulty<-c("difficultyMoy_pendule","difficultyMoy_mainSeule","difficultyMoy_mainIllusion")
satisfaction<-c("satisfactionMoy_pendule","satisfactionMoy_mainSeule","satisfactionMoy_mainIllusion")
easinessFB<-c("pendule_easiness","mainSeule_easiness","mainIllusion_easiness")
learnabilityFB<-c("pendule_learnability","mainSeule_learnability","mainIllusion_learnability")
ListeColonnesA_rassembler <- list(logERD,agencySelf,agencyOther,meanMvt,nbCyclesFB,a_EUFBvisuel,
               nbEssaisFB,medianBeta,success,difficulty,satisfaction,
               easinessFB,learnabilityFB  )#embodiment

nbValeurs<-13#length(colonnesRassemblees)
data_long<-data_final_wide
for (i in 0:12){
  print(i)
  #paste("condition",i)
  colARassembler<-ListeColonnesA_rassembler[[i+1]]
  print(colARassembler)
  nouvelleCol<-colonnesRassemblees[i:i+1]
  print(nouvelleCol)
  data_long <- data_long %>%
    pivot_longer(
      cols = colARassembler, names_to = c("condition"), values_to = nouvelleCol,names_repair="minimal")
  #print(data_long$condition)

#change col value
  data_long$condition<-as.factor(data_long$condition)

}
utils::View(data_long)
#ça marche mais en fait ça fait 3 millions de lignes donc c'est pas trop viable


#on essaie de le faire une a la fois, sauver dans une col et recommencer
test<-data.frame(matrix(NA, nrow = 69, ncol = 1))
nbValeurs<-13#length(colonnesRassemblees)
listeColonnesRassemblees<-list()
data_long<-data_final_wide
for (i in 0:12){
  print(i)
  #paste("condition",i)
  colARassembler<-ListeColonnesA_rassembler[[i+1]]
  print(colARassembler)
  nouvelleCol<-colonnesRassemblees[i:i+1]
  print(nouvelleCol)
  data_longmodif <- data_long %>%
    pivot_longer(
      cols = colARassembler, names_to = c("condition"), values_to = nouvelleCol,names_repair="minimal")
  valueColonne<-data_longmodif[nouvelleCol]
  print(valueColonne)
  append(listeColonnesRassemblees,valueColonne)
  #marchait avant que j'ajoute ça
  # data_longmodif$condition<-as.factor(data_longmodif)
  # levels(data_longmodif$condition)[levels(data_longmodif$condition)=="pendule_learnability"] <- "pendule"
  # levels(data_longmodif$condition)[levels(data_longmodif$condition)=="main_learnability"] <- "main"
  # levels(data_longmodif$condition)[levels(data_longmodif$condition)=="mainIllusion_learnability"] <- "mainIllusion"
  # 
  conditionColonne<-data_longmodif$condition
  test <- cbind(test, valueColonne)
  
}

test<-cbind(test,conditionColonne)
