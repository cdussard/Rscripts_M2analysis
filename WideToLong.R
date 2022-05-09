library(tidyr)
#================try with our data=====================================
library(readr)
data_final_wide <- read_csv("//l2export/iss02.cenir/analyse/meeg/BETAPARK/code/R_SCRIPTS/Rscripts_M2analysis/data/data_final.csv")
data_final_wide$medianBeta_mainSeule<-as.double(data_final_wide$medianBeta_mainSeule)
data_final_wide$medianBeta_pendule<-as.double(data_final_wide$medianBeta_pendule)
# #try to iterate over this
colonnesRassemblees<-c("logERDmedian","agencySelfMoy","agencyOtherMoy","amplitudeMvtMoy",
                       "nbCyclesFB","aEuFBvisuel","nbEssaisFB","medianBeta","successMoy",#"EmbodimentMoy"
                       "difficultyMoy","satisfactionMoy","easinessFB","learnabilityFB"
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

# nbValeurs<-13#length(colonnesRassemblees)
# data_long<-data_final_wide
# for (i in 0:12){
#   print(i)
#   #paste("condition",i)
#   colARassembler<-ListeColonnesA_rassembler[[i+1]]
#   print(colARassembler)
#   nouvelleCol<-colonnesRassemblees[i:i+1]
#   print(nouvelleCol)
#   data_long <- data_long %>%
#     pivot_longer(
#       cols = colARassembler, names_to = c("condition"), values_to = nouvelleCol,names_repair="minimal")
#   #print(data_long$condition)
# 
# #change col value
#   data_long$condition<-as.factor(data_long$condition)
# 
# }
# utils::View(data_long)
#ça marche mais en fait ça fait 3 millions de lignes donc c'est pas trop viable


#on essaie de le faire une a la fois, sauver dans une col et recommencer
test<-data.frame(matrix(NA, nrow = 69, ncol = 0))
nbValeurs<-13#length(colonnesRassemblees)
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
  test <- cbind(test, valueColonne)
  #marchait avant que j'ajoute ça
  data_longmodif$condition<-as.factor(data_longmodif$condition)
  levels(data_longmodif$condition)[levels(data_longmodif$condition)=="pendule_learnability"] <- "pendule"
  levels(data_longmodif$condition)[levels(data_longmodif$condition)=="mainSeule_learnability"] <- "main"
  levels(data_longmodif$condition)[levels(data_longmodif$condition)=="mainIllusion_learnability"] <- "mainIllusion"

  condition<-data_longmodif$condition
  age<-data_longmodif$age
  sujet<-data_longmodif$num_sujet
  genre<-data_longmodif$genre
  freqVibrIllusion<-data_longmodif$freq_vib_illusion
  seuilERD<-data_longmodif$seuilERD
  }

dataLongFINAL<-cbind(test,condition)
dataLongFINAL<-cbind(dataLongFINAL,sujet,age,genre,freqVibrIllusion,seuilERD)
#test normalite data
hist(dataLongFINAL$logERDmedian)
dataPendule<-dataLongFINAL[dataLongFINAL$condition=="pendule",]
dataMain<-dataLongFINAL[dataLongFINAL$condition=="main",]
dataMainIllusion<-dataLongFINAL[dataLongFINAL$condition=="mainIllusion",]
shapiro.test(dataPendule$logERDmedian)
shapiro.test(dataMain$logERDmedian)
shapiro.test(dataMainIllusion$logERDmedian)
