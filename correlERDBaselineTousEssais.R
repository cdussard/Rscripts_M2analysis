library(readxl)
library(ggplot2)
library(dplyr)
library(writexl)
library(readr)
library(readxl)
library(tidyverse)
library(here)
library(ggpubr)

dataGlobal_8_30_essais_CLEANFinal <- read_excel("C:/Users/claire.dussard/OneDrive - ICM/Bureau/taf-claire/R/data/dataGlobal_8_30_essais-CLEANFinal_scaled.xls")
dataGlobal_8_30_essais_CLEANFinal_noNa<-dataGlobal_8_30_essais_CLEANFinal %>% drop_na()
#pendule = 2 mainIllusion = 4 , mainSeule = 1
dataGlobal_8_30_essais_CLEANFinal_noNa$BaselineScaledERD<-as.numeric(dataGlobal_8_30_essais_CLEANFinal_noNa$BaselineScaledERD)

ERD_mainIllusion<-dataGlobal_8_30_essais_CLEANFinal[dataGlobal_8_30_essais_CLEANFinal$FB_int==4,]#condition 1 = main, 2 = mainI, 3 = pendule

ERD_mainIllusion<-ERD_mainIllusion %>% drop_na()#virer lignes incompletes
#mean pour virer les essais
ERD_mainIllusion<-ERD_mainIllusion %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(Log_meanBaseline=log(median(BaselineScaledERD)),
            meanERD = median(LogmedianERD))

correlMainIllusion_perfBL_mean<-ggscatter(ERD_mainIllusion, x = "Log_meanBaseline", y = "meanERD", 
                                          add = "reg.line", conf.int = TRUE, 
                                          cor.coef = TRUE, cor.method = "pearson",
                                          xlab = "power 8-30Hz baseline", ylab = "Median Log(8-30Hz ERD/ERS)")

correlMainIllusion_perfBL_mean


ERD_main<-dataGlobal_8_30_essais_CLEANFinal[dataGlobal_8_30_essais_CLEANFinal$FB_int==1,]#condition 1 = main, 2 = mainI, 3 = pendule

ERD_main<-ERD_main %>% drop_na()#virer lignes incompletes
#mean pour virer les essais
ERD_main<-ERD_main %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(Log_meanBaseline=log(median(BaselineScaledERD)),
            meanERD = median(LogmedianERD))

correlmain_perfBL_mean<-ggscatter(ERD_main, x = "Log_meanBaseline", y = "meanERD", 
                                          add = "reg.line", conf.int = TRUE, 
                                          cor.coef = TRUE, cor.method = "pearson",
                                          xlab = "power 8-30Hz baseline", ylab = "Median Log(8-30Hz ERD/ERS)")

correlmain_perfBL_mean

ERD_pendule<-dataGlobal_8_30_essais_CLEANFinal[dataGlobal_8_30_essais_CLEANFinal$FB_int==2,]
ERD_pendule<-ERD_pendule %>% drop_na()#virer lignes incompletes
#mean pour virer les essais
ERD_pendule<-ERD_pendule %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(Log_meanBaseline=log(median(BaselineScaledERD)),
            meanERD = median(LogmedianERD))

correlpendule_perfBL_mean<-ggscatter(ERD_pendule, x = "Log_meanBaseline", y = "meanERD", 
                                  add = "reg.line", conf.int = TRUE, 
                                  cor.coef = TRUE, cor.method = "pearson",
                                  xlab = "power 8-30Hz baseline", ylab = "Median Log(8-30Hz ERD/ERS)")

correlpendule_perfBL_mean

#10 essais
ERD_pendule<-dataGlobal_8_30_essais_CLEANFinal[dataGlobal_8_30_essais_CLEANFinal$FB_int==2,]
ERD_pendule<-ERD_pendule %>% drop_na()#virer lignes incompletes
#mean pour virer les essais
ERD_pendule_essais<-ERD_pendule %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet,essai_tot)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(Log_meanBaseline=log(median(BaselineScaledERD)),
            meanERD = median(LogmedianERD))

correlpendule_perfBL_mean_essais<-ggscatter(ERD_pendule_essais, x = "Log_meanBaseline", y = "meanERD", 
                                     add = "reg.line", conf.int = TRUE, 
                                     cor.coef = TRUE, cor.method = "pearson",
                                     xlab = "power 8-30Hz baseline", ylab = "Median Log(8-30Hz ERD/ERS)")

correlpendule_perfBL_mean_essais

#10 essais
ERD_main<-dataGlobal_8_30_essais_CLEANFinal[dataGlobal_8_30_essais_CLEANFinal$FB_int==1,]#condition 1 = main, 2 = mainI, 3 = pendule
ERD_main<-ERD_main %>% drop_na()#virer lignes incompletes
#mean pour virer les essais
ERD_main_essais<-ERD_main %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet,essai_tot)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(Log_meanBaseline=log(median(BaselineScaledERD)),
            meanERD = median(LogmedianERD))

correlmain_perfBL_mean_essais<-ggscatter(ERD_main_essais, x = "Log_meanBaseline", y = "meanERD", 
                                  add = "reg.line", conf.int = TRUE, 
                                  cor.coef = TRUE, cor.method = "pearson",
                                  xlab = "power 8-30Hz baseline", ylab = "Median Log(8-30Hz ERD/ERS)")

correlmain_perfBL_mean_essais

#10 essais
ERD_mainIllusion<-dataGlobal_8_30_essais_CLEANFinal[dataGlobal_8_30_essais_CLEANFinal$FB_int==4,]#condition 1 = main, 2 = mainI, 3 = pendule
ERD_mainIllusion<-ERD_mainIllusion %>% drop_na()#virer lignes incompletes
#mean pour virer les essais
ERD_mainIllusion_essais<-ERD_mainIllusion %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet,essai_tot)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(Log_meanBaseline=log(median(BaselineScaledERD)),
            meanERD = median(LogmedianERD))

correlmainIllusion_perfBL_mean_essais<-ggscatter(ERD_mainIllusion_essais, x = "Log_meanBaseline", y = "meanERD", 
                                         add = "reg.line", conf.int = TRUE, 
                                         cor.coef = TRUE, cor.method = "pearson",
                                         xlab = "power 8-30Hz baseline", ylab = "Median Log(8-30Hz ERD/ERS)")

correlmainIllusion_perfBL_mean_essais


library(ggpubr)

dataGlobal_8_30_essais_CLEANFinal_noNa$logBaselineScaledERD<-log(dataGlobal_8_30_essais_CLEANFinal_noNa$BaselineScaledERD)
dataGlobal_8_30_essais_CLEANFinal_noNa$essai_tot<-as.integer(dataGlobal_8_30_essais_CLEANFinal_noNa$essai_tot)
dataGlobal_8_30_essais_CLEANFinal_noNa$FB_int<-as.integer(dataGlobal_8_30_essais_CLEANFinal_noNa$FB_int)
ggline(dataGlobal_8_30_essais_CLEANFinal_noNa, x = "essai_tot", y = "LogmedianERD", add =c("mean_se"),facet.by="FB_int",xlab="essai",ylab="LogmedianERD")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")

ggline(dataGlobal_8_30_essais_CLEANFinal_noNa, x = "essai_tot", y = "logBaselineScaledERD", add =c("mean_se"),facet.by="FB_int",xlab="essai",ylab="logBaselineScaledERD")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")

ggplot(dataGlobal_8_30_essais_CLEANFinal_noNa, aes(x=essai_tot)) + 
  geom_line(aes(y = logBaselineScaledERD), color = "darkred") + 
  geom_line(aes(y = LogmedianERD), color="steelblue", linetype="twodash") + facet_grid(. ~ FB_int)


ggline(dataGlobal_8_30_essais_CLEANFinal_noNa, x = "essai_tot", y = "logBaselineScaledERD", add =c("mean_se"),facet.by="FB_int",xlab="essai",ylab="logBaselineScaledERD")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")

ggplot(dataGlobal_8_30_essais_CLEANFinal_noNa, aes(x=essai_tot)) + 
  geom_boxplot(aes(group=essai_tot,y=logBaselineScaledERD)) + 
  geom_boxplot(aes(group=essai_tot,y=LogmedianERD))+ facet_grid(. ~ FB_int)

#ESSAI 2
ERD_grouped<-dataGlobal_8_30_essais_CLEANFinal_noNa %>%  #summary(dataGlobal$sommeMvt)
  group_by(essai_tot,FB_int)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(Log_meanBaseline=median(logBaselineScaledERD),
            meanERD = median(LogmedianERD))

ggplot(ERD_grouped, aes(x=essai_tot)) + 
  geom_line(aes(y = Log_meanBaseline), color = "darkred") + 
  geom_line(aes(y = meanERD), color="steelblue", linetype="twodash") + facet_grid(. ~ FB_int)

ggplot(ERD_grouped, aes(x=essai_tot)) + 
  geom_line(aes(y = Log_meanBaseline+1), color = "darkred") + 
  geom_line(aes(y = meanERD), color="steelblue", linetype="twodash") + facet_grid(~factor(FB_int, levels=c('2','1','4')))
  +geom_point(data=ERD_grouped, aes(x=essai_tot, y=Log_meanBaseline+1))

#idealement il faudrait surement le laplacien en C3
dataGlobal_BLPerf_essais_mean<-dataGlobal_8_30_essais_CLEANFinal_noNa %>%  #summary(dataGlobal$sommeMvt)
  group_by(FB_int,essai_tot)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(meanMvt=mean(meanMvt),
            meanMedianERD = mean(LogmedianERD ),
            stdError_ERD=sd(LogmedianERD)/sqrt(23),
            meanBLERD = mean(BaselineScaledERD),
            n=n()
            )

#source : https://r-graphics.org/recipe-dataprep-summarize-se
dataGlobal_BLPerf_essais_mean$ci95 <- dataGlobal_BLPerf_essais_mean$stdError_ERD * qt(.975, dataGlobal_BLPerf_essais_mean$n - 1)
# #GRAPHIQUE PERF AU FIL DES ESSAIS


 #fin essai
data_mean<-dataGlobal_BLPerf_essais_mean
varCondition<-"FB_int"
varCond<-data_mean[varCondition]

data_mean[varCondition]<-varCond
data_mean$FB_int<-as.factor(data_mean$FB_int)
levels(data_mean$FB_int)<-c(2,1,4)
levels(data_mean$FB_int)<-c("handAlone","pendulum","handWithVibrations")

# ggline(data_mean, x = "essai_tot", y = "meanMedianERD", add = "mean",facet.by="FB_int")+
#   facet_grid(~factor(FB_int))+geom_errorbar(aes(ymin = meanMedianERD - ci95, ymax = meanMedianERD + ci95), alpha = 0.2)

#FIGURE 2 RESULTAT
ggline(data_mean, x = "essai_tot", y = "meanMedianERD", add = "mean",facet.by="FB_int")+
  facet_grid(~factor(FB_int,levels=c('pendulum','handAlone','handWithVibrations')))+geom_errorbar(aes(ymin = meanMedianERD - stdError_ERD, ymax = meanMedianERD + stdError_ERD), alpha = 0.2)+
  theme_bw()+theme_classic()+xlab("trial number")+ylab("8-30Hz median log(ERD/ERS")
  


ggline(data_mean, x = "essai_tot", y = "LogmedianERD", add = "mean_se",facet.by="FB_int")+
  facet_grid(~factor(FB_int, levels=c('2','1','4')))

ggline(data_mean, x = "essai_tot", y = "meanMedianERD", add = "mean",facet.by="FB_int")+
  geom_ribbon(aes(ymin = meanMedianERD - ci95, ymax = meanMedianERD + ci95) ,fill="grey70")+
  facet_grid(~factor(FB_int, levels=c('2','1','4')))

  geom_ribbon(aes(ymin = level - 1, ymax = level + 1), fill = "grey70") +


#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")
ggline(dataGlobal_BLPerf_essais_mean, x = "essai_tot", y = "meanBLERD", add = "mean",facet.by="FB_int")+
    facet_grid(~factor(FB_int, levels=c('2','1','4'))
               panel.labs=c("pendulum","handAlone","handWithVibrations"))
  +
                 xlab("trial number")+ylab("8-30Hz median log(ERD/ERS)")

ggline(dataGlobal_BLPerf_essais_mean, x = "meanMedianERD", y = "meanBLERD", add = "mean",facet.by="FB_int")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")

ggscatter(dataGlobal_BLPerf_essais_mean, x = "meanBLERD", y = "meanMedianERD", 
                                add = "reg.line", conf.int = TRUE, 
                                cor.coef = TRUE, cor.method = "pearson",
                                xlab = "Mean BL", ylab = "Mean ERD")+facet_grid(~factor(FB_int, levels=c('2','1','4')))
#points sujets
dataGlobal_BLPerf_sujet_mean<-dataGlobal_8_30_essais_CLEANFinal_noNa %>%  #summary(dataGlobal$sommeMvt)
  group_by(FB_int,num_sujet)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(meanMvt=mean(meanMvt),
            meanMedianERD = mean(LogmedianERD ),
            meanBLERD = mean(BaselineScaledERD)
  )

ggscatter(dataGlobal_BLPerf_sujet_mean, x = "meanBLERD", y = "meanMedianERD", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean BL", ylab = "Mean ERD")+facet_grid(~factor(FB_int, levels=c('2','1','4')))

#sujets & essais
dataGlobal_BLPerf_sujetEssai_mean<-dataGlobal_8_30_essais_CLEANFinal_noNa %>%  #summary(dataGlobal$sommeMvt)
  group_by(FB_int,num_sujet,essai_tot)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(meanMvt=mean(meanMvt),
            meanMedianERD = mean(LogmedianERD ),
            meanBLERD = mean(BaselineScaledERD)
  )

ggscatter(dataGlobal_BLPerf_sujetEssai_mean, x = "meanBLERD", y = "meanMedianERD", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean BL", ylab = "Mean ERD")+facet_grid(~factor(FB_int, levels=c('2','1','4')))

