library(readxl)
library(ggplot2)
library(dplyr)
library(writexl)

library(readxl)
library(here)
library(ggpubr)
#> here() starts at C:/Desktop
dataVisualFeedback<-read_excel("C:/Users/claire.dussard/OneDrive - ICM/Bureau/taf-claire/R/data/dataVisualFeedback_8_30_add.xlsx")
dataVisualFeedback$amp_mvt<-as.numeric(dataVisualFeedback$amp_mvt)
dataVisualFeedback$FB_int<-factor(dataVisualFeedback$FB_int)
dataVisualFeedback<-subset(dataVisualFeedback, FB_int!="3")
dataVisualFeedback$moyenne_ech<-as.numeric(dataVisualFeedback$moyenne_ech)
dataVisualFeedback$seuil_min_mvt<-as.numeric(dataVisualFeedback$seuil_min_mvt)
dataVisualFeedback$ERDBeta<-log(dataVisualFeedback$moyenne_ech/dataVisualFeedback$seuil_min_mvt)
dataVisualFeedback<-dataVisualFeedback[dataVisualFeedback$num_sujet!=1,]
dataVisualFeedback<-dataVisualFeedback[dataVisualFeedback$num_sujet!=4,]
#dataVisualFeedback$num_sujet<-as.factor(dataVisualFeedback$num_sujet)

dataGlobal_8_30<-dataVisualFeedback %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet,FB_int,essai)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(meanMvt=mean(amp_mvt),
            mvtMedian = median(amp_mvt),
            stdDev_Mvt = sd(amp_mvt),
            logmedianERD= median(ERDBeta))
se <- function(x) sd(x)/sqrt(length(x))
dataGlobal_8_30_essais<-dataVisualFeedback %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet,FB_int,essai_tot)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(meanMvt=mean(amp_mvt),
            mvtMedian = median(amp_mvt),
            stdDev_Mvt = sd(amp_mvt),
            LogmedianERD= median(ERDBeta),
            sdLogmedianERD=sd(ERDBeta),
            seLogmedianERD=se(ERDBeta))

#write.csv(dataGlobal_8_30_essais,'dataGlobal_8_30_essais.csv')

dataGlobal_8_30_cycles<-dataVisualFeedback %>%  #summary(dataGlobal$sommeMvt)
  group_by(num_sujet,FB_int,cycle)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(meanMvt=mean(amp_mvt),
            mvtMedian = median(amp_mvt),
            stdDev_Mvt = sd(amp_mvt),
            LogmedianERD= median(ERDBeta))


dataGlobal_8_30_main<-dataGlobal_8_30[dataGlobal_8_30$FB_int=="1",]

ggline(dataGlobal_8_30, x = "essai", y = "meanMvt",color = "FB_int",
       add = c("mean_se","jitter"))#MARCHE LE MIEUX

ggline(dataGlobal_8_30, x = "essai", y = "meanMvt",color = "num_sujet",
       add = c("mean_se"))#MARCHE LE MIEUX

#10 essais
ggline(dataGlobal_8_30_essais, x = "essai_tot", y = "meanMvt",color = "FB_int",
       add = c("mean_se","violin"))#MARCHE LE MIEUX

ggline(dataGlobal_8_30_essais, x = "essai_tot", y = "meanMvt",color = "FB_int",
       add = c("mean_se","jitter"))#MARCHE LE MIEUX

dataGlobal_8_30_essais_SujetClass<-dataGlobal_8_30_essais
dataGlobal_8_30_essais_SujetClass$num_sujet<-as.factor(dataGlobal_8_30_essais_SujetClass$num_sujet)

dataGlobal_8_30_essais_SujetClass_main<-dataGlobal_8_30_essais_SujetClass[dataGlobal_8_30_essais_SujetClass$FB_int==1,]
ggline(dataGlobal_8_30_essais_SujetClass_main, x = "essai_tot", y = "meanMvt",color = "num_sujet",
       add = c("mean_se"))#MARCHE LE MIEUX

dataGlobal_8_30_essais_SujetClass_pendule<-dataGlobal_8_30_essais_SujetClass[dataGlobal_8_30_essais_SujetClass$FB_int==2,]
ggline(dataGlobal_8_30_essais_SujetClass_pendule, x = "essai_tot", y = "meanMvt",color = "num_sujet",
       add = c("mean_se"))#MARCHE LE MIEUX

#MAIN ILLUSION
dataGlobal_8_30_essais_SujetClass_mainIllusion<-dataGlobal_8_30_essais_SujetClass[dataGlobal_8_30_essais_SujetClass$FB_int==4,]
ggline(dataGlobal_8_30_essais_SujetClass_mainIllusion, x = "essai_tot", y = "meanMvt",color = "num_sujet",
       add = c("mean_se"))#MARCHE LE MIEUX

ggline(dataGlobal_8_30_essais_SujetClass_mainIllusion, x = "essai_tot", y = "meanMvt",
       add = c("violin"))#MARCHE LE MIEUX

dataGlobal_8_30_essais$essai_tot<-as.factor(dataGlobal_8_30_essais$essai_tot)
ggplot(dataGlobal_8_30_essais, aes(essai_tot, meanMvt)) +
  geom_boxplot(aes(group=c("essai_tot","FB_int")))


ggplot(dataGlobal_8_30_essais, aes(x=essai_tot, y=meanMvt, fill=FB_int)) + 
  geom_boxplot()

ggplot(dataGlobal_8_30_essais, aes(x=essai_tot, y=meanMvt, fill=FB_int)) + 
  geom_boxplot() +
  facet_wrap(~FB_int)

ggplot(dataGlobal_8_30_essais, aes(x=essai_tot, y=meanMvt, fill=FB_int)) + 
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.2)+
  facet_wrap(~FB_int)


ggerrorplot(dataGlobal_8_30_essais, x = "essai_tot", y = "meanMvt", 
            desc_stat = "mean_se",
            error.plot = "errorbar",            # Change error plot type
            add = "mean"                        # Add mean points
)+ facet_wrap(~FB_int)

ggerrorplot(dataGlobal_8_30_essais, x = "essai_tot", y = "meanMvt", 
            desc_stat = "mean_se",
            error.plot = "errorbar",            # Change error plot type
            add = "median"                        # Add mean points
)+ facet_wrap(~FB_int)



#=========== ca commence a etre ok
dataGlobal_8_30_essais_mean<-dataVisualFeedback %>%  #summary(dataGlobal$sommeMvt)
  group_by(FB_int,essai_tot)%>% #ajouter le run en remplacant les essais par  6 7 8 9 10
  summarise(meanMvt=mean(amp_mvt),
            mvtMedian = median(amp_mvt),
            stdDev_Mvt = sd(amp_mvt),
            meanMedianERD = mean(medianERD))

ggline(dataGlobal_8_30_essais, x = "essai_tot", y = "meanMvt", add =c("mean_se"),facet.by="FB_int",xlab="essai",ylab="amplitude de mouvement moyenne")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")



ggline(dataGlobal_8_30_essais, x = "essai_tot", y = "meanMvt", add = "mean",facet.by="FB_int",xlab="essai",ylab="amplitude de mouvement moyenne")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")




#====== par perf=====

ggline(dataGlobal_8_30_essais, x = "essai_tot", y = "LogmedianERD", add = "mean_se",facet.by="FB_int")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")
ggline(dataGlobal_8_30_essais, x = "essai_tot", y = "LogmedianERD", add = "mean",facet.by="FB_int")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")

#cycles
ggline(dataGlobal_8_30_cycles, x = "cycle", y = "LogmedianERD", add = "mean",facet.by="FB_int")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")
ggline(dataGlobal_8_30_cycles, x = "cycle", y = "LogmedianERD", add = "mean_se",facet.by="FB_int")+facet_grid(~factor(FB_int, levels=c('2','1','4')))#,panel.labs=list(FB_int = c("main","pendule","mainIllusion")),xlab="essai",ylab="amplitude de mouvement mediane")


#classe par sujet et par type
cmin=-1.2
cmax= 1.3
by = 0.5
#amp_quart = (cmax-cmin)/4
#quantile(dataGlobal_8_30_essais$LogmedianERD,probs = seq(0, 1, 0.1))

QuatreSujets<-dataGlobal_8_30_essais[dataGlobal_8_30_essais$num_sujet<6,]
QuatreSujets_2<-dataGlobal_8_30_essais[dataGlobal_8_30_essais$num_sujet>5&dataGlobal_8_30_essais$num_sujet<10,]
QuatreSujets_3<-dataGlobal_8_30_essais[dataGlobal_8_30_essais$num_sujet>9&dataGlobal_8_30_essais$num_sujet<14,]
QuatreSujets_4<-dataGlobal_8_30_essais[dataGlobal_8_30_essais$num_sujet>13&dataGlobal_8_30_essais$num_sujet<18,]
QuatreSujets_5<-dataGlobal_8_30_essais[dataGlobal_8_30_essais$num_sujet>17&dataGlobal_8_30_essais$num_sujet<22,]
QuatreSujets_6<-dataGlobal_8_30_essais[dataGlobal_8_30_essais$num_sujet>21&dataGlobal_8_30_essais$num_sujet<25,]
                                                                                                                                                                                                                          #10%percentile=-.54;30%percentile=-0.19;50% = 0.04;70% = 0.27;90% = 0.83
ggline(QuatreSujets, x = "essai_tot", y = "LogmedianERD",add = "median")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax),breaks = seq(cmin,cmax, by = by))+annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))# breaks = c(cmin,-0.54,0.0,0.27,0.83,cmax))+annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))#+geom_hline(0, linetype="dashed")#seq(cmin,cmax, by = by))
ggline(QuatreSujets_2, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by))+annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))
ggline(QuatreSujets_3, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by))+annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))
ggline(QuatreSujets_4, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by)) +annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))
ggline(QuatreSujets_5, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by)) +annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))
ggline(QuatreSujets_6, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 3)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by)) +annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))

#avec erreur bar
ggline(QuatreSujets, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax),breaks = seq(cmin,cmax, by = by))+annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))+geom_errorbar(aes(ymin=LogmedianERD-sdLogmedianERD, ymax=LogmedianERD+sdLogmedianERD), width=.2,position=position_dodge(.9)) # breaks = c(cmin,-0.54,0.0,0.27,0.83,cmax))+annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))#+geom_hline(0, linetype="dashed")#seq(cmin,cmax, by = by))
ggline(QuatreSujets_2, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by))+annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))+geom_errorbar(aes(ymin=LogmedianERD-sdLogmedianERD, ymax=LogmedianERD+sdLogmedianERD), width=.2,position=position_dodge(.9)) 
ggline(QuatreSujets_3, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by))+annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))+geom_errorbar(aes(ymin=LogmedianERD-sdLogmedianERD, ymax=LogmedianERD+sdLogmedianERD), width=.2,position=position_dodge(.9)) 
ggline(QuatreSujets_4, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by)) +annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))+geom_errorbar(aes(ymin=LogmedianERD-sdLogmedianERD, ymax=LogmedianERD+sdLogmedianERD), width=.2,position=position_dodge(.9)) 
ggline(QuatreSujets_5, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 4)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by)) +annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))+geom_errorbar(aes(ymin=LogmedianERD-sdLogmedianERD, ymax=LogmedianERD+sdLogmedianERD), width=.2,position=position_dodge(.9)) 
ggline(QuatreSujets_6, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 3)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by)) +annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))+geom_errorbar(aes(ymin=LogmedianERD-sdLogmedianERD, ymax=LogmedianERD+sdLogmedianERD), width=.2,position=position_dodge(.9)) 

data12Sujets<-dataGlobal_8_30_essais[dataGlobal_8_30_essais$num_sujet<13,]
cmax = 1.4
GlobalPlot<-ggline(dataGlobal_8_30_essais, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 23)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by)) +annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))+geom_errorbar(aes(ymin=LogmedianERD-sdLogmedianERD, ymax=LogmedianERD+sdLogmedianERD), width=.2,position=position_dodge(.9)) 
myfile <- tempfile()                   # portable across OSs
pdf(file=myfile, height=100, width=20)  # 20x20 inches, adjust at will
ggline(dataGlobal_8_30_essais, x = "essai_tot", y = "LogmedianERD")+facet_wrap(~num_sujet + factor(FB_int,levels=c(2,1,4),labels=c("pendule","main","mainI")), nrow = 23)+ scale_y_continuous(limits = c(cmin,cmax), breaks = seq(cmin,cmax, by = by)) +annotate(geom='line', linetype=2, x=c(1,10),y=c(0,0))+geom_errorbar(aes(ymin=LogmedianERD-sdLogmedianERD, ymax=LogmedianERD+sdLogmedianERD), width=.2,position=position_dodge(.9)) 
# or print(....) for lattice + ggplot2
dev.off()                              # finalize and close file
cat("Look at", myfile, "\n")