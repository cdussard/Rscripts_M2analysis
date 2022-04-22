library(readr)
library(ggplot2)
library("plyr")
library("dplyr")
library(Polychrome)
library(cowplot)
ANOVA_12_15Hz_C3_short <- read_csv("//l2export/iss02.cenir/analyse/meeg/BETAPARK/code/PYTHON_SCRIPTS/M2data_analysis/analyse_M2/data/Jasp_anova/ANOVA_12_15Hz_C3_short_med.csv")

#raincloud plot

# data<-ANOVA_12_15Hz_C3_short
# #order conditions
# data$condition <- factor(ANOVA_12_15Hz_C3_short$condition , levels=c("pendule", "main", "mainIllusion"))
# data$condition <- as.integer(factor(ANOVA_12_15Hz_C3_short$condition))

#choose the colors

set.seed(935234)
P23 <- createPalette(23, c("#FF0000", "#00FF00", "#0000FF"), range = c(20, 90))
swatch(P23)
P23 <- sortByHue(P23)
P23 <- as.vector(t(matrix(P23, ncol=1)))
swatch(P23)
names(P23) <- NULL

figureRainCloudPlot <- function(data_ToUse,palette) #palette = P23
  
{
  data<-data_ToUse
  data$condition <- factor(data_ToUse$condition , levels=c("pendule", "main", "mainIllusion"))
  #data$condition <- as.integer(factor(ANOVA_12_15Hz_C3_short$condition))
  t<-ggplot(data,aes(x=condition,y=ERD,group = condition,fill=condition))+
    scale_x_discrete( expand = c(0.06, 0.01),labels=levels(data$condition))+
    geom_jitter(data=data,size=5,aes(color=factor(sujet)),alpha = 0.8,width = 0.01)+
    guides(fill = "none")+#,col = "none")+# a mettre tout a droite,col="none")
    #theme(legend.position = "none")+
    geom_line(data=data,size=1.05,aes(group=sujet,color=factor(sujet)))+
    #geom_boxplot( width = .15,  outlier.shape = NA,alpha=0.0,position =position_nudge(x = 0, y = 0) )
    #scale_color_manual(values=glasbey(23))+
    scale_color_manual(values = P23)+
    theme_bw()+theme_classic()
# 
#boxplot
data$condition <- factor(data_ToUse$condition , levels=c("pendule", "main", "mainIllusion"))
box<-ggplot(data,aes(x=condition,y=ERD,group = condition,fill=condition))+
  geom_boxplot(data=data,aes(fill=condition),width = 0.8,  outlier.shape = NA,alpha=0.4 )+
  theme_bw()+
  theme_classic()+
  scale_x_discrete( expand = c(0.25, 0.0),labels=levels(data$condition))+
  theme(legend.position = "none",axis.line=element_blank())+theme(legend.position = "none")
box<-box+theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
# 
data$condition <- factor(data_ToUse$condition , levels=c("pendule", "main", "mainIllusion"))
#violin + simple
mu <- ddply(data, "condition", summarise, grp.mean=mean(ERD))
med <- ddply(data, "condition", summarise, grp.median= median(ERD))
violin<-ggplot(data, aes(x=ERD,fill = condition)) +
  geom_density(alpha=0.4)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=condition),
             linetype="dashed")+
  coord_flip()+
  theme_bw()
violin<-violin+theme_classic()#remove box around plot
violin<-violin+theme(axis.title.y = element_blank(),axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(),axis.text.x=element_blank(),axis.title.x = element_blank(),axis.ticks.y=element_blank())
ListFigs <- list("indiv" = t, "boxplot" = box,"distrib"=violin)
return(ListFigs)
}
finalFigure<-function(listeRainCloud)
{
  ListFigs<-listeRainCloud
  legendSujets <- get_legend(
    # create some space to the left of the legend
    ListFigs$indiv + theme(legend.justification = "bottom",legend.position = c(-0.5, .0))
  )
  legendConditions<- get_legend(
    ListFigs$distrib + theme(legend.justification = "top")
  )
  
  ListFigs$indiv<-ListFigs$indiv + guides(fill = "none",col = "none")
  ListFigs$distrib<-ListFigs$distrib + guides(fill = "none",col = "none")
  
  # add the legend to the row we made earlier. Give it one-third of 
  # the width of one plot (via rel_widths).
  
  fig<-plot_grid(ListFigs$indiv, ListFigs$boxplot,ListFigs$distrib, labels = c('A', 'B','C'), label_size = 12, ncol = 3, rel_widths = c(0.45, 0.1,0.25))
  
  figLegende<-plot_grid(legendConditions,legendSujets,n_col=2)
  figLegende
  figGlobale<-plot_grid(fig,figLegende,n_col=2,rel_widths = c(1, 0.2),rel_heights = c(1,0.002))
  return(figGlobale)
}

ListFigs <-figureRainCloudPlot(ANOVA_12_15Hz_C3_short,P23)
finalFig<-finalFigure(ListFigs)
finalFig
#ajouter une legende avec les num de sujets

#meme chose avec une 8-30Hz en c3
ANOVA_8_30Hz_C3_short_med <- read_csv("//l2export/iss02.cenir/analyse/meeg/BETAPARK/code/PYTHON_SCRIPTS/M2data_analysis/analyse_M2/data/Jasp_anova/ANOVA_8_30Hz_C3_short_med.csv")

ANOVA_8_30Hz_C3_short_med_sans24<-ANOVA_8_30Hz_C3_short_med[ANOVA_8_30Hz_C3_short_med$sujet!=24,]
ListFigs <-figureRainCloudPlot(ANOVA_8_30Hz_C3_short_med,P23)
finalFig<-finalFigure(ListFigs)
finalFig


data_OV<- read_csv("C:/Users/claire.dussard/OneDrive - ICM/Bureau/MNE_VS_OV/REFAIT/plot_OV_seul/plot_OV_data.csv")
ListFigs <-figureRainCloudPlot(data_OV,P23)
finalFig<-finalFigure(ListFigs)
finalFig

#anova
library(rstatix)
res.aov <- anova_test(data = ANOVA_8_30Hz_C3_short_med, dv = ERD, wid = sujet, within = condition)
get_anova_table(res.aov)

res.aov <- anova_test(data = data_OV, dv = ERD, wid = sujet, within = condition)
get_anova_table(res.aov)
