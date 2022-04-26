library(readr)
library(ggplot2)
library("plyr")
library("dplyr")
library(Polychrome)
library(cowplot)
#j'ai recup les data du M2data sur le bureau
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

figureRainCloudPlot <- function(data_ToUse,varToPlot,varCondition,varSujet,palette) #palette = P23
{
  data<-data_ToUse
  #data[varCondition] <- factor(data_ToUse[condition] , levels=c("pendule", "main", "mainIllusion"))
  #data$condition <- as.integer(factor(ANOVA_12_15Hz_C3_short$condition))
  t<-ggplot(data,aes(x=get(varCondition),y=get(varToPlot),group = get(varCondition),fill=get(varCondition)))+
    scale_x_discrete( expand = c(0.06, 0.01),labels=levels(data[varCondition]))+
    geom_jitter(data=data,size=5,aes(color=factor(get(varSujet))),alpha = 0.8,width = 0.01)+
    guides(fill = "none")+#,col = "none")+# a mettre tout a droite,col="none")
    #theme(legend.position = "none")+
    geom_line(data=data,size=1.05,aes(group=get(varSujet),color=factor(get(varSujet))))+
    #geom_boxplot( width = .15,  outlier.shape = NA,alpha=0.0,position =position_nudge(x = 0, y = 0) )
    #scale_color_manual(values=glasbey(23))+
    scale_color_manual(values = P23)+
    theme_bw()+theme_classic()+
    xlab(varCondition) + ylab(varToPlot)#+ labs(fill = varSujet)
# 
#boxplot
box<-ggplot(data,aes(x=get(varCondition),y=get(varToPlot),group = get(varCondition),fill=get(varCondition)))+
  geom_boxplot(data=data,aes(fill=get(varCondition)),width = 0.7,  outlier.shape = NA,alpha=0.5 )+
  theme_bw()+
  theme_classic()+
  scale_x_discrete( expand = c(0.25, 0.0),labels=levels(data[varCondition]))+
  theme(legend.position = "none",axis.line=element_blank())+theme(legend.position = "none")#+ labs(fill = varCondition)
box<-box+theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
# 
#violin + simple
mu <- ddply(data, varCondition, summarise, grp.mean=mean(get(varToPlot)))
med <- ddply(data, varCondition, summarise, grp.median= median(get(varToPlot)))
violin<-ggplot(data, aes(x=get(varToPlot),fill = get(varCondition))) +
  geom_density(alpha=0.5,lwd = 0.7)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=get(varCondition)),
             linetype="dashed")+
  coord_flip()+#+ labs(fill = varCondition)+
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
    ListFigs$indiv + theme(legend.justification = "bottom",legend.position = c(-0.5, .0))+  labs(col="sujet")
  )
  legendConditions<- get_legend(
    ListFigs$distrib + theme(legend.justification = "top")+  labs(col="condition",fill="condition")
  )
  
  ListFigs$indiv<-ListFigs$indiv + guides(fill = "none",col = "none")
  ListFigs$distrib<-ListFigs$distrib + guides(fill = "none",col = "none")
  
  fig<-plot_grid(ListFigs$indiv, ListFigs$boxplot,ListFigs$distrib, labels = c('A', 'B','C'), label_size = 12, ncol = 3,
                 rel_widths = c(0.3, 0.1,0.1),rel_heights = c(1,1,0.2))
  
  figLegende<-plot_grid(legendConditions,legendSujets,n_col=2,rel_heights = c(1,0.5))
  figLegende
  figGlobale<-plot_grid(fig,figLegende,n_col=2,rel_widths = c(1, 0.4),rel_heights = c(1,0.0005))
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
data_OV<-data_OV[data_OV$calcul=="OV",]
ListFigs <-figureRainCloudPlot(data_OV,"ERD","condition","sujet",P23)
finalFig<-finalFigure(ListFigs)
finalFig

#anova
library(rstatix)
res.aov <- anova_test(data = ANOVA_8_30Hz_C3_short_med, dv = ERD, wid = sujet, within = condition)
get_anova_table(res.aov)

res.aov <- anova_test(data = data_OV, dv = ERD, wid = sujet, within = condition)
get_anova_table(res.aov)

varCondition<-"condition"

ListFigs <-figureRainCloudPlot(dataLongFINAL,"logERDmedian","condition","sujet",P23)
finalFig<-finalFigure(ListFigs)
finalFig

