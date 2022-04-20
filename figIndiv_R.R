library(readr)
library(ggplot2)
#library("plyr")
library("dplyr")
library(Polychrome)
tableauFigR <- read_csv("C:/Users/claire.dussard/OneDrive - ICM/Bureau/MNE_VS_OV/REFAIT/tableauFigR_repris.csv")
#tableauFigR$ERD<-as.numeric(tableauFigR$ERD)
tab_pendule<-tableauFigR[tableauFigR$condition=="pendule",]
tab_pendule_MNE_OV<-tab_pendule[tab_pendule$calcul!="MNE_12_15Hz",]
set.seed(935234)
P23 <- createPalette(30, c("#FF0000", "#00FF00", "#0000FF"), range = c(30, 80))
swatch(P23)
P23 <- sortByHue(P23)
P23 <- as.vector(t(matrix(P23, ncol=1)))
swatch(P23)
names(P23) <- NULL

data1<-tab_pendule_MNE_OV
t_p<-ggplot(data1,aes(x=calcul ,y=ERD,group = calcul))+
  scale_x_discrete( expand = c(0.06, 0.01),labels=levels(data1$calcul))+
  geom_jitter(data=data1,size=5,aes(color=factor(sujet)),alpha = 0.8,width = 0.01)+
  geom_line(data=data1,size=1.05,aes(group=sujet,color=factor(sujet)))+
  scale_color_manual(values = P23)+
  theme_bw()+theme_classic()
t_p

tab_main<-tableauFigR[tableauFigR$condition=="main",]
tab_main_MNE_OV<-tab_main[tab_main$calcul!="MNE_12_15Hz",]
data2<-tab_main_MNE_OV
t_m<-ggplot(data2,aes(x=calcul ,y=ERD,group = calcul))+
  scale_x_discrete( expand = c(0.06, 0.01),labels=levels(data2$calcul))+
  geom_jitter(data=data2,size=5,aes(color=factor(sujet)),alpha = 0.8,width = 0.01)+
  geom_line(data=data2,size=1.05,aes(group=sujet,color=factor(sujet)))+
  scale_color_manual(values = P23)+
  theme_bw()+theme_classic()
t_m

tab_mainI<-tableauFigR[tableauFigR$condition=="mainIllusion",]
tab_mainI_MNE_OV<-tab_mainI[tab_mainI$calcul!="MNE_12_15Hz",]
data3<-tab_mainI_MNE_OV
t_mi<-ggplot(data,aes(x=calcul ,y=ERD,group = calcul))+
  scale_x_discrete( expand = c(0.06, 0.01),labels=levels(data3$calcul))+
  geom_jitter(data=data3,size=5,aes(color=factor(sujet)),alpha = 0.8,width = 0.01)+
  geom_line(data=data3,size=1.05,aes(group=sujet,color=factor(sujet)))+
  scale_color_manual(values = P23)+
  theme_bw()+theme_classic()
t_mi

plot_grid(t_p,t_m,t_mi,n_col=3,rel_widths = c(0.2, 0.2,0.2))


figureRainCloudPlot <- function(data_ToUse,palette) #palette = P23
{
  data<-data_ToUse
  #data$condition <- factor(data_ToUse$condition , levels=c("pendule", "main", "mainIllusion"))
  #data$condition <- as.integer(factor(ANOVA_12_15Hz_C3_short$condition))
  t<-ggplot(data,aes(x=calcul ,y=ERD,group = calcul ,fill=condition))+
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
  #data$condition <- factor(data_ToUse$calcul , levels=c("pendule", "main", "mainIllusion"))
  box<-ggplot(data,aes(x=calcul,y=ERD,group = calcul,fill=calcul))+
    geom_boxplot(data=data,aes(fill=calcul),width = 0.8,  outlier.shape = NA,alpha=0.4 )+
    theme_bw()+
    theme_classic()+
    scale_x_discrete( expand = c(0.25, 0.0),labels=levels(data$condition))+
    theme(legend.position = "none",axis.line=element_blank())+theme(legend.position = "none")
  box<-box+theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
  # 
  #data$condition <- factor(data_ToUse$condition , levels=c("pendule", "main", "mainIllusion"))
  #violin + simple
  mu <- ddply(data, "calcul", summarise, grp.mean=mean(ERD))
  med <- ddply(data, "calcul", summarise, grp.median= median(ERD))
  violin<-ggplot(data, aes(x=ERD,fill = calcul)) +
    geom_density(alpha=0.4)+
    geom_vline(data=mu, aes(xintercept=grp.mean, color=calcul),
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

ListFigs <-figureRainCloudPlot(tab_pendule,P23)
finalFig<-finalFigure(ListFigs)
finalFig