library(readr)
library(ggplot2)
#library("plyr")
library("dplyr")
library(Polychrome)
ANOVA_12_15Hz_C3_short <- read_csv("//l2export/iss02.cenir/analyse/meeg/BETAPARK/code/PYTHON_SCRIPTS/M2data_analysis/analyse_M2/data/Jasp_anova/ANOVA_12_15Hz_C3_short.csv")

#raincloud plot

data<-ANOVA_12_15Hz_C3_short
#order conditions
data$condition <- factor(ANOVA_12_15Hz_C3_short$condition , levels=c("pendule", "main", "mainIllusion"))
data$condition <- as.integer(factor(ANOVA_12_15Hz_C3_short$condition))

#choose the colors

set.seed(935234)
P23 <- createPalette(30, c("#FF0000", "#00FF00", "#0000FF"), range = c(30, 80))
swatch(P23)
P23 <- sortByHue(P23)
P23 <- as.vector(t(matrix(P23, ncol=1)))
swatch(P23)
names(P23) <- NULL
# #alphabet
# library(pals)
# scale_fill_manual(values=as.vector(alphabet(26)))
# #glasbey
# scale_fill_manual(values=as.vector(pal.bands(glasbey(26))))


# Code to plot Raincloud and boxplot ,fill=condition
t<-ggplot(data,aes(x=condition,y=ERD,group = condition,fill=condition))+
  scale_x_discrete( expand = c(0.03, 0.01),labels=levels(data$condition))+
  geom_jitter(data=data,size=5,aes(color=factor(sujet)),alpha = 0.8,width = 0.01)+
  guides(fill = "none",col="none")+
  #theme(legend.position = "none")+
  geom_line(data=data,size=1.05,aes(group=sujet,color=factor(sujet)))+
  #geom_boxplot( width = .15,  outlier.shape = NA,alpha=0.0,position =position_nudge(x = 0, y = 0) )
  #scale_color_manual(values=glasbey(23))+
  scale_color_manual(values = P23)+
                      theme_bw()+theme_classic()
t

#boxplot
box<-ggplot(data,aes(x=condition,y=ERD,group = condition,fill=condition))+
  geom_boxplot(data=data,aes(fill=condition),width = 0.8,  outlier.shape = NA,alpha=0.4 )+
  theme_bw()+
  theme_classic()+
  scale_x_discrete( expand = c(0.25, 0.0),labels=levels(data$condition))+
  theme(legend.position = "none",axis.line=element_blank())+theme(legend.position = "none")
box<-box+theme(axis.title.y = element_blank(),axis.title.x = element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
box
  
data$condition <- factor(ANOVA_12_15Hz_C3_short$condition , levels=c("pendule", "main", "mainIllusion"))
#violin<-ggplot(data,aes(x=condition,y=ERD,group = condition,fill=condition))+ geom_flat_violin()#position =position_nudge(x = 0.0, y = 0))
#violin<-violin+ coord_cartesian(ylim = c(min(data$ERD), max(data$ERD)))#coord_flip()

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
#+theme(legend.position = "none")
#taille egale
plot_grid(t, violin, labels=c("A", "B"), ncol = 2, nrow = 1)
#specifier les tailles
ggdraw() +
  draw_plot(t,0, 0, 0.73, 1) +
  draw_plot(violin, 0.73, 0, .3, 1)+
  draw_plot_label(c("Individual points", "Distribution"), c(0, 0.7), c(0.98,0.98), size = 12)

#avec le boxplot a cote
ggdraw() +
  draw_plot(t,0, 0, 0.73, 1) +
  draw_plot(box, 0.73, 0, .3, 1)+
  draw_plot(violin, 0.83, 0, .3, 1)+
  draw_plot_label(c("Individual points","Boxplot", "Distribution"), c(0,0.73, 0.82), c(0.98,0.98,0.98), size = 12)
#FINAL
#plot_grid(t, box,violin, labels = "AUTO", rel_widths = c(1, 0.2,0.2))
plot_grid(t, box,violin, labels = c('A', 'B','C'), label_size = 12, ncol = 3, rel_widths = c(0.45, 0.12,0.25))



32
