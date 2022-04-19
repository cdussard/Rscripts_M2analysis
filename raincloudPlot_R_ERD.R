library(readr)
library(ggplot2)
ANOVA_12_15Hz_C3_short <- read_csv("//l2export/iss02.cenir/analyse/meeg/BETAPARK/code/PYTHON_SCRIPTS/M2data_analysis/analyse_M2/data/Jasp_anova/ANOVA_12_15Hz_C3_short.csv")


#raincloud plot

library("ggplot2")
library("plyr")
library("dplyr")

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax =x + width / 2)
            
            #temper with xmin & max to try to reverse the plot
  
          },
  
  draw_group = function(data, panel_scales, coord) {
    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x,
                      xmaxv = x + violinwidth * (xmax - x))
    
    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                     plyr::arrange(transform(data, x = xmaxv), -y))
    
    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    
    ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },
  
  draw_key = draw_key_polygon,
  
  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                    alpha = NA, linetype = "solid"),
  
  required_aes = c("x", "y")
  )

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}
#,orientation = "y"
data<-ANOVA_12_15Hz_C3_short
#order conditions
data$condition <- factor(ANOVA_12_15Hz_C3_short$condition , levels=c("pendule", "main", "mainIllusion"))
data$condition <- as.integer(factor(ANOVA_12_15Hz_C3_short$condition))

#choose the colors
library(Polychrome)
set.seed(935234)
P23 <- createPalette(30, c("#FF0000", "#00FF00", "#0000FF"), range = c(30, 80))
swatch(P23)
P23 <- sortByHue(P23)
P23 <- as.vector(t(matrix(P23, ncol=4)))
swatch(P23)
names(P23) <- NULL
#alphabet
library(pals)
scale_fill_manual(values=as.vector(alphabet(26)))
#glasbey
scale_fill_manual(values=as.vector(pal.bands(glasbey(26))))


# Code to plot Raincloud and boxplot ,fill=condition
t<-ggplot(data,aes(x=condition,y=ERD,group = condition,fill=condition))+
  scale_x_discrete( expand = c(0.05, 0.01),labels=levels(data$condition))+
  geom_jitter(alpha = 0.8,width = 0.01)+
  guides(fill = "none",col="none")+
  #theme(legend.position = "none")+
  geom_boxplot( width = .15,  outlier.shape = NA,alpha=0.0,position =position_nudge(x = 0, y = 0) )+
  geom_line(data=data,aes(group=sujet,color=factor(sujet)))+
  #scale_color_manual(values=glasbey(23))+
  scale_color_manual(values = P23)+
                      theme_bw()+theme_classic()

t
#data = ANOVA_12_15Hz_C3_short, aes(x=condition,y=ERD))


data$condition <- factor(ANOVA_12_15Hz_C3_short$condition , levels=c("pendule", "main", "mainIllusion"))
violin<-ggplot(data,aes(x=condition,y=ERD,group = condition,fill=condition))+ geom_flat_violin()#position =position_nudge(x = 0.0, y = 0))
violin<-violin+ coord_cartesian(ylim = c(min(data$ERD), max(data$ERD)))#coord_flip()
#violin + simple
mu <- ddply(data$ERD, "condition", summarise, grp.mean=mean(ERD))
med <- ddply(data$ERD, "condition", summarise, grp.median= median(ERD))
violin<-ggplot(data, aes(x=ERD,fill = condition)) +
  geom_density(alpha=0.4)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=condition),
             linetype="dashed")+
  coord_flip()+
  theme_bw()
violin<-violin+theme_classic()#remove box around plot
violin<-violin+theme(axis.title.y = element_blank())
#+theme(legend.position = "none")
#taille egale
plot_grid(t, violin, labels=c("A", "B"), ncol = 2, nrow = 1)
#specifier les tailles
ggdraw() +
  draw_plot(t,0, 0, 0.73, 1) +
  draw_plot(violin, 0.73, 0, .3, 1)+
  draw_plot_label(c("Individual points", "Distribution"), c(0, 0.7), c(0.98,0.98), size = 12)


#cowplot
#plot_grid(lp, bp, labels=c("A", "B","C"), ncol = 3, nrow = 1)

# library(chronicle)
# ANOVA_12_15Hz_C3_short$condition <- factor(ANOVA_12_15Hz_C3_short$condition , levels=c("pendule", "main", "mainIllusion"))
# make_raincloud(dt = ANOVA_12_15Hz_C3_short, value = 'ERD',groups = 'condition',adjust=1)
