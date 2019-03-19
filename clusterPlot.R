################This is an R Script to create a clusterplot using Metabolic Models R##########################
#Vincent Perez
#03/12/2019
#===================================================================================================#
### (1) Move in to the correct directory and set your working directory

library(ggplot2)
library(grid)
setwd("C:/Users/vincent/Google Drive/helikarlab/Modeling Project/Figures/clusterPlot/")

data=read.csv("clusterPlot_newTrial.csv", header=TRUE)
names2<-as.vector(data[,1])
valuesA<-as.vector(data[,2])
valuesB<-as.vector(data[,3])
var.names<-names2
var.order<-seq(1:124)
values.a<-valuesA
values.b<-valuesB
group.names <- c("GSM Control Liver", "GSM Fatp2-/- Liver")

###(2) Create df1: a plotting data frame in the format required for ggplot2

df1.a <- data.frame(matrix(c(rep(group.names[1], 124), var.names), nrow = 124, ncol = 2), 
                    var.order = var.order, value = values.a)
df1.b <- data.frame(matrix(c(rep(group.names[2], 124), var.names), nrow = 124, ncol = 2), 
                    var.order = var.order, value = values.b)
df1 <- rbind(df1.a, df1.b) 
colnames(df1) <- c("group", "Metabolic.Biosystem", "variable.order", "Sum.of.Carbon.Flux")
head(df1)

### (3) Lets make us a plot.

myang1<-seq(90, 0, length.out=31)
myang2<-seq(0, -90, length.out=31)
myang3<-seq(90, 0, length.out=31)
myang4<-seq(0, -90, length.out=31)
myang5<-c(myang1, myang2, myang3, myang4)

grid.newpage()
g<-ggplot(df1, aes(y = Sum.of.Carbon.Flux, x = reorder(Metabolic.Biosystem, variable.order), 
                group = group, colour = group)) + coord_polar()  +geom_path(lineend= "butt", size=0.5) +
                theme(panel.background = element_rect(fill = "white",colour = "white",size = 1.0, linetype = "solid"),
                  panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "grey"), 
                  panel.grid.minor = element_line(size = 0.1, linetype = 'solid', colour = "grey"),
                  text = element_text(size=6),
                  axis.text.x = element_text(angle=myang5),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  plot.margin = unit(c(2, 0,-4, 0), "cm"),
                  legend.justification='top') +
                labs(x=NULL)
g



###Turn off clipping and export the image

png(file="clusterPlot2.png",width=3300 ,height=2000,res=300)
gt <- ggplot_gtable(ggplot_build(g))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
dev.off()

#===================================================================================================#
###Done! Everything after this are my own personal notes.###
###This is an alternative that I can't quite get to work, but if you want to fiddle with the code,
###you may prefer this.

m2 <- matrix(c(values.a, values.b), nrow = 2, ncol = 71, byrow = TRUE)
group.names <- c(group.names[1:2])
df2 <- data.frame(group = group.names, m2)
colnames(df2)[2:71] <- var.names
print(df2)

source("http://pcwww.liv.ac.uk/~william/Geodemographic%20Classifiability/func%20CreateRadialPlot.r")
CreateRadialPlot(df2, plot.extent.x = 1.5, centre.y=-30000, grid.max=10000)

CreateRadialPlot(df2, plot.extent.x = 1.5, grid.min = 100, centre.y = -5000, grid.max=9000, 
                 label.centre.y = TRUE, label.gridline.min = FALSE)
