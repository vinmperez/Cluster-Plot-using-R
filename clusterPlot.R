################This is an R Script to create a clusterplot using Metabolic Models R##########################
#Vincent Perez
#02/15/2019
#===================================================================================================#
###Move in to the correct directory and set your working directory

library(ggplot2)
library(grid)
setwd("C:/Users/vincent/Desktop/")

data=read.csv("clusterPlot.csv", header=TRUE)
names2<-as.vector(data[,1])
valuesA<-as.vector(data[,2])
valuesB<-as.vector(data[,3])
var.names<-names2
var.order<-seq(1:71)
values.a<-valuesA
values.b<-valuesB

#var.names <- c("Pyrimidine Metabolism", "Nucleotide Metabolism", "Oxidative Phosphorylation", "Carnitine Shuttle (cytosolic)", 
               #"Carnitine Shuttle (ER)", "Nicotinate and Nicotinamide Metabolism", "Transport, extracellular", 
               #"Transport, nuclear", "Fatty Acid Activation")
#var.order = seq(1:9)
#values.a <- c(-1040.77, -6076.51, 2482.99, 1253.21, -8000.00, 2021.60, 
              #-98877.1, -14409.20, 2347.09)
#values.b <- c(-1026.99, -1286.17, 2000.00, 9.45, -8000.00, 3010.80, 
              #-107392, 14437.30, 50.54)
#values.c <- rep(0, 9)
group.names <- c("GSM Control Liver", "GSM Fatp2-/- Liver")


###(2) Create df1: a plotting data frame in the format required for ggplot2

df1.a <- data.frame(matrix(c(rep(group.names[1], 71), var.names), nrow = 71, ncol = 2), 
                    var.order = var.order, value = values.a)
df1.b <- data.frame(matrix(c(rep(group.names[2], 71), var.names), nrow = 71, ncol = 2), 
                    var.order = var.order, value = values.b)
#df1.c <- data.frame(matrix(c(rep(group.names[3], 9), var.names), nrow = 9, ncol = 2), 
                    #var.order = var.order, value = values.c)
df1 <- rbind(df1.a, df1.b) #, df1.c)
colnames(df1) <- c("group", "variable.name", "variable.order", "variable.value")
df1

###Lets make us a plot.

myang1<-seq(90, 0, length.out=18)
myang2<-seq(0, -90, length.out=18)
myang3<-seq(90, 0, length.out=18)
myang4<-seq(0, -90, length.out=18)
myang5<-c(myang1, myang2, myang3, myang4)

grid.newpage()
g<-ggplot(df1, aes(y = variable.value, x = reorder(variable.name, variable.order), 
                group = group, colour = group)) + coord_polar()  +geom_path(lineend= "butt", size=0.5) +
                theme(panel.background = element_rect(fill = "white",
                                                  colour = "white",
                                                  size = 1.0, linetype = "solid"),
                  panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                                  colour = "grey"), 
                  panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                                  colour = "grey"),
                  text = element_text(size=6),
                  axis.text.x = element_text(angle=myang5, hjust=10),
                  plot.margin = unit(c(1,1,1,1), "cm"), 
                  panel.spacing= unit(c(5,5,5,5), "cm")) +
  labs(x = NULL)
g

png(file="clusterPlot2.png",width=3300 ,height=2000,res=300)
gt <- ggplot_gtable(ggplot_build(g))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
dev.off()

###This one looks a little nicer in my opinion, but each to his/her own.

m2 <- matrix(c(values.a, values.b), nrow = 2, ncol = 71, byrow = TRUE)
group.names <- c(group.names[1:2])
df2 <- data.frame(group = group.names, m2)
colnames(df2)[2:71] <- var.names
print(df2)

source("http://pcwww.liv.ac.uk/~william/Geodemographic%20Classifiability/func%20CreateRadialPlot.r")
CreateRadialPlot(df2, plot.extent.x = 1.5, centre.y=-10000, grid.max=10000)

CreateRadialPlot(df2, plot.extent.x = 1.5, grid.min = 100, centre.y = -5000, grid.max=9000, 
                 label.centre.y = TRUE, label.gridline.min = FALSE)