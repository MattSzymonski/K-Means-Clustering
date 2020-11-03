library(DescTools)
library(ggplot2)
library(ggpubr)

rm(list = ls())
source("CustomKMeans.R")


# --- Load data sets

db.file.MickeyMouse = "./Data/Custom/MickeyMouse.csv"
db.MickeyMouse <- read.csv(db.file.MickeyMouse)

db.file.Circles = "./Data/Custom/Circles.csv"
db.Circles <- read.csv(db.file.Circles)

db.file.Crescents = "./Data/Custom/Crescents.csv"
db.Crescents <- read.csv(db.file.Crescents)

# --- Prepare data

data <- db.MickeyMouse
cluster.number <- 3

# --- Calculate clusters

result <- CustomKMeans(data, cluster.number, 20)

# --- Plot

b <- ggplot(data, aes(x=x, y=y, color=as.factor(result$cluster))) 
b + geom_point(aes(color = as.factor(result$cluster))) + 
  stat_chull(aes(color = as.factor(result$cluster), fill = as.factor(result$cluster)), alpha = 0.1, geom = "polygon", show.legend = F) + 
  xlab("X") + ylab("Y") + labs(color = "Clusters") + theme(legend.position="right")

message("Number of iterations: ", result$iterations)
