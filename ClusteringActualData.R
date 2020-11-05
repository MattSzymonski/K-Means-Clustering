library(DescTools)
library(ggplot2)
library(ggpubr)
library(tidyverse)

rm(list = ls())
source("CustomKMeans.R")

# --- Load data sets

db.file.PWT = "./Data/PWT/pwt91.csv"
db.PWT <- read.csv(db.file.PWT)
db.PWT <- db.PWT[c("country", "year", "pop", "rgdpna")]

db.file.Swiid = "./Data/Swiid/swiid8_3_summary.csv"
db.Swiid <- read.csv(db.file.Swiid)
db.Swiid <- db.Swiid[c("country", "year", "gini_mkt", "gini_disp")]

db.needed <- merge(db.PWT, db.Swiid, by = c("country", "year")) # Merge two databases

db.needed.prepared <- data.frame()
countries <- c("United States", "Mexico", "Chile", "Turkey", "Germany", "Poland", "Czech Republic", "Sweden")
for(i in 1:length(countries)) {
  db.country.data <- db.needed[db.needed$country == countries[i],] # Get rows for only one country
  db.country.data <- db.country.data[complete.cases(db.country.data), ] # Remove rows with NA
  
  time.range <- c(min(db.country.data$year), max(db.country.data$year))
  
  growth.rate.factor <- ts(log(db.country.data$rgdpna/db.country.data$pop), start=c(time.range[1]), end=c(time.range[2]), frequency=1)
  growth.rate <- as.numeric(-100 * ((growth.rate.factor) -  stats::lag(growth.rate.factor)))
  
  gini.mkt.rate.factor <- ts(log(db.country.data$gini_mkt), start=c(time.range[1]), end=c(time.range[2]), frequency=1)
  gini.mkt.rate <- as.numeric(-100 * (gini.mkt.rate.factor -  stats::lag(gini.mkt.rate.factor)))

  gini.disp.rate.factor <- ts(log(db.country.data$gini_disp), start=c(time.range[1]), end=c(time.range[2]), frequency=1)
  gini.disp.rate <- as.numeric(-100 * (gini.disp.rate.factor -  stats::lag(gini.disp.rate.factor)))

  db.country.data <- db.country.data[-1,] # Remove first row
  db.country.data <- cbind(db.country.data, growth.rate) # Add column
  db.country.data <- cbind(db.country.data, gini.mkt.rate) # Add column
  db.country.data <- cbind(db.country.data, gini.disp.rate) # Add column
  db.needed.prepared <- rbind(db.needed.prepared, db.country.data) # Add country data to common dataframe
}

# --- Prepare data

data <- db.needed.prepared
cluster.number <- 5

x <- data$growth.rate
y <- data$gini.disp.rate 
color <- data$country
title <- ""
x.label <- "Growth Rate"
y.label <- "Gini Displacement Rate"

# --- Plot

ggplot() + 
  geom_point(data=data, aes(x=x, y=y, color=country)) + 
  labs(color = "Countries", title=title, y=y.label, x=x.label) + theme(legend.position="right")

# --- Calculate clusters

data <- data[c("growth.rate", "gini.disp.rate", "country")]
result <- CustomKMeans(data, cluster.number, 3)

# --- Plot
p <- ggplot() + 
  geom_point(data=data, aes(x=x, y=y, color=country)) +
  geom_point(data=as.data.frame(result$centers), aes(x=result$centers[,1], y=result$centers[,2]), size=5, shape=8) +
  stat_chull(data=data, aes(x=x, y=y, fill = as.factor(result$cluster)), alpha = 0.1, geom = "polygon") + 
  labs(fill = "Clusters", color = "Countries", title=title, y=y.label, x=x.label) + theme(legend.position="right") 

#ggsave(p, file=paste0("Plots/Synthesized_", i, "_clusters_", cluster.number,".png"), width = 15, height = 13, units = "cm")

print(p)

message("Number of iterations: ", result$iterations)



















