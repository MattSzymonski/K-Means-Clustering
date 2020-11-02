
# --- Synthesized data sets


# --- Actual-data data sets

db.file.PWT = "./Data/PWT/pwt91.csv"
db.PWT <- read.csv(db.file.PWT)
db.PWT.needed <- db.PWT[c("country", "year", "pop", "rgdpna")]
db.PWT.needed.usa <- db.PWT.needed[db.PWT.needed$country == "United States",]

growth_rate_factor  <- ts(log(db.PWT.needed.usa$rgdpna/db.PWT.needed.usa$pop), start=c(min(db.PWT.needed.usa$year)), end=c(max(db.PWT.needed.usa$year)), frequency=1)
growth_rate <- 100 * (growth_rate_factor - lag(growth_rate_factor))
growth_rate
plot(growth_rate)


db.file.Swiid = "./Data/Swiid/swiid8_3_summary.csv"
db.Swiid <- read.csv(db.file.Swiid)
db.Swiid.needed <- db.Swiid[c("country", "year", "gini_mkt", "gini_disp")]
db.Swiid.needed.usa <- db.Swiid.needed[db.Swiid.needed$country == "United States",]

gini_mkt_rate_factor <- ts(log(db.Swiid.needed.usa$gini_mkt), start=c(min(db.Swiid.needed.usa$year)), end=c(max(db.Swiid.needed.usa$year)), frequency=1)
gini_mkt_rate <- 100 * (gini_mkt_rate_factor - lag(gini_mkt_rate_factor))
gini_mkt_rate

gini_disp_rate_factor <- ts(log(db.Swiid.needed.usa$gini_disp), start=c(min(db.Swiid.needed.usa$year)), end=c(max(db.Swiid.needed.usa$year)), frequency=1)
gini_disp_rate <- 100 * (gini_disp_rate_factor - lag(gini_disp_rate_factor))
gini_disp_rate


countries <- c("United States", "Mexico", "Chile", "Turkey", "Germany", "Poland", "Czechia", "Sweden")
for(i in 1:length(countries)) {
  countries[i]
}



library(DescTools)
rm(list = ls())
source("CustomKMeans.R")

# --- Prepare data

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
result <- CustomKMeans(data, cluster.number, 10)

# --- Plot
plot(x=1, xlab="x axis", ylab="y axis", main="my plot", ylim=c(0,10), xlim=c(0,10), type = "n")
points(data$x, data$y, pch=16, col=result$clusters)

legend.items <- vector("expression", cluster.number)
for (i in 1:cluster.number) { legend.items[[i]] <- bquote(.(i)~"(Size: "*.(result$size[i])*")") }
legend("bottomright", legend = legend.items, title="Cluster", col = c(1:cluster.number), pch = c(16, 16), box.lty = 1, bg = "white", cex = 0.75)
# text(data$x, data$y, result$clusters, cex=0.65, pos=1, col="grey") 

for(i in 1:cluster.number) { # Add centroids to the plot
  points(result$centers[i,1], result$centers[i,2], col=i, pch=8, cex=3)
  DrawCircle(result$centers[i,1], result$centers[i,2], r.in=0, r.out=result$radius[i], border=i, lty=3, lwd=1)
}
message("Number of iterations: ", result$iterations)


# df <- na.omit(df) # To remove any missing value that might be present in the data, type this:
# colnames(x) <- c("x", "y") 
