# k-means clustering algorithm based on Lloyd's approach, written Mateusz Szymonski

# ------------------------ Auxiliary Functions -------------------------

SquaredEuclideanDistance <- function(p1, p2) { 
  return(sum((p1 - p2)^2)) 
}

UpdateCentroids <- function(data, cluster.number, centroid, clusters) {
  for(i in 1:cluster.number) { 
    centroid[i,1] <- mean(data[,1][which(clusters == i)]) # Calculate mean of x value of points assigned to this centroid
    centroid[i,2] <- mean(data[,2][which(clusters == i)]) # Calculate mean of x value of points assigned to this centroid
  }
  return (centroid)
}

RecalculateClusters <- function(data, cluster.number, centroid, clusters) {
  distances <- rep(NA, cluster.number)
  for(i in 1:length(data[,1])) {
    for(j in 1:cluster.number) {
      distances[j] <- SquaredEuclideanDistance(data[i,], centroid[j,]) # Calculate distance from point to each centroid, save that distance to the vector (index represents centroid)
    }
    closest.centroid.index <- which.min(distances)
    clusters[i] <- closest.centroid.index # Assign index of closest centroid to point
  }
  return (clusters)
}

CalculateClusterRadius <- function(data, cluster.number, centroid, clusters) {
  radius <- rep(0, cluster.number)
  for(i in 1:length(data[,1])) {
    distance <- SquaredEuclideanDistance(data[i,], centroid[clusters[i],])
    if (distance > radius[clusters[i]]) {
      radius[clusters[i]] <- distance
    }
  }
  return (sqrt(radius))
}

CalculateClustersSize <- function(cluster.number, clusters) {
  size <- rep(0, cluster.number)
  for(i in 1:cluster.number) {
    size[i] <- sum(clusters == i)
  }
  return (size)
}

CalculateWithinClusterVariation <- function(data, cluster.number, centroid, clusters) {
  within.cluster.variation <- rep(0, cluster.number)
  for(i in 1:length(data[,1])) {
    within.cluster.variation[clusters[i]] <- within.cluster.variation[clusters[i]] + SquaredEuclideanDistance(data[i,], centroid[clusters[i],])
  }
  return (within.cluster.variation)
}

CalculateBetweenClusterVariation <- function(cluster.number, centroid) {
  between.cluster.variation <- 0
  for(i in 1:cluster.number) {
    for(j in 1:cluster.number) {
      if(j != i) {
        between.cluster.variation <- between.cluster.variation + SquaredEuclideanDistance(centroid[i,], centroid[j,])
      }
    }
  }
  return (between.cluster.variation)
}

# ------------------------ Algorithm (Hartigan-Wong (1979)) ------------------------

CustomKMeans <- function(data, cluster.number=3, iteration.limit=100) {
  # Data should be dataframe where first column is x value of data point,  second  column  is  y  value  of  data  point and each row is one data point
  
  data <- data[,c(1:2)]
  
  message("Calculating clusters...")
  
  # --- Randomly pick initial centroids
  
  #set.seed(2137)
  centroids.initial.x <- runif(cluster.number, min(data[,1]), max(data[,1])) # Generate random deviates of uniform distribution
  #set.seed(1337)
  centroids.initial.y <- runif(cluster.number, min(data[,2]), max(data[,2])) # Generate random deviates of uniform distribution
  
  centroid <- matrix(nrow = cluster.number, ncol=2)
  for(i in 1:cluster.number) { # Set up initial centroid coordinates
    centroid[i,] <- c(centroids.initial.x[i], centroids.initial.y[i])  
  }
  
  # --- Calculate the distances of all observations and assign cluster
  
  clusters <- clusters.new <- rep(NA, length(data[,1])) # Vector of indies of closest centroids to points (element is point, value is index of centroid)
  clusters <-  RecalculateClusters(data, cluster.number, centroid, clusters)
 
  # --- Recalculate centroids and reassign clusters (First step)
  
  centroid <- UpdateCentroids(data, cluster.number, centroid, clusters)
  clusters.new <- RecalculateClusters(data, cluster.number, centroid, clusters.new)
  
  # --- Iterate
  
  counter <- 2 # since we already have two iterations
  identifier <- sum(abs(clusters - clusters.new)) # Check if clusters changed from last iteration (points have been assigned to other centroids)
  
  repeat{
    clusters <- clusters.new
    centroid <- UpdateCentroids(data, cluster.number, centroid, clusters)
    clusters.new <- RecalculateClusters(data, cluster.number, centroid, clusters.new)
    
    identifier <- sum(abs(clusters - clusters.new))
    counter <- counter + 1
    if (identifier == 0) { break }
    if (counter == iteration.limit) { break }
  }
  
  cluster.radius <- CalculateClusterRadius(data, cluster.number, centroid, clusters.new)
  cluster.size <- CalculateClustersSize(cluster.number, clusters.new)
  within.cluster.variation <- CalculateWithinClusterVariation(data, cluster.number, centroid, clusters.new)
  between.clusters.variation <- CalculateBetweenClusterVariation(cluster.number, centroid)
  
  customKMeansResult <- list(
    clusters = clusters.new,
    centers = centroid,
    size = cluster.size,
    radius = cluster.radius,
    iterations = counter, 
    wcv = within.cluster.variation,
    twcv = sum(within.cluster.variation),
    bcv = between.clusters.variation
  )
  
  class(customKMeansResult) <- "CustomKMeansResult"
  return(customKMeansResult)
}