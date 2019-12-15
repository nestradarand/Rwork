#######################################
#                                     #
#                PCA                  #
#                                     #
#              11/19/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - Review on clustering
#   - Hierarchical clustering
#   - Visualize hierarchical clustering
#   - Apply dimension reduction by PCA
#   - Understanding PCA factors
#   - Visualize PCA results
#######################################

library(ggplot2)
library(factoextra)


#------------------------------------------------
### Hierarchical Clustering
#------------------------------------------------

#using iris data: flowers species
data("iris")
# hierarchical clustering
###gets dissimilarity matrix
dissimilarity <- dist(iris[,c(3:4)])
clusters <- hclust(dissimilarity)
# plotting the dendogram
plot(clusters)
# cutting the tree to find clusters
####shows us where three clusters would be located
plot(clusters)
rect.hclust(clusters,k=3,border = 1:3)

###cutting the dendrogram with specified number of clusters
cluster_cut <- cutree(clusters,k=3)
table(cluster_cut,iris$Species)
# different method to find the linkage: average


iris_clusters2 <- hclust(dist(iris[,3:4]),method = 'average')
plot(iris_clusters2)
rect.hclust(iris_clusters2,3)


# plotting the dendogram with clusters
                             # plot tree
                             # add rectangle

cluster_cut2 <- cutree(iris_clusters2,3)
table(cluster_cut2,iris$Species)



# Check if clustering could effectively differentiate species
###colors shows clusters
ggplot(iris,aes(x= Petal.Length,y = Petal.Width,color = Species)) + 
  geom_point(alpha = .4,size = 3.5) +
  geom_point(col = cluster_cut2)

#------------------------------------------------
### Auto data
#------------------------------------------------
# using Auto data
library(ISLR)
data("Auto")
# scaling
####make variables similar scales
str(Auto)
Auto_scaled <- scale(Auto[,1:7]) ###gets rid of origin and name and scales the values
#------------------------------------------------
### number of clusters?
#------------------------------------------------
# elbow method
fviz_nbclust(Auto_scaled,
             kmeans,
             method="wss") +
  geom_vline(xintercept=3, linetype =2) + 
  labs(subtitle ="Elbow Method")

# Silhouette method
fviz_nbclust(Auto_scaled,
             kmeans,
             method="silhouette")

# Gap Statistic method
fviz_nbclust(Auto_scaled,
             kmeans,
             method="gap_stat",
             nboot=100)


library("NbClust")


####shows which k is used by different numbers of indices
Nb_cl <- NbClust(Auto_scaled,
                 diss = NULL,
                 distance = "euclidean",
                 min.nc = 2,
                 max.nc = 10,
                 method = "kmeans")

# try different clusters and compare the results
km2 <- kmeans(Auto_scaled,2)
km3 <- kmeans(Auto_scaled,3)
km4 <- kmeans(Auto_scaled,4)
km5 <- kmeans(Auto_scaled,5)
km6 <- kmeans(Auto_scaled,6)
km7 <- kmeans(Auto_scaled,7)



# visualize the results

p1 <- fviz_cluster(km2,data = Auto_scaled) + theme_minimal() +
  ggtitle("k=2")
p2 <- fviz_cluster(km3,data = Auto_scaled) + theme_minimal() +
  ggtitle("k=3")
p3 <- fviz_cluster(km4,data = Auto_scaled) + theme_minimal() +
  ggtitle("k=4")
p4 <- fviz_cluster(km5,data = Auto_scaled) + theme_minimal() +
  ggtitle("k=5")
p5 <- fviz_cluster(km6,data = Auto_scaled) + theme_minimal() +
  ggtitle("k=6")
p6 <- fviz_cluster(km7,data = Auto_scaled) + theme_minimal() +
  ggtitle("k=7")


library(cowplot)
plot_grid(p1,p2,p3,p4,p5,p6)



# pick the best number of clusters
###nstart uses many iterations through kmeans to remove randomization of initial assignments
finalkm <- kmeans(Auto_scaled,3,nstart = 30)
fviz_cluster(finalkm,data = Auto_scaled)
# store the cluster information in the data frame
Auto$cluster <- as.factor(finalkm$cluster)
# use ggpairs to see if the clusters are meaningful
library(GGally)
ggpairs(Auto,1:5,mapping = aes(color = cluster,alpha = .5))

#------------------------------------------------
### PCA
#------------------------------------------------
# use the auto data set
data(Auto)
# run pca model
###by default scales the data
auto_pca <- prcomp(Auto[,1:7],
                   center = TRUE,
                   scale = TRUE)
# summary
summary(auto_pca)

# how much variation each dimension captures 
fviz_screeplot(auto_pca)

# Extract the results for variables
var <- get_pca_var(auto_pca)

# Contributions of variables to PC1
fviz_contrib(auto_pca,choice = "var",axes = 1,top = 10)

# Control variable colors using their contributions to the principle axis


#install.packages("devtools")
#library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
# ggbiplot to visualize the results
ggbiplot(auto_pca,ellipse = TRUE)
# create the country variable based on the origin

# use country to group the observations

# remember the default plotting option is biplot; It is not very interesting though
#biplot(pca_auto, scale = 0)

set.seed(2019)
library(ISLR)
data(Auto)
# do the above with a sampled data (20 observations)
# sampled data
auto_sample <- Auto[sample(1:nrow(Auto),size = 20,replace = FALSE),]
# scale the data
auto_sample[,1:7] <-scale(auto_sample[,1:7])
# run PCA
pca_auto <- prcomp(auto_sample[,1:7],
                   center = TRUE,
                   scale. = TRUE)
summary(pca_auto)
# visualize with car names on it
library(ggbiplot)
ggbiplot(pca_auto)
ggbiplot(pca_auto,labels = auto_sample$name)

# more options

auto_sample$country <- ifelse(auto_sample$origin == 1,"American",
                              ifelse(auto_sample$origin == 2,"European","Japanese"))
ggbiplot(pca_auto,labels = auto_sample$name,
         grous = auto_sample$country,
         ellipse = TRUE)
# third and fourth PC

