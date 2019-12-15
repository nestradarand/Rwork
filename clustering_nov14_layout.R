#######################################
#                                     #
#             Clustering              #
#                                     #
#              11/17/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
# 1. What is unsupervised learning?
# 2. Examples of unsupervised learning
# 3. K-means clustering algorithm
# 4. hierarchical clustering algorithm
# 5. Constructing dissimilarity distance matrices
# 6. How to pick optimal clusters k
# 7. Estimating k-means clustering using k-means
# 8. Plotting clusters using clusplot()
# 9. Going from clusters assigmments to conceptual name 
#######################################

#------------------------------------------------
### Dissimilarity Matrix
#------------------------------------------------
DF <- data.frame(X1 = c(1,2,1,3,4),
                 X2 = c(1,3,1.5,4,4.5))
###gives the matrix
dist(DF)
dist(DF,method = "manhattan")


###also gives the matrix based on the type of distance to be calculated
library(cluster)
mat <-  daisy(DF, metric = "manhattan")



#------------------------------------------------
### Customer Segmentation
#------------------------------------------------
setwd("C:\\Users\\noahe\\Desktop\\310_mine")
customers_df <-  read.csv("Wholesale_customers.csv")

# https://archive.ics.uci.edu/ml/datasets/wholesale+customers
# Attribute Information:
#   
#   1) FRESH: annual spending (m.u.) on fresh products (Continuous);
# 2) MILK: annual spending (m.u.) on milk products (Continuous);
# 3) GROCERY: annual spending (m.u.)on grocery products (Continuous);
# 4) FROZEN: annual spending (m.u.)on frozen products (Continuous)
# 5) DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous)
# 6) DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous);
# 7) CHANNEL: customers' Channel - Horeca (Hotel/Restaurant/Cafe) or Retail channel (Nominal)
# 8) REGION: customers' Region Lisnon, Oporto or Other (Nominal)
# Descriptive Statistics:
#   
#   (Minimum, Maximum, Mean, Std. Deviation)
# FRESH ( 3, 112151, 12000.30, 12647.329)
# MILK (55, 73498, 5796.27, 7380.377)
# GROCERY (3, 92780, 7951.28, 9503.163)
# FROZEN (25, 60869, 3071.93, 4854.673)
# DETERGENTS_PAPER (3, 40827, 2881.49, 4767.854)
# DELICATESSEN (3, 47943, 1524.87, 2820.106)
# 
# REGION Frequency
# Lisbon 77
# Oporto 47
# Other Region 316
# Total 440
# 
# CHANNEL Frequency
# Horeca 298
# Retail 142
# Total 440

#install.packages('ggcorrplot')


# generate PCAs
#install.packages("factoextra")
library(ggplot2)
library(ggcorrplot)

ggcorrplot(round(cor(customers_df),2))
###insert type  = "lower" to only get the lower matrix of the result

library(factoextra)
library(cluster)

#------------------------------------------------
### number of clusters?
#------------------------------------------------
# elbow method

###wss is the elbow method
fviz_nbclust(customers_df,
             kmeans,
             method = "wss") +
  geom_vline(xintercept = 4,linetype = 2) +
  labs(subtitle = "Elbow Method")

# Silhouette method
##silhouette used for silhouette
fviz_nbclust(customers_df,
             kmeans,
             method = "silhouette")

# Gap Statistic method
fviz_nbclust(customers_df,
             kmeans,
             method = "gap_stat",
             nboot = 100)


#install.packages('NbClust')
library(NbClust)
nb_cl <- NbClust(customers_df,
                 diss = NULL,
                 distance = "euclidean",
                 min.nc = 2,
                 max.nc = 15,
                 method = "kmeans")
###gives the best k based on different methods
nb_cl$Best.nc[1,]
nb_cl$Best.partition

#------------------------------------------------
### K-Means
#------------------------------------------------
#nstart is how many times to start with random assignment at the beginning of the whole process
kmeans3 <- kmeans(customers_df,
                  centers = 3,
                  nstart = 25)
kmeans3$centers
###gives which cluster each customer belongs to 
kmeans3$cluster


# cluster plot

clusplot(customers_df,
         kmeans3$cluster,
         color = TRUE,
         shade = TRUE)
#------------------------------------------------
### Hierarchical Clustering
#------------------------------------------------

