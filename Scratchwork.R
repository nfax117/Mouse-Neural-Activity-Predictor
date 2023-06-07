
# Perform KMeans clustering

# MAY NEED TO DO THIS WITH THE COMPLETE DATAFRAME INSTEAD OF CLUSTER DATAFRAME

clusterDF <- sessionDF[, 4:14]
clusterDF <- clusterDF[, !names(clusterDF) %in% c("number_of_trials")]
clusterDF <- cbind(seq(1, nrow(clusterDF), by=1), clusterDF)
colnames(clusterDF)[1] = "id"

# Clustering data for 3 clusters
clusterDF.k3 <- clusterDF %>% kmeans(3)
clusterIndicatorVector <- clusterDF.k3$cluster

# Append the cluster indicator to the dataframes for further analysis
completeDF <- cbind(clusterIndicatorVector, completeDF)
clusterDF <- cbind(clusterIndicatorVector, clusterDF)

colnames(completeDF)[1] <- "cluster_id"
colnames(clusterDF)[1] <- "cluster_id"


####


# Plots demonstrating clusters

clusterDF %>% mutate(cluster = clusterDF.k3$cluster) %>%
  ggplot(aes(x = number_of_active_neurons, y = firing_rate, color = as.factor(cluster))) + 
  geom_point()

clusterDF %>% mutate(cluster = clusterDF.k3$cluster) %>%
  ggplot(aes(x = number_of_unique_brain_area, y = total_number_of_spks, color = as.factor(cluster))) + 
  geom_point()

clusterDF %>% mutate(cluster = clusterDF.k3$cluster) %>%
  ggplot(aes(x = total_number_of_spks, y = firing_rate, color = as.factor(cluster))) + 
  geom_point()

#####

# ASK IF I NEED THESE THREE SEPARATE DATAFRAMES

# Separate the complete dataframe into 3 separate dataframes based on cluster_id

cluster1DF <- completeDF[completeDF$cluster_id == 1,]
cluster1DF <- cluster1DF[, colSums(cluster1DF != 0, na.rm = TRUE) > 0]
cluster1DF[is.na(cluster1DF)] <- 0

cluster2DF <- completeDF[completeDF$cluster_id == 2,]
cluster2DF <- cluster2DF[, colSums(cluster2DF != 0, na.rm = TRUE) > 0]
cluster2DF[is.na(cluster2DF)] <- 0

cluster3DF <- completeDF[completeDF$cluster_id == 3,]
cluster3DF <- cluster3DF[, colSums(cluster3DF != 0, na.rm = TRUE) > 0]
cluster3DF[is.na(cluster3DF)] <- 0

# Correlation matrices for new dataframe clusters

ggcorrplot(round(cor(cluster1DF[, 5:15]), 1), hc.order = TRUE,
           type = "lower", lab =TRUE, p.mat = cor_pmat(cluster1DF[, 5:15]))

ggcorrplot(round(cor(cluster2DF[, 5:15]), 1), hc.order = TRUE,
           type = "lower", lab =TRUE, p.mat = cor_pmat(cluster2DF[, 5:15]))

ggcorrplot(round(cor(cluster3DF[, 5:15]), 1), hc.order = TRUE,
           type = "lower", lab =TRUE, p.mat = cor_pmat(cluster3DF[, 5:15]))

#####

# Figure out which variables and their weighting from the specific clusterDF to use for predictive model using PCA

cluster1DF.pca <- subset(cluster1DF, select = -c(cluster_id, session_number, trial_number, mouse_name, feedback_type)) %>% prcomp(center = TRUE, scale = TRUE)
cluster2DF.pca <- subset(cluster2DF, select = -c(cluster_id, session_number, trial_number, mouse_name, feedback_type)) %>% prcomp(center = TRUE, scale = TRUE)
cluster3DF.pca <- subset(cluster3DF, select = -c(cluster_id, session_number, trial_number, mouse_name, feedback_type)) %>% prcomp(center = TRUE, scale = TRUE)

cluster1DF.pca$rotation
summary(cluster1DF.pca)

par(mfrow=c(1,3))
plot(cluster1DF.pca, type = 'l', main = "Cluster 1 Scree Plot")
plot(cluster2DF.pca, type = 'l', main = "Cluster 2 Scree Plot")
plot(cluster3DF.pca, type = 'l', main = "Cluster 3 Scree Plot")

par(mfrow=c(1,3))
biplot(cluster1DF.pca)
biplot(cluster2DF.pca)
biplot(cluster3DF.pca)

#cluster1.pc <- cluster1DF.pca$x[, 1:8] %>% as.data.frame()

#ls(cluster1DF.pca)

