
# Code do get brain area firing rates
# Dataframe for the average spks per trial for each session

# Function to calculate the average spks per brain area for the specific trial of a session
firing_rate_brainarea <- function(trialNum, currSession) {
  
  spkTrial = currSession$spks[[trialNum]]
  currBrainArea = currSession$brain_area
  spkCount = apply(spkTrial, 1, sum)
  spkAvg = tapply(spkCount, currBrainArea, mean)
  return(spkAvg)
  
}


# Function to find average spks per brain area for all trials in a session
trial_firing_rate_brainarea <- function(sessionNum) {
  
  trialLen = length(session[[sessionNum]]$feedback_type)
  brainareaLen = length(unique(session[[sessionNum]]$brain_area))
  
  trialBrainareaSummary = matrix(nrow = trialLen, ncol = brainareaLen + 2)
  
  for(q in 1:trialLen) {
    
    trialBrainareaSummary[q,] = c(firing_rate_brainarea(q, currSession = session[[sessionNum]]), sessionNum, q)
    
  }
  
  colnames(trialBrainareaSummary) = c(names(firing_rate_brainarea(q,currSession = session[[sessionNum]])), 'session_number', 'trial_number')
  
  trialBrainareaSummary <- as_tibble(trialBrainareaSummary)
  
  
  # Plot the spikes per area in each session against trials
  colorPalette = brewer.pal(n=brainareaLen, name = 'Paired')
  
  plot(0~1, col='white',xlim=c(0,trialLen),ylim=c(0.5,2.2), xlab="Trials",ylab="Average Spike Counts", main=paste("Spikes per Area in Session", sessionNum))
  
  for(i in 1:brainareaLen){
    lines(y=trialBrainareaSummary[[i]],x=trialBrainareaSummary$trial_number,col=colorPalette[i],lty=2,lwd=1)
    lines(smooth.spline(trialBrainareaSummary$trial_number, trialBrainareaSummary[[i]]),col=colorPalette[i],lwd=3)
  }
  
  legend("topright", 
         legend = colnames(trialBrainareaSummary)[1:brainareaLen], 
         col = colorPalette, 
         lty = 1, 
         cex = 0.8
  )
  
  
  # Plot the correlation matrix for the brain areas, see if there are relations between brain areas
  correlationMatrixPlot <- suppressWarnings(chart.Correlation(trialBrainareaSummary[, 3:brainareaLen], histogram = TRUE, pch = 7, alpha = 0.3))
  correlationMatrixPlot
  
  
  # Return the dataframe of brain areas for one session
  return(trialBrainareaSummary)
  
}

# Use above function to apply to all sessions, bind them together to make one dataframe of all session brainarea spk averages. 

brainarea_firing_rateDF <- data.frame()

for (p in 1:18) {
  
  brainarea_firing_rateDF = bind_rows(brainarea_firing_rateDF, trial_firing_rate_brainarea(p))
  
}

brainarea_firing_rateDF <- relocate(brainarea_firing_rateDF, trial_number)
brainarea_firing_rateDF <- relocate(brainarea_firing_rateDF, session_number)

#head(brainarea_firing_rateDF)
#datatable(brainarea_firing_rateDF)

# In the correlation matrices:
#The distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)

############

# Scatterplot of mice brainarea firing rate by trial per mouse

tempBrainAreaPerMouse <- completeDF[completeDF$mouse_name == mouseNameVector[4], ]
tempBrainAreaPerMouse <- tempBrainAreaPerMouse[,colSums(is.na(tempBrainAreaPerMouse))<nrow(tempBrainAreaPerMouse)]
tempBrainAreaPerMouse <- tempBrainAreaPerMouse[, 14:length(colnames(tempBrainAreaPerMouse))]
tempBrainAreaPerMouse[is.na(tempBrainAreaPerMouse)] <- 0

ggplot(melt(tempBrainAreaPerMouse), aes(x = variable, y = value)) + geom_boxplot(outlier.color="red", outlier.shape=8, outlier.size=2, alpha = 0.2) + xlab("Brain Areas") + ylab("Firing Rate") + ggtitle(paste("Mouse", mouseNameVector[4], "Box Plots of Brain Area Firing Rates"))



# Scatterplot for Mice Brain areas on feedback type per mouse

for (i in 1:18) {
  
  tempBrainAreaDF <- completeDF[completeDF$session_number == i, colSums(is.na(completeDF[completeDF$session_number == i,])) == 0]
  tempBrainAreaDF <- melt(tempBrainAreaDF, id.vars = "feedback_type", measure.vars = colnames(tempBrainAreaDF)[14:length(colnames(tempBrainAreaDF))])
  
  
  tempPlot <- ggplot(tempBrainAreaDF) + geom_boxplot(aes(x = as.factor(feedback_type), y = value, color = variable), outlier.color="red", outlier.shape=8, outlier.size=2, alpha = 0.2) + xlab("Feedback Type") + ylab("Firing Rate") + ggtitle(paste("Session", i, "Box Plot of Brain Area Firing Rate on Feedback Type")) + guides(color = guide_legend(title = "Brain Areas"))
  
  print(tempPlot)
  
}


############


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


#####################


# Predictive model using Tree-Based Method

treePredictedModel = rpart(formula = as.factor(feedback_type) ~ cluster_id + contrast_left + contrast_right + total_number_of_neurons + number_of_unique_brain_area + number_of_trials + contrast_difference + number_of_active_neurons + total_number_of_spks + proportion_of_active_neurons + firing_rate, data = sessionDF_train)

predictedFeedbackType.tree = predict(treePredictedModel, newdata = sessionDF_test, type = 'class')

# Confusion Matrix
treePredictedModel.cm = table(sessionDF_test$feedback_type, predictedFeedbackType.tree)
treePredictedModel.cm <- confusionMatrix(treePredictedModel.cm)
treePredictedModel.cm

paste("Accuracy is: ", treePredictedModel.cm$overall['Accuracy'])

####################

# Predictive Model using Tree-Based Method with Pruning
treePredictedModel2.cv <- cv.tree(treePredictedModel2);

best_size = treePredictedModel2.cv$size[which.min(treePredictedModel2.cv$dev)];

treePredictedModel2.pruned = prune.tree(treePredictedModel2, best = best_size)

predictedFeedbackType.tree2pruned <- predict(treePredictedModel2.pruned, newdata = sessionDF_test, type = 'class')

treePredictedModel2pruned.cm <- table(sessionDF_test$feedback_type, predictedFeedbackType.tree2)
treePredictedModel2pruned.cm <- confusionMatrix(treePredictedModel2pruned.cm)
treePredictedModel2pruned.cm

################### EXTRA


dat <- session[[1]]

#session[[1]]$brain_area

unique(dat$brain_area)

i_trial = 12;
i_session = 1;

dim(session[[i_session]]$spks[[i_trial]])

length(session[[i_session]]$feedback_type)

apply(session[[i_session]]$spks[[i_trial]], 2, sum)

names(session[[2]])



