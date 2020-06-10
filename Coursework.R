######################################################################
#
#   CS5608 Big Data Analytics
#   Coursework
#   King County Housing
#   Federico Riva
#
######################################################################
#
#   1. Install packages
#   2. Data cleaning and preparation
#   3. (Simple) outlier detection
#   4. Graphical Analysis
#   5. Principal Component Analysis
#   6. Performance evaluation
#
######################################################################
# 1. Install packages
install.packages("dplyr")
install.packages('cluster')
install.packages('neuralnet')
install.packages('tree')
install.packages('caret')
install.packages('rpart')
install.packages('ROCR')

# load the libraries
library(dplyr)
library(RgoogleMaps)
library(neuralnet)
library(tree)
library(lattice)
library(ggplot2)
library(caret)
library(ROCR)

######################################################################
# 2. Data cleaning and preparation

# import the datasets
kc_house_data <- read.csv('kc_house_data.csv')
kc_crime <- read.csv('King_County_Sheriff_s_Office_-_Incident_Dataset.csv')
kc_transport <- read.table('stops.txt', header = T, sep = ',')

################################
# count stops per lat_long pairs

double_kc_trans <- kc_transport
double_kc_trans$stop_lat <- as.numeric(substr(kc_transport$stop_lat,1,6))
double_kc_trans$stop_lon <- as.numeric(substr(kc_transport$stop_lon,1,7))

transport_map_table <- table(double_kc_trans$stop_lat,double_kc_trans$stop_lon)
transport_map <- data.frame(transport_map_table)

colnames(transport_map)[1] <- 'Latitude'
colnames(transport_map)[2] <- 'Longitude'

transport_map$Latitude <- as.numeric(levels(transport_map$Latitude))
transport_map$Longitude <- as.numeric(levels(transport_map$Longitude))

transport_map <- transport_map %>% distinct(Latitude, Longitude, .keep_all = T)

#####################################
# count the different crimes per area

kc_crime$year <- as.numeric(substr(kc_crime$incident_datetime,7,11))
crime_map_table <- table(kc_crime$zip,kc_crime$year)
crime_map <- data.frame(crime_map_table)

colnames(crime_map)[1] <- 'zipcode'
colnames(crime_map)[2] <- 'year'

crime_map$zipcode <- as.numeric(levels(crime_map$zipcode))
crime_map$year <- as.numeric(levels(crime_map$year))

#########################################
# merge the datasets with the houses data
colnames(kc_house_data)[18] <- 'Latitude'
colnames(kc_house_data)[19] <- 'Longitude'

kc_house_data$year <- as.numeric(substr(kc_house_data$date,1,4))
kc_house_data$Latitude <- as.numeric(substr(kc_house_data$Latitude,1,6))
kc_house_data$Longitude <- as.numeric(substr(kc_house_data$Longitude,1,7))

kc_house_data_merge_transport <- left_join(kc_house_data, transport_map)
colnames(kc_house_data_merge_transport)[23] <- 'pt_stops'
kc_houses_final <- left_join(kc_house_data_merge_transport, crime_map)
colnames(kc_houses_final)[24] <- 'crimes'

########################################
# remove null value and duplicates

pt_stops_na <- is.na(kc_houses_final$pt_stops)
kc_houses_final[pt_stops_na, 'pt_stops'] <- 0

crime_na <- is.na(kc_houses_final$crimes)
kc_houses_final[crime_na, 'crimes'] <- 0

########################################################
# waterfront, condition, grade must be turned to factors
kc_houses_final[,c('waterfront','condition','grade','view')] <- lapply(kc_houses_final[,c('waterfront','condition','grade','view')], as.factor)
kc_houses_final$waterfront <- factor(kc_houses_final$waterfront, labels = c(FALSE, TRUE))
kc_houses_final$condition <- factor(kc_houses_final$condition, ordered = TRUE)
kc_houses_final$grade <- factor(kc_houses_final$grade, ordered = TRUE)
kc_houses_final$view <- factor(kc_houses_final$view, ordered = TRUE)

########################################
# reorder columns and delete useless ones
kc_houses_final$date <- as.Date(substr(kc_house_data$date,1,8),format = '%Y%m%d')
kc_houses_final_clean <- kc_houses_final[,-22]

#########################################
# take into account only the seattle area
seattle_zips <- c(98101:98109,98111:98119,98121,98122,98124:98127,98129,98131:98134,98136,98139,98141,98144:98146,98154,98161,98164,98165,98170,98174,98175,98177,98178,98181,98185,98190,98191,98194,98195,98199)
seattle_houses_check <- is.element(kc_houses_final_clean$zipcode,seattle_zips)
seattle_houses <- kc_houses_final_clean[seattle_houses_check,]

sd12_zips <- c(98126,98106,98108,98136,98116,98134,98144,98118)
sd12_houses_check <- is.element(kc_houses_final_clean$zipcode,sd12_zips)
sd12_houses <- kc_houses_final_clean[sd12_houses_check,]

######################################################################
# 3. (Simple) outlier detection

relevant_cols <- c(3:8,13:15,20,21)

# inspect the various distributions using the summary statistics
sh_summaries <- apply(sd12_houses[,relevant_cols], 2, summary)
sh_summaries

# generate a boxplot of the chosen variables
opar <- par()
par(mfrow = c(2,6))
sh_boxplot <- apply(sd12_houses[,relevant_cols], 2, boxplot)
par(opar)

find_compVals <- function(x) {
  if (min(x$out) < max(x$stats)) {
    return(max(x$stats)+1)
  } else {
    return(min(x$out))
  }
}
compare_vals <- lapply(sh_boxplot,find_compVals)

# Removing outliers
outlierRemoval <- function(x) {
  for(i in 1:11) {
    if (as.numeric(x[[relevant_cols[i]]]) >= as.numeric(compare_vals[[i]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

sd12_houses_nOut_list <- apply(sd12_houses, 1, outlierRemoval)
sd12_houses_nOut <- sd12_houses[sd12_houses_nOut_list,]

#new boxplots
opar <- par()
par(mfrow = c(2,6))
sh_boxplot_nOut <- apply(sd12_houses_nOut[,relevant_cols], 2, boxplot)
par(opar)

#now everything seems more normal

######################################################################
# 2. Graphical analysis

relevant_cols_freq <- c(3:8,10:15,20:23)

# generate a histogram for each variable (and show them on the same page)
#   note: titles and x labels are set to the name of the relevant variable
opar <- par()
par(mfrow = c(4,4))
for(i in c(1:16)) {
  hist(as.numeric(sd12_houses_nOut[, relevant_cols_freq[i]]), main = names(sd12_houses_nOut)[relevant_cols_freq[i]], xlab = names(sd12_houses_nOut)[relevant_cols_freq[i]])
}
par(opar)

# generate a density plot for each variable (and show them on the same page)
#   note: kernel density estimation may have tails outside the variable range!
opar <- par()
par(mfrow = c(4,4))
for(i in c(1:16)) {
  plot(density(as.numeric(sd12_houses_nOut[, relevant_cols_freq[i]])), main = names(sd12_houses_nOut)[relevant_cols_freq[i]], xlab = names(sd12_houses_nOut)[relevant_cols_freq[i]])
}
par(opar)

######################################################################
# 3. Principal Component Analysis

sd12_houses_num <- sd12_houses_nOut
for (i in relevant_cols_freq) {
  sd12_houses_num[,i] <- as.numeric(sd12_houses_nOut[,i]) 
}

# perform PCA
#   note: variables are centered and scaled before analysis
pc_sd12_houses <- prcomp(sd12_houses_num[,relevant_cols_freq], center = T, scale. = T)

######################################################################
# 4. Visual analysis of PCA results

# calculate the proportion of exaplained variance (PEV) from the std values
pc_sd12_houses_var <- pc_sd12_houses$sdev^2
pc_sd12_houses_var
pc_sd12_houses_PEV <- pc_sd12_houses_var / sum(pc_sd12_houses_var)
pc_sd12_houses_PEV

# plot the variance per PC
#   note: this can be done using the plot function on the prcomp object
plot(pc_sd12_houses)

# plot the cumulative value of PEV for increasing number of additional PCs
#   note: add an 80% threshold line to inform the feature extraction
#     according to the plot the first 3 PCs should be selected
opar <- par()
plot(
  cumsum(pc_sd12_houses_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange'
)
abline(h = 0.8, col = 'red', lty = 'dashed')
par(opar)

# get and inspect the loadings for each PC
#   note: loadings are reported as a rotation matrix (see lecture)
pc_sd12_houses_loadings <- pc_sd12_houses$rotation
pc_sd12_houses_loadings

# plot the loadings for the first three PCs as a barplot
#   note: two vectors for colours and labels are created for convenience
#     for details on the other parameters see the help for barplot and legend
opar <- par()
colvector = rainbow(16)
labvector = c('PC1', 'PC2', 'PC3','PC4', 'PC5', 'PC6','PC7')
barplot(
  pc_sd12_houses_loadings[,c(1:7)],
  beside = T,
  yaxt = 'n',
  names.arg = labvector,
  col = colvector,
  ylim = c(-1,1),
  border = 'white',
  ylab = 'loadings'
)
axis(2, seq(-1,1,0.1))
legend(
  'top',
  bty = 'n',
  col = colvector,
  pch = 15,
  ncol = 5,
  text.width = 10,
  row.names(pc_sd12_houses_loadings)
)
par(opar)

# generate a biplot for each pair of important PCs (and show them on the same page)
#   note: the option choices is used to select the PCs - default is 1:2
opar = par()
par(mfrow = c(3,7))
for (i in c(1:7)) {
  j <- i+1
  while (j <= 7) {
    biplot(
      pc_sd12_houses,
      choices = c(i,j),
      scale = 0,
      col = c('grey40','orange')
    )
    j <- j+1
  }
}
par(opar)

######################################################################
# 2. Cluster analysis (agglomerative hierarchical and k-means)

# hierarchical clustering
#   first generate the distance matrix with euclidian distance
#     note: exclude the country attribute
dist_sd12_houses <- dist(sd12_houses_nOut[,relevant_cols_freq], method = 'euclidian')
#   then apply complete linkage
hc_sd12_houses <- hclust(dist_sd12_houses, method = 'complete')
hc_sd12_houses

# plot the associated dendrogram
plot(hc_sd12_houses, hang = -0.1, labels = sd12_houses_nOut$price)

# 'cut' the dendrogram to select one partition with 5 groups
#   note: the cutree command requires a distance cutoff h
#      or the number k of desired groups
hc_cluster_id_sd12_houses <- cutree(hc_sd12_houses, k = 3)

# k-means
k.max <- 10
wss <- sapply(1:k.max,function(k){kmeans(sd12_houses_nOut[,relevant_cols_freq], k,)$tot.withinss})
plot(1:k.max, wss,type="b", pch = 19, frame = FALSE, xlab="Number of clusters K",ylab="Total within-clusters sum of squares")

k_sd12_houses <- kmeans(sd12_houses_nOut[,relevant_cols_freq],3)
k_sd12_houses

# get the cluster id from the kmeans object
k_cluster_id_sd12_houses <- k_sd12_houses$cluster

######################################################################
# 3. Evaluation of cluster results

# silhoutte plot
# first install the package cluster

# then calculate the silhoutte score for the two cluster solutions
#   note: look at the help for silhoutte to understand the required input
sil_hc_sd12_houses <- cluster::silhouette(hc_cluster_id_sd12_houses, dist_sd12_houses)
sil_k_sd12_houses <- cluster::silhouette(k_cluster_id_sd12_houses, dist_sd12_houses)

# plot the results of the silhoutte analysis for the two cluster solutions
opar <- par()
par(mfrow = c(1,2))
plot(sil_hc_sd12_houses, col = 2:4, border = NA, main = "Silhouette plot for HC")
plot(sil_k_sd12_houses, col=2:4, border = NA, main = "Silhouette plot for k-means")
par(opar)

opar <- par()
par(pty="s", mfrow = c(1,2))
plotmap(lat = Latitude, lon = Longitude, pch=20, col = as.numeric(hc_cluster_id_sd12_houses+1), data = sd12_houses_nOut)
plotmap(lat = Latitude, lon = Longitude, pch=20, col = as.numeric(k_cluster_id_sd12_houses+1), data = sd12_houses_nOut)
par(opar)

######################################################################
# 2. Neural Network

## transform the data using a min-max function
#   note: this will make the data more suitable for use with NN
#     as the attribute values will be on a narrow interval around zero
# first define a MinMax function
set.seed(2020)

relevant_cols_nn <- c(relevant_cols,16,22,23)
MinMax <- function(x){
  if (counter %in% relevant_cols_nn) {
    y <- as.numeric(x)
    tx <- (y - min(y)) / (max(y) - min(y))
    counter <<- counter + 1
    return(tx)
  } else {
    counter <<- counter + 1
    return (x)
  }
}
# then apply the function to each column of the data set
#   note: the apply function returns a matrix
counter <- 1
sd12_houses_minmax <- apply(sd12_houses_nOut, 2, MinMax)

# the matrix needs to be 'cast' into a data frame
#   note: R has an as.data.frame function for this purpose
sd12_houses_minmax <- as.data.frame(sd12_houses_minmax, stringsAsFactors = FALSE)
sd12_houses_minmax <- type.convert(sd12_houses_minmax)
sd12_houses_minmax$date <- as.numeric(sd12_houses_minmax$date)

# create a 70/30 training/test set split

idx_rows <- sample(seq(1,3), size = nrow(sd12_houses_minmax), replace = TRUE, prob = c(.6, .2, .2))
training_sd12_houses_minmax <- sd12_houses_minmax[idx_rows == 1,]
test_sd12_houses_minmax <- sd12_houses_minmax[idx_rows == 2,]
val_sd12_houses_minmax <- sd12_houses_minmax[idx_rows == 3,]

#########################
# Neural network training
for (zip in unique(training_sd12_houses_minmax$zipcode)) {
  training_sd12_houses_minmax <- cbind(training_sd12_houses_minmax, training_sd12_houses_minmax$zipcode == zip)
  names(training_sd12_houses_minmax)[length(training_sd12_houses_minmax)] = paste("zip", zip, sep="_")
}

# define a formula for predicting strength
sd12_houses_formula <- (zip_98118 + zip_98126 + zip_98116 + zip_98106 + zip_98136 + zip_98144 + zip_98108 
                        ~ date + price + bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + sqft_above + sqft_basement + yr_built + yr_renovated + sqft_living15 + sqft_lot15 + pt_stops + crimes)

# nn with no hidden layers
sd12_houses_nn_Nohl <- neuralnet(sd12_houses_formula, data = training_sd12_houses_minmax)

# train a neural network with 5 nodes on one hidden layer
#   note: the number of layers is set with the hidden option parameter
val_results <- rep(0, times = 8)
for(i in c(1:8)) {
  sd12_houses_nn_1hl <- neuralnet(sd12_houses_formula, hidden = i, data = training_sd12_houses_minmax, stepmax = 1e7)
  val_sd12_houses_nn_1hl <- compute(sd12_houses_nn_1hl, val_sd12_houses_minmax[,-c(1,17)])
  val_weights_1hl <- val_sd12_houses_nn_1hl$net.result
  idx_1hl <- apply(val_weights_1hl, 1, which.max)
  val_1hl <- c(98118, 98126, 98116, 98106, 98136, 98144, 98108)[idx_1hl]
  val_dataframe <- data.frame(
    actual = val_sd12_houses_minmax$zipcode,
    nn_1hl = val_1hl
  )
  val_results[i] <- cor(val_dataframe[,'actual'], val_dataframe[,'nn_1hl'])
  print(c(i,':',val_results[i]))
}

# highest correlation found at 6 hidden layers
sd12_houses_nn_1hl <- neuralnet(sd12_houses_formula, hidden = 6, data = training_sd12_houses_minmax, stepmax = 1e7)

# another validation process, to see that the network trained at its best
val_sd12_houses_nn_1hl <- compute(sd12_houses_nn_1hl, val_sd12_houses_minmax[,-c(1,17)])
val_weights_1hl <- val_sd12_houses_nn_1hl$net.result
idx_1hl <- apply(val_weights_1hl, 1, which.max)
val_1hl <- c(98118, 98126, 98116, 98106, 98136, 98144, 98108)[idx_1hl]
val_dataframe <- data.frame(
  actual = val_sd12_houses_minmax$zipcode,
  nn_1hl = val_1hl
)
cor(val_dataframe[,'actual'], val_dataframe[,'nn_1hl'])

# train a neural network with 5 nodes on each of two hidden layers
# it does not work, cannot finish the computation
sd12_houses_nn_2hl <- neuralnet(sd12_houses_formula, hidden = c(10,10), data = training_sd12_houses_minmax, stepmax = 1e6, threshold = 0.1)

# plot the three neural networks and compare their structure
plot(sd12_houses_nn_Nohl)
plot(sd12_houses_nn_1hl)

######################################################################
# 4. Neural network prediction

# compute the prediction for each neural network
#   note: the strength attribute (column 9) is excluded from the test data set
pred_sd12_houses_nn_Nohl <- compute(sd12_houses_nn_Nohl, test_sd12_houses_minmax[,-c(1,17)])
pred_sd12_houses_nn_1hl <- compute(sd12_houses_nn_1hl, test_sd12_houses_minmax[,-c(1,17)])

# create a table with actual values and the three predictions
#   note: predicted values are stored as net_result attribute of the prediction object
pred_weights_Nohl <- pred_sd12_houses_nn_Nohl$net.result
idx_Nohl <- apply(pred_weights_Nohl, 1, which.max)
pred_Nohl <- c(98118, 98126, 98116, 98106, 98136, 98144, 98108)[idx_Nohl]

pred_weights_1hl <- pred_sd12_houses_nn_1hl$net.result
idx_1hl <- apply(pred_weights_1hl, 1, which.max)
pred_1hl <- c(98118, 98126, 98116, 98106, 98136, 98144, 98108)[idx_1hl]

sd12_houses_nn_results <- data.frame(
  actual = test_sd12_houses_minmax$zipcode,
  nn_Nohl = pred_Nohl,
  nn_1hl = pred_1hl
)


# calculate the correlation between actual and predicted values to identify the best predictor
cor(sd12_houses_nn_results[,'actual'], sd12_houses_nn_results[,c("nn_Nohl","nn_1hl")])

table(sd12_houses_nn_results$actual, sd12_houses_nn_results$nn_Nohl)
nn_1hl_table <- table(sd12_houses_nn_results$actual, sd12_houses_nn_results$nn_1hl)
acc_nn <- sum(diag(nn_1hl_table)) / sum(nn_1hl_table)
acc_nn

# plot actual vs predicted values for the worst (blue) and best predictor (orange)
#   note: points is used to add points on a graph
opar <- par()
plot(
  sd12_houses_nn_results$actual,
  sd12_houses_nn_results$nn_1hl,
  col = 'steelblue2',
  xlab = 'actual zipcode',
  ylab = 'predicted zipcode',
  pch = 20,
  cex = table(sd12_houses_nn_results$actual, sd12_houses_nn_results$nn_1hl)/25
)
points(
  sd12_houses_nn_results$actual,
  sd12_houses_nn_results$nn_Nohl,
  col = 'orange',
  pch = 20,
  cex = table(sd12_houses_nn_results$actual, sd12_houses_nn_results$nn_Nohl)/50
)
abline(a = 0, b = 1, col = 'red', lty = 'dashed')
legend(
  'bottomright',
  c('nn_1hl', 'nn_Nohl'),
  pch = 20,
  col = c('steelblue2', 'orange'),
  bty = 'n',
  horiz = TRUE
)
par(opar)

######################################################################
# 3. Decision tree training

# set random seed
set.seed(2018)

sd12_houses_nOut$zipcode <- as.factor(sd12_houses_nOut$zipcode)
# create a 70/30 training/test set split
n_rows <- nrow(sd12_houses_nOut)
# sample 70% (n_rows * 0.7) indices in the ranges 1:nrows
training_idx_tree <- sample(n_rows, n_rows * 0.7)
# filter the data frame with the training indices (and the complement)
training_sd12_houses_tree <- sd12_houses_nOut[training_idx_tree,]
test_sd12_houses_tree <- sd12_houses_nOut[-training_idx_tree,]

# define a formula for predicting Sales
sd12_houses_tree_formula = zipcode ~ date + price + bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + sqft_above + sqft_basement + yr_built + yr_renovated + sqft_living15 + sqft_lot15 + pt_stops + crimes

# train a decision tree
tree_sd12_houses <- tree(sd12_houses_tree_formula, data = training_sd12_houses_tree)

# inspect the tree
summary(tree_sd12_houses)

# plot the tree
plot(tree_sd12_houses)
text(tree_sd12_houses, pretty = 0)

# prune the tree using cross-validation
cv_sd12_houses <- cv.tree(tree_sd12_houses, FUN=prune.misclass)
# create a table of tree size and classification error
#   note: the cross-validation object has an attribute dev for the classification error
#     you can check the attribute names using attributes(cv_carseats_sales)
cv_sd12_houses_table <- data.frame(
  size = cv_sd12_houses$size,
  error = cv_sd12_houses$dev
)

# plot the cv_carseats_sales_table
plot(
  cv_sd12_houses_table,
  xaxt = 'n',
  yaxt = 'n'
)
axis(1, seq(1,max(cv_sd12_houses_table$size)))
axis(2, seq(0,2,1))

# select the tree size with the minimum error
pruned_tree_size <- cv_sd12_houses_table[which.min(cv_sd12_houses_table$error), 'size']

# prune the tree to the required size
pruned_tree_sd12_houses <- prune.misclass(tree_sd12_houses, best = pruned_tree_size)

# inspect the pruned tree
summary(pruned_tree_sd12_houses)

# plot the tree
plot(pruned_tree_sd12_houses)
text(pruned_tree_sd12_houses, pretty = 0)

# compare the un/pruned trees
opar <- par()
par(mfrow = c(1,2))
plot(tree_sd12_houses)
text(tree_sd12_houses, pretty = 0)
plot(pruned_tree_sd12_houses)
text(pruned_tree_sd12_houses, pretty = 0)
par(opar)

######################################################################
# 4. Decision tree prediction

# compute the prediction for un/pruned trees
#   note: the Sales attribute (column 1) is excluded from the test data set
tree_sd12_houses_pred <- predict(tree_sd12_houses, test_sd12_houses_tree[,-1], type= "class")
pruned_tree_sd12_houses_pred <- predict(pruned_tree_sd12_houses, test_sd12_houses_tree[,-1], type= "class")

# create a table with actual values and the two predictions
sd12_houses_results <- data.frame(
  actual = test_sd12_houses_tree$zipcode,
  unpruned = tree_sd12_houses_pred,
  pruned = pruned_tree_sd12_houses_pred
)

# create a contingency table for the actual VS predicted for both predictions
unpruned_results_table <- table(sd12_houses_results[,c('actual', 'unpruned')])
unpruned_results_table
pruned_results_table <- table(sd12_houses_results[,c('actual', 'pruned')])
pruned_results_table

# calculate accuracy from each contigency table
#   as sum of diagonal elements over sum of the matrix values
acc_unpruned <- sum(diag(unpruned_results_table)) / sum(unpruned_results_table)
acc_unpruned
acc_pruned <- sum(diag(pruned_results_table)) / sum(pruned_results_table)
acc_pruned


# Confusion Matrix for performance evaluation

tree_confmat <- confusionMatrix(data = tree_sd12_houses_pred, reference = test_sd12_houses_tree$zipcode, positive = "True")
rf_confmat <- confusionMatrix(data = pruned_tree_sd12_houses_pred, reference = test_sd12_houses_tree$zipcode, positive = "True")
tree_confmat
rf_confmat
