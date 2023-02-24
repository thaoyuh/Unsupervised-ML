#--------------------------------------------
#                    Set-up           
#--------------------------------------------
# load packages
if(!require(pacman)){install.packages("pacman")}
p_load(tidyverse, PMA, latex2exp, knitr, formatR, devtools, readr,datasets,
       ggplot2, ClusterR, cluster, factoextra, gtools, ggplot2, caret, corrplot, xtable)

# set seed
set.seed(432)

# import manual functions
source("./dev/functions.r")

#---------------------------------------------
#                Data Preparation           
#---------------------------------------------
# Load data
dfData <- read_csv("data.csv")
attach(dfData)

# Remove irrelevant columns
dfData = dfData[,-c(1, 33)]

# Format the dependent variable
dfData$diagnosis = ifelse(dfData$diagnosis =="M", 1, 0)

# Scale all feature variables
dfData[, -c(1)] <- scale(dfData[, -c(1)]) %>% as.matrix()

#----------------------------------------------
#         Principle Component Analysis            
#----------------------------------------------
lPCA = prcomp(dfData[, -c(1)], center = TRUE, scale = TRUE)  #perform PCA on unlabeled data

# Get dataframe with all PCs, their eigenvalues and (cumulative) explained variances
eig.val <- get_eigenvalue(lPCA)
as.data.frame(eig.val) %>% mutate_if(is.numeric, round, digits = 2) %>% kable(format = 'latex', booktabs = TRUE)

#Scree plot 
fviz_eig(lPCA, addlabels = TRUE, barcolor = "dodgerblue",  barfill = "dodgerblue",
         linecolor = "maroon", ylim = c(0, 50))

# Contributions of variables to the first 3 PCs
fviz_contrib(lPCA, choice = "var", axes = 1:2, top = 10, color = "dodgerblue", fill="dodgerblue")
# Contributions of variables to PC1
fviz_contrib(lPCA, choice = "var", axes = 1, top = 10, color = "dodgerblue", fill="dodgerblue")
# Contributions of variables to PC2
fviz_contrib(lPCA, choice = "var", axes = 2, top = 10, color = "dodgerblue", fill="dodgerblue")

# Descriptions of loadings in first 6 components
as.data.frame(lPCA$rotation[,1:6]) %>% mutate_if(is.numeric, round, digits = 3) %>% kable(format = 'latex', booktabs = TRUE)

#----------------------------------------------
#             K-mean clustering            
#----------------------------------------------

#------------Using 6 principle components------
kmeans_res_6 = k_mean_with_PC(lPCA, 6, nstart = 5)
evaluation_metrics6 = kmeans_res_6[[2]]

#------------Using 3 principle components------
kmeans_res_3 = k_mean_with_PC(lPCA, 3, nstart = 5)
evaluation_metrics3 = kmeans_res_3[[2]]

#------------Using 2 principle components------
kmeans_res_2 = k_mean_with_PC(lPCA, 2, nstart = 5)
evaluation_metrics2 = kmeans_res_2[[2]]

# Scatter plot of diagnosis from K-mean
K_cluster = kmeans_res_6[[1]]$cluster
K_mean_viz<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=K_cluster, col= K_cluster))+ 
  geom_point(alpha=0.6) + stat_ellipse(aes(color=K_cluster), level = 0.85) + scale_color_gradient(low="dodgerblue", high="maroon")
K_mean_viz
# Scatter plot of diagnosis from empirical data
Diagnosis = dfData$diagnosis
PCA_viz<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=Diagnosis, col= Diagnosis))+ 
  geom_point(alpha=0.6) + stat_ellipse(aes(color=Diagnosis), level = 0.85)+ scale_color_gradient(low="dodgerblue", high="maroon")
PCA_viz

#----------------------------------------------------------
#             Comparing manual and package versions            
#----------------------------------------------------------
# Load data
data("iris")

#Get a subset of data to test clustering
dfIris = sample_n(iris,  30)
dfIris = scale(dfIris[,-5])

# Calculate distance matrix
mD = as.matrix(dist(dfIris))

# Manual algorithm
manual_merge_height = hierachichal_cluster(dfIris, mD, "complete")

# Package algorithm
package_clusters = hclust(dist(dfIris), method="complete")
package_height = as.matrix(package$height)
package_merge = as.matrix(package$merge)

# Format outputs
df <- cbind(as.matrix(manual_merge_height),package_merge, package_height)
index <- as.matrix(1:nrow(df))
df = cbind(index,df)
df %>%kable(format = 'latex', booktabs = TRUE)



