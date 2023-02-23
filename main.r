#------------------------------------
#               Set up           
#------------------------------------
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
lPCA = prcomp(dfData[, -c(1)], center = TRUE, scale = TRUE)

# Choose PC with eigenvalues of greater than 1
eig.val <- get_eigenvalue(lPCA)
eig.val

#Scree plot 
fviz_eig(lPCA, addlabels = TRUE, barcolor = "dodgerblue",  barfill = "dodgerblue",
         linecolor = "maroon", ylim = c(0, 50))
# Variable plot
fviz_pca_var(lPCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("deepskyblue", "gold", "hotpink"),
             repel = TRUE,     
             select.var = list(name =NULL, cos2 = NULL, contrib = 10)
)

# Explained variance cumulative sum
plot(
  cumsum(lPCA$sdev^2/sum(lPCA$sdev^2)*100) ,
  xlab = "Components" ,
  ylab = "Cumulative Percentage of Variance" , type = "b",
  pch = 16
)

# Correlation plot
var <- get_pca_var(lPCA)
var$cor[,1:6]
corrplot(var$cor[,1:2], is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2 QUESTION: choose first 3 or 6?
fviz_cos2(lPCA, choice = "var", axes = 1:2 , top = 30)

# Contributions of variables to PC1
fviz_contrib(lPCA, choice = "var", axes = 1:2, top = 30, color = "dodgerblue", fill="dodgerblue")
# Contributions of variables to PC2
fviz_contrib(lPCA, choice = "var", axes = 2, top = 10)

# Individual plot
fviz_pca_ind(lPCA, 
             repel = TRUE, # Avoid text overlapping (slow if many points)
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = factor(dfData$diagnosis), # color by groups
             palette =  c("#00AFBB", "#FC4E07"),
            # addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

# plot diagnosis scatter
Diagnosis = dfData$diagnosis
PC_plot<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=factor(Diagnosis), col= Diagnosis))+ 
  geom_point(alpha=0.5) + stat_ellipse(aes(color=Diagnosis), level = 0.95)
PC_plot + scale_color_gradient(low="dodgerblue", high="maroon")


#----------------------------------------------
#             K-mean clustering            
#----------------------------------------------

# Get principle components
dfPCA = lPCA$x[,1:2]

# CLustering
kmeans <- kmeans(dfPCA, centers = 2, nstart = 20)
kmeans$cluster <-replace(kmeans$cluster, kmeans$cluster==2, 0)
kmeans$cluster

# Confusion Matrix
confusionMatrix(factor(kmeans$cluster), factor(dfData$diagnosis), mode = "prec_recall", positive="1")

# Model Evaluation and visualization
# plot diagnosis scatter from kmean
require(gridExtra)
K_mean_cluster = kmeans$cluster
PC_plot1<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=K_mean_cluster, col= K_mean_cluster))+ 
  geom_point(alpha=0.5) + stat_ellipse(aes(color=K_mean_diagnosis), level = 0.85) + scale_color_gradient(low="dodgerblue", high="maroon")
# plot diagnosis scatter from kmean
Real_cluster = dfData$diagnosis
PC_plot2<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=Real_cluster, col= Real_cluster))+ 
  geom_point(alpha=0.5) + stat_ellipse(aes(color=Real_diagnosis), level = 0.85)+ scale_color_gradient(low="dodgerblue", high="maroon")
grid.arrange(PC_plot1, PC_plot2, ncol=2)

#----------------------------------------------
#             Agglomerative clustering            
#----------------------------------------------

# Load data
data("iris")

#Get a subset of data to test clustering
dfIris = sample_n(iris,  30)
dfIris = scale(dfIris[,-5])

# Calculate distance matrix
mD = as.matrix(dist(dfIris))

manual_merge_height = hierachichal_cluster(dfIris, mD, "complete")

package_clusters = hclust(dist(dfIris), method="complete")
package_height = as.matrix(package$height)
package_merge = as.matrix(package$merge)

df <- cbind(as.matrix(manual_merge_height),package_merge, package_height)
xtable(df) #Export as Latex table


