#------------------------------------
#               Set up           
#------------------------------------
# load packages
if(!require(pacman)){install.packages("pacman")}
p_load(tidyverse, PMA, latex2exp, knitr, formatR, devtools, 
       ggplot2, ClusterR, cluster, factoextra, gtools, ggplot2, caret, corrplot)

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
fviz_eig(lPCA, addlabels = TRUE, barcolor = "deepskyblue",  barfill = "deepskyblue",
         linecolor = "hotpink", ylim = c(0, 50))
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
fviz_cos2(lPCA, choice = "var", axes = 1:2 , top = 10)

# Contributions of variables to PC1
fviz_contrib(lPCA, choice = "var", axes = 1:2, top = 30)
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
PC_plot<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=Diagnosis, col= Diagnosis))+ 
  geom_point(alpha=0.5) + stat_ellipse(aes(color=Diagnosis), level = 0.95)
PC_plot + scale_color_gradient(low="deepskyblue", high="hotpink")


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
K_mean_diagnosis = kmeans$cluster
PC_plot1<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=K_mean_diagnosis, col= K_mean_diagnosis))+ 
  geom_point(alpha=0.5) + stat_ellipse(aes(color=K_mean_diagnosis), level = 0.90) + scale_color_gradient(low="deepskyblue", high="hotpink")
# plot diagnosis scatter from kmean
Real_diagnosis = dfData$diagnosis
PC_plot2<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=Real_diagnosis, col= Real_diagnosis))+ 
  geom_point(alpha=0.5) + stat_ellipse(aes(color=Real_diagnosis), level = 0.90)+ scale_color_gradient(low="deepskyblue", high="hotpink")
grid.arrange(PC_plot1, PC_plot2, ncol=2)

