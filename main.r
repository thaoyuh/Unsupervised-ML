#------------------------------------
#               Set up           
#------------------------------------
# load packages
if(!require(pacman)){install.packages("pacman")}
p_load(tidyverse, PMA, rlist, latex2exp, knitr, formatR, devtools, reshape2, 
       ggplot2, ggbiplot, factoextra, pracma, gtools,sparsepca, ggplot2)

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
lPCA = prcomp(dfData, center = TRUE, scale = TRUE)

# Choose PC with eigenvalues of greater than 1
library(factoextra)
eig.val <- get_eigenvalue(lPCA)
eig.val

#Scree plot 
fviz_eig(lPCA, addlabels = TRUE, 
         linecolor = "Red", ylim = c(0, 50))
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
library(corrplot)
var <- get_pca_var(lPCA)
var$cor[,1:6]
corrplot(var$cor[,1:6], is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.3
fviz_cos2(lPCA, choice = "var", axes = 1:3 , top = 15)

# Contributions of variables to PC1
fviz_contrib(lPCA, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(lPCA, choice = "var", axes = 2, top = 10)

# Individual plot
fviz_pca_ind(lPCA, 
             repel = TRUE, # Avoid text overlapping (slow if many points)
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = factor(dfData$diagnosis), # color by groups
             palette =  c("#00AFBB", "FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

# plot diagnosis scatter
Diagnosis = dfData$diagnosis
PC_plot<-ggplot(as.data.frame(lPCA$x), aes(x=PC1, y=PC2,group=Diagnosis, col= Diagnosis))+ 
  geom_point(alpha=0.5) + #stat_ellipse(aes(color=Diagnosis), level = 0.95)
PC_plot + scale_color_gradient(low="deepskyblue", high="hotpink")

