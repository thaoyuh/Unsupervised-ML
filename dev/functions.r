#----------------------------------------------------
#               K-means with chosen PC            
#----------------------------------------------------
#' Function to perform and evaluate K-means clusters based on the number of principle components used
#' 
#' @param dfData dataframe, explain variables.
#' @param mD matrix, pairwise distances
#' @param linkage string, linkage type: "single" or "complete"

#' @return Encoder_C encoder matrix with final clusters.
k_mean_with_PC = function(lPCA, num_PC, nstart){
  dfPCA = lPCA$x[,1:num_PC]
  
  # Clustering
  kmeans <- kmeans(dfPCA, centers = 2, nstart = nstart)
  kmeans$cluster <-replace(kmeans$cluster, kmeans$cluster==2, 0) #reformat
  
  # Confusion Matrix
  confusion = confusionMatrix(factor(kmeans$cluster), factor(dfData$diagnosis), mode = "prec_recall", positive="1")
  return(list(kmeans, confusion))
  }

#----------------------------------------------------------------------
#             Manual hierarchical clustering functions           
#----------------------------------------------------------------------

#' Function to calculate the merges and heights of all clusters in hierarchical clustering
#' 
#' @param dfData dataframe, explain variables.
#' @param mD matrix, pairwise distances
#' @param linkage string, linkage type: "single" or "complete"

#' @return Encoder_C encoder matrix with final clusters.
hierachichal_cluster = function (dfData, mD, linkage){
  #Get initial clusters
  init_C = c(1:nrow(dfData))
  #Encoder matrix to store final cluster
  Encoder_C = matrix(NA,nrow=nrow(dfData)-1, ncol=3)
  
  for(j in 1:(nrow(dfData)-1)){
    # Get unique cluster indices after each update
    ind = unique(init_C)
    #Temporary encoder matrix after updating clusters
    Temporary_encoder_matrix = matrix(0, (length(ind) * ((length(ind) - 1) / 2)), 3)
    
    #Get pairwise distances
    k=1
    for(G in 1:(length(ind) - 1)) {
      for (H in (G + 1):length(ind)) {
        clus_G = which(init_C == ind[G])
        clus_H = which(init_C == ind[H])
        if(linkage == "single"){
          Temporary_encoder_matrix[k,] = c(ind[G], ind[H], complete_linkage(mD , clus_G, clus_H))
          k=k+1
        }
        else if(linkage == "complete"){
          Temporary_encoder_matrix[k,] = c(ind[G], ind[H], complete_linkage(mD , clus_G, clus_H))
          k=k+1
        }
        else{
          print("dplyr::Please select the appropriate linkage type.")
        }
      }
    }
    #Get pair of clusters with minimum distance
    minD = Temporary_encoder_matrix [which(Temporary_encoder_matrix[ ,3] == min(Temporary_encoder_matrix[ ,3])),]
    #update initial clusters
    init_C [which(init_C == minD[2]) ] = minD[1]
    #update final clusters with heights
    Encoder_C[j,1] = minD[1]
    Encoder_C[j,2] = minD[2] 
    Encoder_C[j,3] = minD[3] #heights of clusters
  }
  return (Encoder_C)
}

#' Function to calculate the linkage
#' 
#' @param mD matrix, pairwise distances
#' @param clus_1 cluster 1
#' @param clus_2 cluster 2
#' @return linkage linkage of distance between two cluster
single_linkage = function(mD, clus_1, clus_2){
  likage = min(mD[clus_1, clus_2])
  return(likage)
}

#' Function to calculate the linkage
#' 
#' @param mD matrix, pairwise distances
#' @param clus_1 cluster 1
#' @param clus_2 cluster 2
#' @return linkage linkage of distance between two cluster
complete_linkage = function(mD, clus_1, clus_2){
  likage = max(mD[clus_1, clus_2])
  return(likage)
}

