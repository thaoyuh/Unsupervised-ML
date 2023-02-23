#---------------------------------------------------
#         Manual hierachical clustering            
#---------------------------------------------------

#' Function to cross validate for all hyperparameters in Elastic Net.
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
    #update final clusters
    Encoder_C[j,1] = minD[1]
    Encoder_C[j,2] = minD[2]
    Encoder_C[j,3] = minD[3]
  }
  return (Encoder_C)
}

