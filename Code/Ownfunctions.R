
#function to make a Gower Distance Matrix
clustergower <- function(DT, weights){
  #cluster
  gower_dist <- daisy(DT,
                    metric = "gower", 
                    weights = weights)
  gower_mat <- as.matrix(gower_dist)
  gower_mat1<-gower_mat 
  rownames(gower_mat1) <-DT$nummerDWH
  colnames(gower_mat1) <-DT$nummerDWH
  return(list(gower_dist, weights=weights))
  }
  
#function to make a silhuoette width plot
Plotgower <- function(gower_dist, n=50){
  sil_width <- c(NA)
  for(i in 2:50){
    pam_fit <- pam(gower_dist[[1]],
                   diss = TRUE,
                   k = i)
    sil_width[i] <- pam_fit$silinfo$avg.width
  }
  p <- plot(1:n, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width")
  lines(1:n, sil_width)
  title("")

return(p)
}


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

#function to make melt a  Gower Distance Matrix
MeltGowerMat <- function(InputDat){
  InputDat <- round(InputDat,4)
  InputDat[lower.tri(InputDat)] <- NA
  InputDat <-melt(InputDat) %>%
    filter(is.na(value)==F)
  names(InputDat) <- c("Var1","Var2","value")
  
  #make nice names for policies and concern for each combination
  InputDat$Polisnr1 <- gsub("_.*","",gsub(".*#","",InputDat$Var1) )
  InputDat$Polisnr2 <- gsub("_.*","",gsub(".*#","",InputDat$Var2) )
  InputDat$Polis1 <- gsub("@.*","",gsub(".*_","",InputDat$Var1))
  InputDat$Polis2 <- gsub("@.*","",gsub(".*_","",InputDat$Var2))
  InputDat$Concern1 <-gsub("#.*","",(gsub("_.*","",InputDat$Var1)))
  InputDat$Concern2 <-gsub("#.*","",(gsub("_.*","",InputDat$Var2)))
  InputDat$jaar <- gsub(".*@","",InputDat$Var1)

  #make polis level ordering for plot
  levelsPolis <- unique(InputDat[order(InputDat$Polisnr1),]$Polis1)
  InputDat$Polis1 <- factor(InputDat$Polis1, levels = levelsPolis )
  InputDat$Polis2 <- factor(InputDat$Polis2, levels = levelsPolis)
InputDat
}

#function to make a Variable Importance Plot
getImportanceData <- function(rfObject,variable){
  #https://gist.github.com/ramhiser/6dec3067f087627a7a85
  # Extracts variable importance (mean decrease in accuracy)
  # Sorts by variable importance and relevels factors to match ordering
  var_importance <- data_frame(variable=variable,
                                 importance=as.vector(importance(rfObject,type=1)))
  var_importance <- arrange(var_importance, desc(importance))
  var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
  var_importance
}

#function to  calculate correlations
getCorData <- function(DT){
  melted_cormat <- reshape2::melt(get_upper_tri(round(cor(DT,method='spearman'),2)), na.rm = TRUE)
  melted_cormat <- melted_cormat %>% 
    mutate(X1=factor(X1,levels=names(DT))) %>% 
    mutate(X2=factor(X2,levels=names(DT)))
  melted_cormat
  }