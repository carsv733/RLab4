
numOperations <- setRefClass("numOperations", fields=c(X_in="matrix", betaHat="matrix"), methods=list(
  norm_x = function(X){
    X_new<-X
    for (i in 2:dim(X)[2]){
      mean <- mean(X[,i])
      sd <- sd(X[,i])
      for (j in 1:dim(X)[1]) {
        X_new[j,i] <- (X[j,i]-mean)/sd 
      }
    }
    return(X_new)
  },
  pred_y = function(X,betaHat) {
    yHat <- X%*%betaHat
    return(yHat)
  }
))
