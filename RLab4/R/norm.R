
numOperations <- setRefClass("numOperations", fields=c(X="matrix", betaHat="matrix"), methods=list(
  norm_x = function(X){
    for (i in 2:dim(X)[2]){
      mean <- mean(X[,i])
      sd <- sd(X[,i])
      for (j in 1:dim(X)[1]) {
        X[j,i] <- (X[j,i]-mean)/sd 
      }
    }
    return(X)
  },
  pred_y = function(X,betaHat) {
    yHat <- X%*%betaHat
    return(yHat)
  }
))
