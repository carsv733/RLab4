linreg <- function(formula, data) {
  
  X<-model.matrix(formula, data=data)
  nameY<-all.vars(formula)[1]
  y<-data[[nameY]]
  
  betaHat<-solve(t(X)%*%X)%*%t(X)%*%y

  yHat<-X%*%betaHat
  
  res<-y-yHat
  
  df<-length(y)-dim(betaHat)[1]
  
  varRes<-as.numeric((t(res)%*%res)/df)
  
  varCoef<-varRes*(solve(t(X)%*%X))
  
  tBeta<-matrix()
  for (i in seq(dim(betaHat)[1])) {
    tBeta[i]<-betaHat[i]/sqrt(varCoef[i,i])
  }
  
  p<-2*pt(abs(tBeta), df=df, lower.tail=FALSE)
  return(list(betaHat, yHat, res, df, varRes, varCoef, tBeta, p))
  
}

linreg(waiting ~ eruptions, faithful)
