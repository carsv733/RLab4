
ridgereg <- setRefClass("ridgereg",
                        fields=list(formula="formula",data="data.frame",lambda="numeric",
                                    betaHat="matrix",yHat="matrix"),
                        methods=list(
                          initialize = function(formula,data,lambda) {
                            
                            X<-model.matrix(formula, data=data)
                            norm <- function(X){
                              for (i in 2:dim(X)[2]){
                                mean <- mean(X[,i])
                                sd <- sd(X[,i])
                                for (j in 1:dim(X)[1]) {
                                  X[j,i] <- (X[j,i]-mean)/sd 
                                }
                              }
                              return(X)
                            }
                            
                            X_norm <- norm(X)
                            nameY<-all.vars(formula)[1]
                            y<-data[[nameY]]
                            betaHat<<-solve(t(X_norm)%*%X_norm +  diag(rep(lambda,dim(X_norm)[2]), dim(X_norm)[2])   )%*%t(X_norm)%*%y
                            yHat<<-X_norm%*%betaHat
                            
                          },
                          coef = function() {
                            betaHat
                          },
                          predict = function() {
                            yHat
                          },
                          print = function() {
                            list(Coefficients=t(betaHat))
                          }))

