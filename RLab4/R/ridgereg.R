
ridgereg <- setRefClass("ridgereg",
                        fields=list(formula="formula",data="data.frame",lambda="numeric",
                                    betaHat="matrix",yHat="matrix",df="integer", varRes="numeric", 
                                    varCoef="matrix",tBeta2="numeric",p="numeric",res="matrix",
                                    rstand2="numeric",cstand2="numeric",formula2="formula"),
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
                            
                            formula2 <<-formula
                            res<<-y-yHat
                            df<<-length(y)-dim(betaHat)[1]
                            varRes<<-as.numeric((t(res)%*%res)/df)
                            varCoef<<-varRes*(solve(t(X_norm)%*%X_norm))
                            tBeta<-matrix()
                            for (i in seq(dim(betaHat)[1])) {
                              tBeta[i]<-betaHat[i]/sqrt(varCoef[i,i])
                            }
                            tBeta2<<-tBeta
                            p<<-2*pt(abs(tBeta2), df=df, lower.tail=FALSE)
                            rstand<-numeric(0)
                            for (i in seq(length(res))) {
                              rstand[i]<-sqrt((abs(res[i]-mean(res)))/varRes)
                            }
                            rstand2 <<- rstand
                            cstand<-numeric(0)
                            for (i in seq(length(betaHat))) {
                              cstand[i]<-sqrt(varCoef[i,i])
                            }
                            cstand2 <<- cstand
                          },
                          coef = function() {
                            betaHat
                          },
                          resid = function() {
                            res
                          },
                          pred = function() {
                            yHat
                          },
                          predict = function(data2=NULL) {
                            X2 <-model.matrix(formula2, data=data2)
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
                            X2_norm <- norm(X2)
                            y2Hat<-X2_norm%*%betaHat
                            # y2Hat<<-X2_norm%*%betaHat
                            return(y2Hat)
                          },
                          print = function() {
                            t(betaHat)
                          },
                          summary = function() {
                            s <- data.frame(betaHat,cstand2,tBeta2,p)
                            colnames(s) <- c("Estimate", "Std.Error","t value","p value")
                            
                            return(list(Coefficients=s,degrees_of_freedom=df,residual_standard_error=sqrt(varRes)))
                          }))

# a <- ridgereg(Sepal.Length ~ Petal.Width + Petal.Length, iris, 0.2)
# a$predict(head(iris))      

