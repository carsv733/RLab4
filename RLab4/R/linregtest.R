linreg <- setRefClass("linreg",
                      fields=list(formula="formula",data="data.frame",
                                  betaHat="matrix",yHat="matrix",res="matrix",
                                  df="matrix", varRes="matrix", 
                                  varCoef="matrix", tBeta="matrix", p="matrix"),
                      methods=list(
                        linreg = function(formula,data) {
                          X<-model.matrix(formula, data=data)
                          nameY<-all.vars(formula)[1]
                          y<-data[[nameY]]
                          betaHat<<-solve(t(X)%*%X)%*%t(X)%*%y
                          yHat<<-X%*%betaHat
                          res<<-y-yHat
                          df<<-length(y)-dim(betaHat)[1]
                          varRes<<-as.numeric((t(res)%*%res)/df)
                          varCoef<<-varRes*(solve(t(X)%*%X))
                          tBeta<<-matrix()
                          for (i in seq(dim(betaHat)[1])) {
                            tBeta[i]<<-betaHat[i]/sqrt(varCoef[i,i])
                          }
                          p<<-2*pt(abs(tBeta), df=df, lower.tail=FALSE)
                        }))




######-------------------------------------------
linreg <- setRefClass("linreg",
                      fields=list(formula="formula",data="data.frame",betaHat="matrix",yHat="matrix",res="matrix"),
                      methods=list(
                        linreg = function(formula,data) {
                          X<-model.matrix(formula, data=data)
                          nameY<-all.vars(formula)[1]
                          y<-data[[nameY]]
                          return(list(y,X))
                        },
                        coef = function(formula,data) {
                          X<-model.matrix(formula, data=data)
                          nameY<-all.vars(formula)[1]
                          y<-data[[nameY]]
                          betaHat<<-solve(t(X)%*%X)%*%t(X)%*%y
                          return(betaHat)
                        },
                        pred = function(formula,data) {
                          X<-model.matrix(formula, data=data)
                          nameY<-all.vars(formula)[1]
                          y<-data[[nameY]]
                          betaHat<<-solve(t(X)%*%X)%*%t(X)%*%y
                          yHat<<-X%*%betaHat
                          return(yHat)
                        },
                        resid = function(formula,data) {
                          X<-model.matrix(formula, data=data)
                          nameY<-all.vars(formula)[1]
                          y<-data[[nameY]]
                          betaHat<<-solve(t(X)%*%X)%*%t(X)%*%y
                          yHat<<-X%*%betaHat
                          res<<-y-yHat
                          return(res)
                        }))




b <- linreg$new()
b$resid(Sepal.Length ~ Petal.Length,iris)


#####-------------------------------------------------------------





linreg <- setRefClass("linreg",
                      fields=list(formula="formula",data="data.frame"),
                      methods=list(
                        linreg = function(formula,data) {
                          X<-model.matrix(formula, data=data)
                          nameY<-all.vars(formula)[1]
                          y<-data[[nameY]]
                          return(list(y,X))
                        }))

b <- linreg$new()
b$linreg(Sepal.Length ~ Petal.Length,iris)
b$linreg(Sepal.Length ~ Petal.Length,iris)[[1]]


linreg2 <- setRefClass("linreg2", fields=list(betaHat="matrix"),
                       contains = "linreg",
                       methods=list(
                         coef = function() {
                           X <- b$linreg(Sepal.Length ~ Petal.Length,iris)[[2]]
                           y <- b$linreg(Sepal.Length ~ Petal.Length,iris)[[1]]
                           betaHat<<-solve(t(X)%*%X)%*%t(X)%*%y
                           return(betaHat)
                         }))

c <- linreg2$new()
c$coef()

