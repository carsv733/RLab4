
linreg <- setRefClass("linreg",
                      fields=list(formula="formula",data="data.frame",
                                  betaHat="matrix",yHat="matrix",res="matrix",
                                  df="integer", varRes="numeric", 
                                  varCoef="matrix",tBeta2="numeric",p="numeric"),
                      methods=list(
                        initialize = function(formula,data) {
                          X<-model.matrix(formula, data=data)
                          nameY<-all.vars(formula)[1]
                          y<-data[[nameY]]
                          betaHat<<-solve(t(X)%*%X)%*%t(X)%*%y
                          yHat<<-X%*%betaHat
                          res<<-y-yHat
                          df<<-length(y)-dim(betaHat)[1]
                          varRes<<-as.numeric((t(res)%*%res)/df)
                          varCoef<<-varRes*(solve(t(X)%*%X))
                          tBeta<-matrix()
                          for (i in seq(dim(betaHat)[1])) {
                            tBeta[i]<-betaHat[i]/sqrt(varCoef[i,i])
                          }
                          tBeta2<<-tBeta
                          p<<-2*pt(abs(tBeta2), df=df, lower.tail=FALSE)
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
                        print = function() {
                          t(betaHat)
                        },
                        plot = function() {
                          data2 <- data.frame(yHat,res)
                          pl1 <- 
                            ggplot(data2) + 
                            aes(x=yHat, y=res) + 
                            geom_point()
                          return(pl1)
                        },
                        summary = function() {
                          s <- data.frame(betaHat,diag(varCoef),tBeta2,p)
                          return(s)
                          df
                          return(varRes)
                        }))



a <- linreg(Sepal.Length ~ Petal.Length,iris)
a$coef()

b <- lm(Sepal.Length ~ Petal.Length,iris)



