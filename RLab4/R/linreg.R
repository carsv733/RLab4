
linreg <- setRefClass("linreg",
                      fields=list(formula="formula",data="data.frame",
                                  betaHat="matrix",yHat="matrix",res="matrix",
                                  df="integer", varRes="numeric", 
                                  varCoef="matrix",tBeta2="numeric",p="numeric",
                                  rstand2="numeric",cstand2="numeric"),
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
                          library(ggplot2)
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
                          data2 <- data.frame(yHat,res,rstand2)
                          
                          pl1 <- 
                            ggplot(data2) + 
                            aes(x=yHat, y=res) + 
                            geom_point(shape = 1) +
                            geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "red") +
                            labs(x = "Fitted values", y = "Residuals") + 
                            ggtitle("Residuals vs Fitted") +
                            theme_bw() +
                            theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5))
                          
                          
                          pl2 <- 
                            ggplot(data2) + 
                            aes(x=yHat, y=rstand2) + 
                            geom_point(shape = 1)+
                            geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "red") +
                            labs(x = "Fitted values", y = "sqrt(Standard residuals)") + 
                            ggtitle("Scale-Location") +
                            theme_bw() +
                            theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5))
                          devAskNewPage(ask = TRUE)
                          return(list(pl1,pl2))
                          
                        },
                        summary = function() {
                          s <- data.frame(betaHat,cstand2,tBeta2,p)
                          colnames(s) <- c("Estimate", "Std.Error","t value","p value")
                          
                          return(list(Coefficients=s,degrees_of_freedom=df,residual_standard_error=sqrt(varRes)))
                        }))

