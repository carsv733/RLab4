linreg <- setRefClass("linreg",
                      fields=list(formula="formula",data="data.frame",
                                  betaHat="matrix",yHat="matrix",res="matrix",
                                  df="integer", varRes="numeric", 
                                  varCoef="matrix",t_Beta="numeric",p="numeric",
                                  r_stand="numeric",c_stand="numeric"),
                      contains = c("numOperations", "dateOperations"),
                      methods=list(
                        initialize = function(formula,data) {
                          if (inherits(formula, "formula")==FALSE) {
                            stop("The formula provided is not a formula.")
                          }
                          if (inherits(data, "data.frame")==FALSE) {
                            stop("The data provided is not a data frame.")
                          }
                          data<<-data
                          X<-model.matrix(formula, data=data)
                          nameY<-all.vars(formula)[1]
                          y<-data[[nameY]]
                          betaHat<<-solve(t(X)%*%X)%*%t(X)%*%y
                          yHat<<-pred_y(X,betaHat)
                          res<<-y-yHat
                          df<<-length(y)-dim(betaHat)[1]
                          varRes<<-as.numeric((t(res)%*%res)/df)
                          varCoef<<-varRes*(solve(t(X)%*%X))
                          tBeta<-matrix()
                          for (i in seq(dim(betaHat)[1])) {
                            tBeta[i]<-betaHat[i]/sqrt(varCoef[i,i])
                          }
                          t_Beta<<-tBeta
                          p<<-2*pt(abs(t_Beta), df=df, lower.tail=FALSE)
                          rstand<-numeric(0)
                          for (i in seq(length(res))) {
                            rstand[i]<-sqrt((abs(res[i]-mean(res)))/varRes)
                          }
                          r_stand <<- rstand
                          cstand<-numeric(0)
                          for (i in seq(length(betaHat))) {
                            cstand[i]<-sqrt(varCoef[i,i])
                          }
                          c_stand <<- cstand
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
                        plot = function() {
                          data2 <- data.frame(yHat,res,r_stand)
                          colr <- "red"
                          vert_just <- 1.5
                          ln_size <- 1.5
                          point_style <- 1
                          pl1 <- 
                            ggplot(data2) + 
                            aes(x=yHat, y=res) + 
                            geom_point(shape = point_style) +
                            geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = colr) +
                            labs(x = "Fitted values", y = "Residuals") + 
                            ggtitle("Residuals vs Fitted") +
                            theme_bw() +
                            theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  plot.title = element_text(size = rel(ln_size), face = "bold", vjust = vert_just))
                          
                          
                          pl2 <- 
                            ggplot(data2) + 
                            aes(x=yHat, y=r_stand) + 
                            geom_point(shape = point_style)+
                            geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = colr) +
                            labs(x = "Fitted values", y = "sqrt(Standard residuals)") + 
                            ggtitle("Scale-Location") +
                            theme_bw() +
                            theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  plot.title = element_text(size = rel(ln_size), face = "bold", vjust = vert_just))
                          devAskNewPage(ask = TRUE)
                          return(list(pl1,pl2))
                          
                        },
                        summary = function() {
                          s <- data.frame(betaHat,c_stand,t_Beta,p)
                          colnames(s) <- c("Estimate", "Std.Error","t value","p value")
                          
                          return(list(Coefficients=s,degrees_of_freedom=df,residual_standard_error=sqrt(varRes)))
                        }))


