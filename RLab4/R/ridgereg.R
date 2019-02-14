
ridgereg <- setRefClass("ridgereg",
                        fields=list(formula="formula",data="data.frame",lambda="numeric",
                                    betaHat="matrix",yHat="matrix",df="integer", varRes="numeric", 
                                    varCoef="matrix",t_Beta="numeric",p="numeric",res="matrix",
                                    rstand2="numeric",cstand2="numeric"),
                        contains = "numOperations",
                        methods=list(
                          initialize = function(formula,data,lambda) {
                            if (inherits(formula, "formula")==FALSE) {
                              stop("The formula provided is not a formula.")
                            }
                            if (inherits(data, "data.frame")==FALSE) {
                              stop("The data provided is not a data frame.")
                            }
                            if (inherits(lambda, "numeric")==FALSE) {
                              stop("The lambda provided is not a numeric.")
                            }
                            data<<-data
                            X<-model.matrix(formula, data=data)
                            X_norm <- norm_x(X)
                            nameY<-all.vars(formula)[1]
                            y<-data[[nameY]]
                            betaHat<<-solve(t(X_norm)%*%X_norm +  diag(rep(lambda,dim(X_norm)[2]), dim(X_norm)[2])   )%*%t(X_norm)%*%y
                            yHat<<-pred_y(X_norm,betaHat)
                            
                            formula <<-formula
                            res<<-y-yHat
                            df<<-length(y)-dim(betaHat)[1]
                            varRes<<-as.numeric((t(res)%*%res)/df)
                            varCoef<<-varRes*(solve(t(X_norm)%*%X_norm))
                            tBeta<-matrix()
                            for (i in seq(dim(betaHat)[1])) {
                              tBeta[i]<-betaHat[i]/sqrt(varCoef[i,i])
                            }
                            t_Beta<<-tBeta
                            prob_val <- 2
                            p<<-prob_val*pt(abs(t_Beta), df=df, lower.tail=FALSE)
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
                            if (is.null(data2)) {
                              message("No data provided, using original input.")
                              data2<-data
                            }
                            
                            X2 <-model.matrix(formula, data=data2)
                            X2_norm <- norm_x(X2)
                            y2Hat<-pred_y(X2_norm,betaHat)
                            return(y2Hat)
                          },
                          print = function() {
                            t(betaHat)
                          },
                          summary = function() {
                            s <- data.frame(betaHat,cstand2,t_Beta,p)
                            colnames(s) <- c("Estimate", "Std.Error","t value","p value")
                            
                            return(list(Coefficients=s,degrees_of_freedom=df,residual_standard_error=sqrt(varRes)))
                          }))

