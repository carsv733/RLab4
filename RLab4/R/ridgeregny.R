
ridgereg_class <- setRefClass("ridgereg",
                        fields=list(data="list"),
                        methods=list(
                          coef = function() {
                            coef <- as.vector(data$Regression_coefficients)
                            names(coef) <- row.names(data$Regression_coefficients)
                            return(coef)
                          },
                          predict = function(data2=NULL) {
                            X2 <- model.matrix(data$formula, data=data2)
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
                            y2Hat <- X2_norm %*% data$Regression_coefficients
                            names(y2Hat) <- as.character(1:length(y2Hat))
                            return(y2Hat[,1])
                          },
                          print = function() {
                            cat("Call:", "\n",
                                "lm(formula = ", paste(deparse(formula)), ",", " data = data)", "\n",
                                "\n", "Coefficients:", "\n",sep = "")
                            cat(row.names(data$Regression_coefficients))
                            cat("\n",data$Regression_coefficients, sep = " ")
                          }))

# a <- ridgereg(Sepal.Length ~ Petal.Width + Petal.Length, iris, 0.2)
# a$predict(head(iris))      

ridgereg = function(formula,data,lambda) {
  
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
  nameY <- all.vars(formula)[1]
  y <- data[[nameY]]
  betaHat <- solve(t(X_norm)%*%X_norm +  diag(rep(lambda,dim(X_norm)[2]), dim(X_norm)[2])   )%*%t(X_norm)%*%y
  yHat <- X_norm%*%betaHat
  
  
  ridgeregList <- list(data_name=all.vars(formula), formula = formula, 
                       Regression_coefficients = betaHat, Fitted_values = yHat)
  
  ridgereg_data <- ridgereg_class(data=ridgeregList)
  
  return(ridgereg_data)
}


# a <- ridgereg(Sepal.Length ~ Petal.Width + Petal.Length, iris, 0.2)
# a$predict(head(iris))  