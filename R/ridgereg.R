#' Ridge Regression
#' 
#' @description
#' A package that take a formula object as well as a dataset and return a ridgereg object.
#' Implement a RC class to handle special functions: print(), coef() and predict().
#'
#' @field formula formula. 
#' @field data data.frame. 
#' @field lambda numeric, ridge constant.
#' @field X matrix which contains independent values.
#' @field y matrix which contains dependent values.
#' @field Q matrix, from the QR decomposition. 
#' @field R upper triangular matrix.
#' @field Qy matrix. 
#' @field reg_coef Regression coefficients. 
#' @field fit_val The fitted values. 
#' @field res_val The residuals. 
#' @field dof The degree of freedom. 
#' @field res_var The residual variance. 
#' @field var_reg_coef The variance of the regression coefficients. 
#' @field t_val The t-values for each coefficient. 
#' @field p_val P-values for each regression coefficients.
#' @field data_name character. 
#' @field X_mean numeric. 
#' @field X_var numeric. 
#'
#' @param formula object of class 'ridgereg': a symbolic description of the model to be fitted.
#' @param data data frame containing the variables in the model.
#' @param lambda a numeric data, which is a ridge constant.  
#' @return Returns an object of class 'ridgereg'.
#'
#' @examples
#' data(iris)
#' mod_object <- ridgereg(Petal.Length~Species, data = iris, lambda = 0.5)
#' mod_object$print()
#' mod_object$coef()
#' mod_object$predict()
#' 
#' @importFrom methods new
#' 
#' @exportClass ridgereg
#' @export ridgereg
ridgereg <- setRefClass("ridgereg", fields = list( formula = "formula",
                                                   data = "data.frame",
                                                   lambda = "numeric",
                                                   X = "matrix",
                                                   y = "matrix",
                                                   Q = "matrix",
                                                   R = "matrix",
                                                   Qy = "matrix",
                                                   
                                                   reg_coef = "array",
                                                   fit_val = "array",
                                                   res_val = "array",
                                                   dof = "integer",
                                                   res_var = "numeric",
                                                   var_reg_coef = "array",
                                                   t_val = "array",
                                                   p_val = "array",
                                                   data_name = "character",
                                                   
                                                   X_mean = "numeric",
                                                   X_var = "numeric"
                                                   ),
                                    methods = list(
                                      initialize = function(formula, data, lambda, QR_dec = FALSE){
                                        formula <<- formula
                                        data <<- data
                                        lambda <<- lambda
                                        
                                        # https://stackoverflow.com/questions/36361158/how-to-test-if-an-object-is-a-formula-in-base-r
                                        is.formula <- function(x){
                                          inherits(x, "formula")
                                        }
                                        
                                        stopifnot(is.data.frame(data), is.logical(QR_dec), is.formula(formula))
                                        
                                        data_name <<- deparse(substitute(data))
                                        
                                        X <<- model.matrix(formula, data)
                                        y <<- as.matrix(data[all.vars(formula)[1]])
                                        
                                        X_mean <<- numeric()
                                        X_var <<- numeric()
                                        
                                        # For normalize all covariates
                                        i <- 2
                                        while (i <= ncol(X)){
                                          X_mean <<- c(X_mean, mean(X[,i]))
                                          X_var <<- c(X_var, var(X[,i]))
                                          i <- i+1
                                        }
                                        
                                        
                                        k <- 2 
                                        while (k <= ncol(X)){
                                          X[,k] <<- (X[,k] - X_mean[k-1]) / sqrt(X_var[k-1]) # xnorm
                                          k <- k+1
                                        }
                                        
                                        # https://stat.ethz.ch/R-manual/R-patched/library/base/html/qr.html
                                        # https://pages.stat.wisc.edu/~st849-1/lectures/Orthogonal.pdf
                                        # http://staff.www.ltu.se/~jove/courses/c0002m/least_squares.pdf
                                        # https://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression
                                        if(QR_dec == TRUE){ 
                                          QR <- qr(X)
                                          Q <<- qr.Q(QR)
                                          R <<- qr.R(QR)
                                          Qy <<- t(Q) %*% y
                                          
                                          reg_coef <<- solve((t(R)%*%R)+(lambda*diag(ncol(X))))%*%(t(X)%*%y)
                                          fit_val <<- X %*% reg_coef
                                          res_val <<- y - fit_val
                                          #res_val <<- round(y - fit_val, 5)
                                          dof <<- nrow(X) - ncol(X)
                                          #dof <<- length(X[,1]) - length(X[1,])
                                          res_var <<- as.numeric((t(res_val) %*% res_val)/dof)
                                          #res_var <<- (t(res_val) %*% res_val) / dof
                                          var_reg_coef <<- as.numeric(res_var) * solve((t(R)%*%R))
                                          t_val <<- reg_coef / sqrt(diag(var_reg_coef))
                                          p_val <<- 2*pt(abs(t_val), dof, lower.tail = FALSE)
                                          #p_val <<- 2*pt(-abs(t_val), dof)
                                          
                                        }
                                        else{
                                          reg_coef <<- solve((t(X) %*% X) +(lambda*diag(ncol(X)))) %*% (t(X) %*% y)
                                          fit_val <<- X %*% reg_coef
                                          #res_val <<- y - fit_val
                                          #dof <<- nrow(X) - ncol(X)
                                          #res_var <<- as.numeric((t(res_val) %*% res_val)/dof)
                                          #var_reg_coef <<- as.numeric(res_var) * solve((t(X)%*%X))
                                          #t_val <<- reg_coef / sqrt(diag(var_reg_coef))
                                          #p_val <<- 2*pt(abs(t_val), dof, lower.tail = FALSE)
                                        }
                                        },
                                      
                                      coef = function(){
                                        "This function returns regression coefficients"
                                        return(reg_coef)
                                      },
                                      
                                      print = function(){
                                        "This function prints out the coefficients and coefficient names"
                                        cat("Call:", sep="\n")
                                        cat(paste("ridgereg(formula = ", deparse(formula), ", ", "data = ", data_name, ")", sep=""), sep="\n")
                                        cat(sep="\n")
                                        cat("Coefficients:")
                                        cat(sep="\n")
                                        
                                        new_reg_coef_name <- rownames(reg_coef)
                                        new_reg_coef <- round(reg_coef, 2)
                                        
                                        new_reg_coef[1] <- format(new_reg_coef[1], width = max(nchar(new_reg_coef[1]),nchar(new_reg_coef_name[1]),nchar("Coefficients"))+5,justify = "r")
                                        new_reg_coef_name[1]<-format(new_reg_coef_name[1], width=max(nchar(new_reg_coef[1]),nchar(new_reg_coef_name[1]),nchar("Coefficients")),justify = "r")
                                        
                                        for(i in 2:length(new_reg_coef)){
                                          new_reg_coef[i] <- format(new_reg_coef[i], width = max(nchar(new_reg_coef_name[i]), nchar(new_reg_coef[i])), justify = "r")
                                        }
                                        
                                        cat(new_reg_coef_name)
                                        cat(sep="\n")
                                        cat(new_reg_coef)
                                      },
                                      
                                      predict = function(df=NA){
                                        if (is.na(df)){
                                          return(fit_val)
                                        }
                                        else{
                                          sd1 <- numeric(length(X)-1)
                                          for (i in 1:(length(X) - 1)) {
                                            sd1[i] <- sd(X[,i+1])
                                          }
                                          df_norm <- scale(df,colMeans(X[,-1]),sd1)
                                          pred <- (as.matrix(cbind(1,df_norm))) %*% reg_coef
                                          return(pred)
                                        }
                                      }
                                      
                                    ))