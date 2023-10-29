#' Ridge Regression
#' 
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
#' @field fit_val The fitted values. 
#' @field data_name character.
#' @field reg_coef_ridge Regressions Coefficients of Ridge 
#' @field X_norm Normalization of X
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
#' @importFrom MASS lm.ridge
#' @import caret
#' @import leaps
#' @import mlbench
#' 
#' @export ridgereg
#' @exportClass ridgereg
ridgereg <- setRefClass("ridgereg", fields = list( formula = "formula",
                                                   data = "data.frame",
                                                   lambda = "numeric",
                                                   X = "matrix",
                                                   y = "numeric",
                                                   Q = "matrix",
                                                   R = "matrix",
                                                   Qy = "matrix",
                                                   fit_val = "matrix",
                                                   data_name = "character",
                                                   X_norm='matrix',
                                                   reg_coef_ridge='matrix'
                                                   ),
                                    methods = list(
                                      initialize = function(formula, data, lambda, QR_dec = FALSE){
                                        
                                        # https://stackoverflow.com/questions/36361158/how-to-test-if-an-object-is-a-formula-in-base-r
                                        is.formula <- function(x){
                                          inherits(x, "formula")
                                        }
          
                                        stopifnot(is.data.frame(data), is.logical(QR_dec), is.formula(formula))
                                        
                                        formula <<- formula
                                        data <<- data
                                        lambda <<- lambda
                                        
                                        data_name <<- deparse(substitute(data))
                                        
                                        X <<- model.matrix(formula, data)
                                        y <<- data[,all.vars(formula)[1]]
                                        
                                        X_norm <<- cbind(X[,1],scale(X[,-1]))
                                        reg_coef_ridge <<- solve(t(X_norm)%*%X_norm+lambda*diag(ncol(X_norm)), t(X_norm)%*%y)
                                        rownames(reg_coef_ridge)[1] <<- '(Intercept)'
                                        
                                        
                                        
                                        # https://stat.ethz.ch/R-manual/R-patched/library/base/html/qr.html
                                        # https://pages.stat.wisc.edu/~st849-1/lectures/Orthogonal.pdf
                                        # http://staff.www.ltu.se/~jove/courses/c0002m/least_squares.pdf
                                        # https://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression
                                        if(QR_dec == TRUE){
                                          QR <- qr(X)
                                          Q <<- qr.Q(QR)
                                          R <<- qr.R(QR)
                                          Qy <<- t(Q) %*% y
                                          
                                          reg_coef_ridge <<- solve((t(R)%*%R)+(lambda*diag(ncol(X_norm))))%*%(t(X_norm)%*%y)
                                          
                                        }
                                        
                                        fit_val <<- X_norm%*%reg_coef_ridge
                                        
                                        },
                                      
                                      coef = function(){
                                        "This function returns regression coefficients"
                                        return(reg_coef_ridge)
                                      },
                                      
                                      print = function(){
                                        "This function prints out the coefficients and coefficient names"
                                        cat("Call:", sep="\n")
                                        cat(paste("ridgereg(formula = ", deparse(formula), ", ", "data = ", data_name, ")", sep=""), sep="\n")
                                        cat(sep="\n")
                                        cat("Coefficients:")
                                        cat(sep="\n")
                                        
                                        new_reg_coef_name <- rownames(reg_coef_ridge)
                                        new_reg_coef <- round(reg_coef_ridge, 2)
                                        
                                        new_reg_coef[1] <- format(new_reg_coef[1], width = max(nchar(new_reg_coef[1]),nchar(new_reg_coef_name[1]),nchar("Coefficients"))+5,justify = "r")
                                        new_reg_coef_name[1]<-format(new_reg_coef_name[1], width=max(nchar(new_reg_coef[1]),nchar(new_reg_coef_name[1]),nchar("Coefficients")),justify = "r")
                                        
                                        for(i in 2:length(new_reg_coef)){
                                          new_reg_coef[i] <- format(new_reg_coef[i], width = max(nchar(new_reg_coef_name[i]), nchar(new_reg_coef[i])), justify = "r")
                                        }
                                        
                                        cat(new_reg_coef_name)
                                        cat(sep="\n")
                                        cat(new_reg_coef)
                                      },
                                      
                                      predict = function(newdata=NULL){
                                        if (is.null(newdata)){
                                          return(fit_val)
                                        }
                                        else{
                                          sds <- apply(X[,-1], 2, sd)
                                          newdata_norm <- scale(newdata,colMeans(X[,-1]),sds)
                                          prediction <- as.matrix(cbind(1,newdata_norm)) %*% reg_coef_ridge
                                          return(prediction)
                                        }
                                      }
                                      
                                    ))