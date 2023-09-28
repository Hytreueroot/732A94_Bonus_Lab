#' Title
#'
#' @field formula formula. 
#' @field data data.frame. 
#' @field reg_coef numeric. 
#' @field fit_val numeric. 
#' @field res_val numeric. 
#' @field dof numeric. 
#' @field res_var matrix. 
#' @field var_reg_coef numeric. 
#' @field t_val numeric. 
#' @field p_val array. 
#'
#' @return
#' @export
#'
#' @examples
linreg <- setRefClass("linreg", fields = list(formula = "formula",
                                              data = "data.frame",
                                              reg_coef = "numeric",
                                              fit_val = "numeric",
                                              res_val = "numeric",
                                              dof = "numeric",
                                              res_var = "matrix",
                                              var_reg_coef = "numeric",
                                              t_val = "numeric",
                                              p_val = "array"),
                      methods = list(initialize = function(formula, data){
                        formula <<- formula # Bunu kaldır bak 
                        data <<- data # Bunu kaldır bak 
                        
                        X <- model.matrix(formula, data)
                        y <- as.matrix(data[, names(data)==all.vars(formula)[1]]) # daha farklı yaz
                        
                        # calculate regression coefficients
                        reg_coef <<- solve(t(X)%*%X) %*% t(X)%*%y
                        
                        # calculate the fitted values
                        fit_val <<- X%*%reg_coef
                        
                        # calculate the residuals
                        res_val <<- y-X%*%reg_coef
                        
                        # calculate the degrees of freedom
                        # N: sample size, P: the number of parameters or relationships
                        dof <<- nrow(X) - ncol(X)
                        
                        # calculate residual variance
                        res_val <<- (t(e)%*%e)/df
                        
                        # calculate the variance of the regression coefficients
                        var_reg_coef <<- res_val * solve((t(X)%*%X))
                        
                        # calculate t_values for each coefficient
                        t_val <<- reg_coef / (sqrt(var_reg_coef))
                        
                        # calculate p-values
                        p_val <<- pt(reg_coef, df)
                        
                        # resid function
                        resid <- function(){
                          return(as.vector(res_val))
                        }
                        
                        # pred function
                        pred <- function(){
                          return(fit_val)
                        }
                        
                        # coef function
                        coef <- function(){
                          return(reg_coef)
                        }
                      }))
                               