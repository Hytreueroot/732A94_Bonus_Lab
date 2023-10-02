#' Linear Regression
#' 
#' @name linreg
#'
#' @field formula formula. 
#' @field data data.frame. 
#' @field reg_coef array. 
#' @field fit_val numeric. 
#' @field res_val numeric. 
#' @field dof numeric. 
#' @field res_var matrix. 
#' @field var_reg_coef numeric. 
#' @field t_val numeric. 
#' @field p_val array. 
#'
#'
#' @examples
#' data(iris)
#' mod_object <- linreg(Petal.Length~Species, data = iris)
#' print(mod_object)
#' 
#' 
#' @return Linear Regression
#' @export linreg
#' @exportClass linreg

linreg <- setRefClass("linreg", fields = list(formula = "formula",
                                              data = "data.frame",
                                              reg_coef = "array",
                                              fit_val = "array",
                                              res_val = "array",
                                              dof = "integer",
                                              res_var = "numeric",
                                              var_reg_coef = "array",
                                              t_val = "array",
                                              p_val = "array",
                                              data_name = "character"),
                      methods = list(
                        initialize = function(formula, data){
                          formula <<- formula
                          data <<- data
                          data_name <<- deparse(substitute(data))
                          X <- model.matrix(formula, data)
                          y <- as.matrix(data[all.vars(formula)[1]])
                        
                          # calculate regression coefficients
                          reg_coef <<- solve(t(X)%*%X) %*% t(X)%*%y
                        
                          # calculate the fitted values
                          fit_val <<- X%*%reg_coef
                        
                          # calculate the residuals
                          res_val <<- y-fit_val
                        
                          # calculate the degrees of freedom
                          # N: sample size, P: the number of parameters or relationships
                          dof <<- nrow(X) - ncol(X)
                        
                          # calculate residual variance
                          res_var <<- as.numeric((t(res_val) %*% res_val)/dof)
                        
                          # calculate the variance of the regression coefficients
                          var_reg_coef <<- as.numeric(res_var) * solve((t(X)%*%X))
                        
                          # calculate t_values for each coefficient
                          t_val <<- reg_coef / (sqrt(diag(var_reg_coef)))
                        
                          # calculate p-values
                          # https://cosmosweb.champlain.edu/people/stevens/webtech/R/Chapter-9-R.pdf
                          p_val <<- 2*pt(abs(t_val), dof, lower.tail = FALSE)
                          },
                        resid = function(){
                          "This function returns residuals value"
                          return(as.vector(res_val))
                          },
                        pred = function(){
                          "This function returns fitted value"
                          return(fit_val)
                          },
                        coef = function(){
                          "This function returns regression coefficients"
                          return(reg_coef)
                          },
                        print = function(){
                          "This function prints out the coefficients and coefficient names"
                          cat("Call:", sep="\n")
                          cat(paste("linreg(formula = ", deparse(formula), ", ", "data = ", data_name, ")", sep=""), sep="\n")
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
                        summary = function(){
                          "This function prints out the coefficients with their standard error, t-value and p-value"
                        
                          new_data <- as.data.frame(cbind(reg_coef, reg_coef / t_val, t_val, p_val, "***"))
                          colnames(new_data) <- c("Estimate", "Standard Error", "T value", "P value", "")
                          print.data.frame(new_data)
                          cat("Residual standard error: ", sqrt(res_var), " on ", dof, " degrees of freedom", sep = "")
                          
                        }
                        ))
                               