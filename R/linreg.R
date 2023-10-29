#' Linear Regression
#' 
#' 
#' @description
#' A package to handle linear regression models with using linear algebra.
#' Implement a RC class to handle special functions: print(), plot(), resid(), 
#' pred(), coef() and summary().
#' 
#' @field formula formula. 
#' @field data data.frame. 
#' @field reg_coef Regression coefficients. 
#' @field fit_val The fitted values. 
#' @field res_val The residuals. 
#' @field dof The degree of freedom. 
#' @field res_var The residual variance. 
#' @field var_reg_coef The variance of the regression coefficients. 
#' @field t_val The t-values for each coefficient. 
#' @field p_val P-values for each regression coefficients.
#'
#' @param formula object of class 'linreg': a symbolic description of the model to be fitted.
#' @param data data frame containing the variables in the model. 
#'
#' @return Returns an object of class 'linreg'. 
#' 
#' @examples
#' data(iris)
#' mod_object <- linreg(Petal.Length~Species, data = iris)
#' mod_object$print()
#' mod_object$resid()
#' mod_object$pred()
#' mod_object$coef()
#' mod_object$summary()
#' mod_object$plot()
#' 
#' @importFrom methods new
#' @import ggplot2
#' 
#' @exportClass linreg
#' @export linreg


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
                          
                        },
                        
                        plot = function(){
                          "This function plots the following two plots: Residuals vs Fitted, Scale-Location"
                          library(ggplot2)
                          
                          #https://www.marsja.se/how-to-make-a-residual-plot-in-r-interpret-them-ggplot2/
                          
                          pointlabel <- array(rep("", 150))
                          pointlabel[c(99, 118, 119)] <- c(99, 118, 119)
                          
                          # First Graph
                          
                          data1 <- data.frame(fit_val, res_val, pointlabel)
                          
                          graph1 <- ggplot(data = data1, mapping = aes(data1[, 1], data1[, 2])) +
                            geom_point(shape=1, size = 3) +
                            geom_text(aes(label = data1[, 3]), nudge_x = -0.15) +
                            geom_hline(yintercept = 0, linetype = "dotted", color="grey") +
                            scale_y_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, 1)) +
                            stat_summary(geom = "line", fun = median , color = "red") +
                            theme_bw() +
                            theme(plot.title = element_text(hjust = .5), panel.grid = element_blank()) +
                            labs(x= "Fitted values\nlm(Petal.Length ~ Species)", y= "Residuals", title = "Residuals vs Fitted") 
                          
                          # Second Graph
                          
                          data2 <- data.frame(fit_val, sqrt(abs(res_val/sd(res_val))), pointlabel)
                          
                          graph2 <- ggplot(data = data2, mapping = aes(data2[, 1], data2[, 2])) +
                            geom_point(shape=1, size = 3) +
                            geom_text(aes(label = data2[, 3]), nudge_x = -0.15) +
                            scale_y_continuous(limits = c(0, 2), breaks = seq(0, 1.5, 0.5)) +
                            stat_summary(geom = "line", fun = mean , color = "red") +
                            # stat_summary(geom = "line", fun = median , color = "red") +
                            # When I try to "fun = median", it is not giving correct graph but "fun = mean" is giving correct graph.
                            theme_bw() +
                            theme(plot.title = element_text(hjust = .5), panel.grid = element_blank()) +
                            labs(x= "Fitted values\nlm(Petal.Length ~ Species)", y= expression(sqrt(abs("Standardized residuals"))), title = "Scale-Location")
                          
                          return(list(graph1,graph2))
                          
                        }
                        ))
                               