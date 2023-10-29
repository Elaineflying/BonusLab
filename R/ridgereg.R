#' ridgereg is to build a ridge regression mode.
#' @references Reference page link <https://dataxujing.github.io/R_oop/RC.html#section-5.5.1>
#' @description ridgereg is to build a multiple regression mode based on a given dataset and a formula.
#' @param formula a formula specifying the regression model, e.g., y ~ x1 + x2
#' @param data a data frame containing the variables specified in the formula.
#' @param lambda the variable λ specifying the variable for ridge  regression formula.
#' @returns An object of class "ridgereg" containing ridge regression results.
#' @examples
#' data(iris)
#' ridgereg_mod_object <- ridgereg$new(Petal.Length~Species, data = iris, lambda=0)
#' ridgereg_mod_object$print()
#' ridgereg_mod_object$resid()
#' ridgereg_mod_object$pred()
#' ridgereg_mod_object$coef()
#' ridgereg_mod_object$summary()
#' @importFrom plyr is.formula
#' @import ggplot2
#' @import methods
#' @export ridgereg
#define an RC class object named ridgereg
ridgereg <- setRefClass("ridgereg",fields=list(Formula="formula",
                                           Dataset="data.frame",
                                           DatasetName="character",
                                           X="matrix",
                                           y="numeric",
                                           I="matrix",
                                           lambda="numeric",
                                           RidgeCoefficients="matrix",
                                           FittedValues="matrix",
                                           Residuals="matrix",
                                           DegreesofFreedom="numeric",
                                           ResidulVariance="numeric",
                                           VarianceofCoefficients="matrix",
                                           StandardErrors="numeric",
                                           TValues="matrix",
                                           PValues="matrix"),
                      methods = list(
                        #initialize the function
                        initialize = function(formula, data, lambda) {
                          stopifnot(plyr::is.formula(formula),is.data.frame(data),is.numeric(lambda))
                          #create matrix X with independent variables
                          .self$Formula <<- formula
                          .self$Dataset <<- data
                          .self$lambda <<- lambda
                          #extract datasetname
                          .self$DatasetName <<- deparse(substitute(data))
                          #create matrix X with independent variables
                          .self$X <<- model.matrix(formula, data)
                          #create dependent varabile y
                          .self$y <<- data[,all.vars(formula)[1]]
                          #create I diagonal matrix
                          .self$I <<- diag(ncol(.self$X))
                          #XtX and Xty
                          #calculate the ridge coefficients β^ using the formula (XtX+λI)−1 Xty
                          .self$RidgeCoefficients <<- solve(t(.self$X) %*% .self$X + lambda * I) %*% (t(.self$X) %*% .self$y)
                          #calculate the fitted values using the formula Xβ^
                          .self$FittedValues <<- .self$X %*% .self$RidgeCoefficients
                          #calculate the residuals using formula y - X %*% beta_hat
                          .self$Residuals <<- .self$y - .self$X %*% .self$RidgeCoefficients
                          #calculate the degrees of freedom using formula n-p
                          # n is the number of observations
                          # p is number of predictor variables
                          .self$DegreesofFreedom <<- length(.self$y) - ncol(.self$X)
                          #calculate the residual variance using formula sum(residuals^2) / df
                          .self$ResidulVariance <<- sum(.self$Residuals^2) / .self$DegreesofFreedom
                          #calculate the variance of the regression coefficients
                          .self$VarianceofCoefficients <<- .self$ResidulVariance * solve(t(.self$X) %*% .self$X)
                          #calculate standard errors of coeficients
                          .self$StandardErrors <<- sqrt(diag(.self$VarianceofCoefficients))
                          #calculate t-values for each coefficient
                          .self$TValues <<- .self$RidgeCoefficients / .self$StandardErrors
                          #calculate p-values for each coefficient
                          .self$PValues <<- 2*pt(-abs(.self$TValues), df=length(data)-1)
                        },
                        # print function for ridgereg
                        print = function(){
                          cat ("call:\n")
                          cat (paste0("ridgereg(formula = ", deparse(Formula), ", data = ", DatasetName, ", lambda = ", lambda, ")\n"))
                          cat ("\nCoefficients:\n")
                          base::print(RidgeCoefficients[,1])
                        },
                        # resid() function for ridgereg
                        resid = function(){
                          base::print(Residuals[,1])
                        },
                        # pred() function for ridgereg
                        pred = function(){
                          base::print(FittedValues[,1])
                        },
                        # coef() function for ridgereg
                        coef = function() {
                          base::print(RidgeCoefficients[,1])
                        },
                        # summary() function for ridgereg
                        summary = function() {
                          cat ("call:\n")
                          cat (paste0("ridgereg(formula = ", deparse(Formula), ", data = ", DatasetName, ", lambda = ", lambda, ")\n"))
                          cat ("\nResiduals:\n")
                          # Calculate the Residuals statistics
                          min_residual <- min(Residuals)
                          q1_residual <- quantile(Residuals, 0.25)
                          median_residual <- median(Residuals)
                          q3_residual <- quantile(Residuals, 0.75)
                          max_residual <- max(Residuals)
                          resid_matrix <- matrix(0, nrow = 1, ncol = 5)
                          colnames(resid_matrix) <- c("Min", "1Q", "Median", "3Q", "Max")
                          resid_matrix[1, ] <- c(min_residual, q1_residual, median_residual, q3_residual, max_residual)
                          base::print(resid_matrix, quote = FALSE)
                          cat ("\nCoefficients:\n")

                          coef_matrix <- cbind(RidgeCoefficients, StandardErrors, TValues, PValues)
                          coef_dataframe <-as.data.frame(coef_matrix)
                          star <- rep("***",ncol(X))
                          coef_dataframe <-cbind(coef_dataframe,star)
                          colnames(coef_dataframe) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
                          base::print(coef_dataframe)
                          cat (paste0("\nResidual standard error: ", sqrt(ResidulVariance)," on ", DegreesofFreedom," degrees of freedom"))
                        },
                        predict = function (newdata, se.fit = FALSE, type = "response") {
                          X_new <- model.matrix(Formula, newdata)
                          fitted_values <- X_new %*% RidgeCoefficients

                          if (se.fit) {
                            if (type == "response") {
                              RSS <- sum((y - fitted_values)^2)
                              sigma <- sqrt(RSS / (length(y) - ncol(X)))
                            } else if (type == "link") {
                              # Calculate standard errors for link function (not relevant for ridge regression)
                              sigma <- NULL
                            }
                            se <- rep(sigma, length(fitted_values))
                            result <- list(fitted = fitted_values, se.fit = se)
                          } else {
                            result <- fitted_values
                          }

                          return(result)
                        }
                      ))


