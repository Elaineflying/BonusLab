#' linreg is to build a multiple regression mode.
#' @references Reference page link <https://dataxujing.github.io/R_oop/RC.html#section-5.5.1>
#' @description linreg is to build a multiple regression mode based on a given dataset and a formula.
#' @param formula a formula specifying the regression model, e.g., y ~ x1 + x2
#' @param data a data frame containing the variables specified in the formula.
#' @returns An object of class "linreg" containing regression results.
#' @examples 
#' data(iris)
#' linreg_mod_object <- linreg$new(Petal.Length~Species, data = iris)
#' linreg_mod_object$print()
#' linreg_mod_object$plot()
#' linreg_mod_object$resid()
#' linreg_mod_object$pred()
#' linreg_mod_object$coef()
#' linreg_mod_object$summary()
#' @import plyr
#' @import ggplot2
#' @import methods
#' @export linreg
#define an RC class object named linreg
linreg <- setRefClass("linreg",fields=list(Formula="formula",
                                           Dataset="data.frame",
                                           DatasetName="character",
                                           X="matrix",
                                           y="numeric",
                                           RegressionsCoefficients="matrix",
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
                        initialize = function(formula, data) {
                          stopifnot(plyr::is.formula(formula),is.data.frame(data))
                          #create matrix X with independent variables
                          .self$Formula <<- formula
                          .self$Dataset <<- data
                          #extract datasetname
                          .self$DatasetName <<- deparse(substitute(data))
                          #create matrix X with independent variables
                          .self$X <<- model.matrix(formula, data)
                          #create dependent varabile y
                          .self$y <<- data[,all.vars(formula)[1]]
                          #XtX and Xty
                          #calculate the coefficients β^ using the formula (XtX)−1 Xty
                          .self$RegressionsCoefficients <<- solve(t(.self$X) %*% .self$X) %*% (t(.self$X) %*% .self$y)
                          #calculate the fitted values using the formula Xβ^
                          .self$FittedValues <<- .self$X %*% .self$RegressionsCoefficients
                          #calculate the residuals using formula y - X %*% beta_hat
                          .self$Residuals <<- .self$y - .self$X %*% .self$RegressionsCoefficients
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
                          .self$TValues <<- .self$RegressionsCoefficients / .self$StandardErrors
                          #calculate p-values for each coefficient
                          .self$PValues <<- 2*pt(-abs(.self$TValues), df=length(data)-1)
                        },
                        # print function for linreg
                        print = function(){
                          cat ("call:\n")
                          cat (paste0("linreg(formula = ", deparse(Formula), ", data = ", DatasetName, ")\n"))
                          cat ("\nCoefficients:\n")
                          base::print(RegressionsCoefficients[,1])
                        },
                        # plot() function for linreg
                        plot = function(){
                          #create first plot's specific points dataframe
                          g1_specific_points_df <- data.frame(
                            Residuals = c(Residuals[99], Residuals[118], Residuals[119]),
                            FittedValues = c(FittedValues[99], FittedValues[118], FittedValues[119]),
                            Label = c("99", "118", "119")
                          )
                          #create first plot for residuals VS fitted Values
                          g1 <- ggplot2::ggplot(Dataset, aes(x = FittedValues, y = Residuals)) +
                            geom_point(shape = 1) +
                            labs(x = paste0("Fitted Values\n linreg(", deparse(Formula), ")"), y = "Residuals", title = "Residuals VS Fitted") +
                            stat_summary(fun = median, geom = "line", color="red") +
                            theme_test() +
                            theme(plot.title = element_text(size=12,hjust=0.5)) +
                            geom_hline(yintercept = 0, color = "grey", linetype = "dotted") +
                            geom_text(data = g1_specific_points_df, aes(x = FittedValues, y = Residuals, label = Label), hjust = -0.2, vjust = 0.5, size = 3)
                          #create second plot's dataframe from the linreg mode
                          g2_residuals_fitted_df <- data.frame(
                            Sqrt_Abs_Residuals = sqrt(abs(Residuals/sd(Residuals))),
                            FittedValues = FittedValues
                          )
                          #create second plot's specific points dataframe
                          g2_specific_points_df <- data.frame(
                            Sqrt_Abs_Residuals = c(g2_residuals_fitted_df$Sqrt_Abs_Residuals[99], g2_residuals_fitted_df$Sqrt_Abs_Residuals[118], g2_residuals_fitted_df$Sqrt_Abs_Residuals[119]),
                            FittedValues = c(g2_residuals_fitted_df$FittedValues[99], g2_residuals_fitted_df$FittedValues[118], g2_residuals_fitted_df$FittedValues[119]),
                            Label = c("99", "118", "119")
                          )
                          #create second plot for scale-location
                          g2 <- ggplot2::ggplot(g2_residuals_fitted_df, aes(x = FittedValues, y = Sqrt_Abs_Residuals)) +
                            geom_point(shape = 1) +
                            labs(x = paste0("Fitted Values\n linreg(", deparse(Formula), ")"), y = expression(sqrt(abs("Standardized residuals"))), title = "Scale-Location") +
                            stat_summary(fun = median, geom = "line", color="red") +
                            theme_test() +
                            theme(plot.title = element_text(size=12,hjust=0.5)) +
                            geom_text(data = g2_specific_points_df, aes(x = FittedValues, y = Sqrt_Abs_Residuals, label = Label), hjust = -0.2, vjust = 0.5, size = 3)
                          plots <- list(g1,g2)
                          for (i in 1:length(plots)) {
                            base::plot(plots[[i]])
                            #print a message to ask user to press Enter
                            cat("Hit <Return> to see the next plot...")
                            #wait for user to press Enter
                            readline(prompt = "")
                          }
                        },
                        # resid() function for linreg
                        resid = function(){
                          base::print(Residuals[,1])
                        },
                        # pred() function for linreg
                        pred = function(){
                          base::print(FittedValues[,1])
                        },
                        # coef() function for linreg
                        coef = function() {
                          base::print(RegressionsCoefficients[,1])
                        },
                        # summary() function for linreg
                        summary = function() {
                          cat ("call:\n")
                          cat (paste0("linreg(formula = ", deparse(Formula), ", data = ", DatasetName, ")\n"))
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

                          coef_matrix <- cbind(RegressionsCoefficients, StandardErrors, TValues, PValues)
                          coef_dataframe <-as.data.frame(coef_matrix)
                          star <- rep("***",ncol(X))
                          coef_dataframe <-cbind(coef_dataframe,star)
                          colnames(coef_dataframe) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
                          base::print(coef_dataframe)
                          cat (paste0("\nResidual standard error: ", sqrt(ResidulVariance)," on ", DegreesofFreedom," degrees of freedom"))
                        }
                      ))
