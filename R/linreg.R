#' Linear Regression Using Reference Class
#'
#' A function in form of class for fitting a linear regression model using
#' QR decomposition. Also supports basic regression methods like fitting the
#' model, printing the results, plotting diagnostics, and returning residuals,
#' predicted values, and coefficients.
#'
#' @name linreg
#' @export
linreg <- setRefClass( "linreg",
                       fields = list(
                         formula = "formula",
                         data = "data.frame",
                         data_name = "character",
                         X = "matrix",
                         y = "numeric",
                         beta = "numeric",
                         fitted = "numeric",
                         residuals = "numeric",
                         dof = "numeric",
                         residual_variance = "numeric",
                         regression_variance = "matrix",
                         Q = "matrix",
                         R = "matrix",
                         standard_error = "numeric",
                         t_value = "numeric",
                         p_value = "numeric",
                         estimated_residual_se = "numeric"
                       ),
                       methods = list(
                         #' @title Initialize the linreg Object
                         #'
                         #' @description Initializes an object by performing QR decomposition
                         #' and calculating the regression coefficients, fitted values, residuals,
                         #' and residual variance.
                         #'
                         #' @param formula A formula specifying the linear regression model (e.g., y ~ x).
                         #' @param data A data frame containing the data for fitting the model.
                         #' @return An object with the calculated coefficients, residuals, fitted values,
                         #' and other related statistics.
                         initialize = function(formula, data){
                           .self$formula <- formula
                           .self$data <- data
                           .self$data_name <- deparse(substitute(data))
                           .self$X <- model.matrix(formula,data)
                           .self$y <- data[[all.vars(formula)[1]]]

                           #qr decomposition
                           qr_decomposition = qr(.self$X)
                           .self$Q <- qr.Q(qr_decomposition)
                           .self$R <- qr.R(qr_decomposition)

                           #beta
                           .self$beta <- as.numeric(solve(.self$R, crossprod(.self$Q, .self$y)))

                           #fitted value
                           .self$fitted = as.numeric(.self$X %*% .self$beta)

                           #residuals
                           .self$residuals = .self$y - .self$fitted

                           #degrees of freedom
                           n <- length(.self$y)
                           p <- ncol(.self$X)
                           .self$dof <- n-p

                           #residual variance
                           .self$residual_variance <- sum(.self$residuals^2) / .self$dof

                           #regression coefficients var(Beta)
                           .self$regression_variance <-  .self$residual_variance * t(solve(.self$R)) %*% solve(.self$R)
                         },
                         #' @title Print the Coefficient and Formula
                         #'
                         #' @description Prints the formula used in the model and the regression coefficients.
                         print = function() {
                           cat("linreg(formula = ", deparse(.self$formula),
                               ", data = ", .self$data_name, ")\n", sep = "")

                           cat("\nCoefficients:\n")
                           named_coefficients <- .self$beta
                           names(named_coefficients) <- colnames(.self$X)
                           base::print(named_coefficients)
                         },
                         #' @title Plot Graphic
                         #'
                         #' @description Shows two plots: Residuals vs. Fitted values plot
                         #' and Scale-Location plot.

                         plot = function(){
                           #residuals vs fitted
                           plot_rvf <- ggplot(data = data.frame(fitted = .self$fitted, residuals = .self$residuals), aes(x = fitted, y = residuals)) +
                             geom_point() +
                             geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                             labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")

                           #scale location
                           standardized_residuals <- .self$residuals / sqrt(.self$residual_variance)
                           plot_sl <- ggplot(data = data.frame(fitted = .self$fitted, sqrt_residuals = sqrt(abs(standardized_residuals))),
                                             aes(x = fitted, y = sqrt_residuals)) +
                             geom_point() +
                             labs(title = "Scale-Location Plot", x = "Fitted values", y = "SQRT(Standardized Residuals)")

                           #print
                           print(plot_rvf)
                           print(plot_sl)
                         },

                         #' @title Get Residuals
                         #'
                         #' @description Returns the residuals (errors) of the regression model.
                         #'
                         #' @return A numeric vector of residuals (ˆe).
                         resid = function(){
                           return(.self$residuals)
                         },

                         #' @title Get Predicted Values
                         #'
                         #' @description Returns the predicted values (fitted values) of the model.
                         #'
                         #' @return A numeric vector of fitted values.
                         pred = function(){
                           return(.self$fitted)
                         },

                         #' @title Get Coefficients
                         #'
                         #' @description Returns the regression coefficients (beta) of the model.
                         #'
                         #' @return A numeric vector of regression coefficients.
                         coef = function(){
                           return(.self$beta)
                         },

                         #' @title Get Model Summary
                         #'
                         #' @description Summarizes the regression model by providing the coefficients,
                         #' standard errors, t-values, p-values, residual standard error, and degrees of freedom.
                         summary = function() {
                           cat("\nCoefficients:\n")
                           named_coefficients <- .self$beta
                           names(named_coefficients) <- colnames(.self$X)

                           named_coefficients <- round(named_coefficients, 2)

                           base::print(named_coefficients)
                           .self$estimated_residual_se <- sqrt(.self$residual_variance)

                           cat("\nEstimated Residual Standard Error:", round(.self$estimated_residual_se, 2), "\n")
                           cat("Degrees of Freedom:", .self$dof, "\n")
                         }



                       )

)
