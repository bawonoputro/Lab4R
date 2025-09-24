
linreg <- setRefClass( "linreg",
                       fields = list(
                         formula = "formula",
                         data = "data.frame",
                         X = "matrix",
                         y = "numeric",
                         beta = "numeric",
                         fitted = "numeric",
                         residuals = "numeric",
                         dof = "numeric",
                         residual_variance = "numeric",
                         regression_variance = "matrix"
                       ),
                       methods = list(
                         initialize = function(formula, data){
                           .self$formula <- formula
                           .self$data <- data
                           .self$X <- model.matrix(formula,data)
                           .self$y <- data[[all.vars(formula)[1]]]

                           #qr decomposition
                           qr_decomposition = qr(.self$X)
                           .self$Q <- qr.Q(qr_decomposition)
                           .self$R <- qr.R(qr_decomposition)

                           #beta
                           .self$beta <- as.numeric(solve(.self$R, crossprod(.self$Q, .self$y)))

                           #fitted value
                           .self$fitted = .self$X %*% .self$beta

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
                         print = function(){
                           cat("Call:\n")
                           print(.self$formula)

                           cat("\nCoefficients:\n")
                           print(.self$beta)

                         },
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
                         resid = function(){
                           return(.self$residuals)
                         },
                         pred = function(){
                           return(.self$fitted)
                         },
                         coef = function(){
                           return(.self$beta)
                         },
                         summary = function(){
                           .self$standard_error <- sqrt(diag(.self$regression_variance))
                           .self$t_value <- .self$beta / sqrt(diag(.self$regression_variance))
                           .self$p_value <- 2 * (1 - pt(abs(.self$beta / sqrt(diag(.self$regression_variance))), df = .self$dof))

                           summary_table <- data.frame(
                             Standard_Error = .self$standard_error,
                             t.value = .self$t_value,
                             p_value = .self$p_value,
                           )

                           cat("\nCoefficients:\n")
                           print(summary_table)  # Print the coefficients table with Std.Error, t.value, p.value

                           .self$estimated_residual_se <- sqrt(.self$residual_variance)
                           cat("\nEstimated Residual Standard Error:", .self$estimated_residual_se, "\n")
                           cat("Degrees of Freedom:", .self$dof, "\n")
                         }


                       )

)
