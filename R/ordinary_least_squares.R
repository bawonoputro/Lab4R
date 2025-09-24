
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

                           #regression
                           XtX <- crossprod(.self$X)
                           Xty <- crossprod(.self$X, .self$y)
                           .self$beta <- as.numeric(solve(XtX, Xty))

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

                           #regression coefficients
                           .self$regression_variance <- .self$residual_variance * solve(XtX)


                         },


                       )

)
