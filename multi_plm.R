multi_plm <- function(..., index, data=DT, model='pooling', hc='HC1') {
  # Function performs regressions for multiple regression equations specified as ...
  # returns lists with results for use with stargazer or outputs results straight 
  # as an html stargazer_file
  
  # Regression equations
  if (!class(list())=="list") {
    reg_eqs <- list(...) # List of all regression equations defined above
  }

  # Names
  # These vectors contain names for things computed for each equation
  fit_n <- seq_along(reg_eqs) # Sequence from one to n along equations, for naming the results
  fit_names <- paste("fit", fit_n, sep=".") # Regression outputs
  se_names <- paste("se", fit_n, sep=".") # Robust standard errors
  # ll_names <- paste("ll", fit_n, sep=".") # Log likelihoods

  for (j in fit_n) {
    eq <- reg_eqs[[j]] # Select regression equation
    # Estimation
    fit <- plm(eq, data=data, model=model, index=index)
    # Adjust SEs and extract stats
    cov <- vcovHC(fit, type=hc) # Covariance matrix
    se <- sqrt(diag(cov)) # Robust standard errors
    # Information criteria (manually calculated following https://stackoverflow.com/a/35924744/2254210)
    res <- fit$residuals
    n <- nrow(fit$model)    
    w <- rep(1, n)
    df <- length(fit$coefficients)
    ll <- 0.5 * (sum(log(w)) - n * (log(2 * pi) + 1 - log(n) + log(sum(w * res^2)))) # Log likelihood of regression
    fit$AIC <- -2 * ll + 2 * df # Add AIC to regression
    fit$BIC <- -2 * ll + log(n) * df # Add BIC to regression
    # Store resuts
    assign(fit_names[j], fit, envir = parent.frame())
    assign(se_names[j], se, envir = parent.frame())
    # assign(ll_names[j], ll, envir = parent.frame())
  }

  # Output: This is written to global workspace if stargazer_output not set
  models_list <- lapply(fit_names, get)
  se_list <- lapply(se_names, get)
  # ll_list <- lapply(ll_names, get)
  
  results <- list(
    "coef" = models_list,
    "se" = se_list
    # "ll" = ll_list
  )

  # Output results
  return(results)
}