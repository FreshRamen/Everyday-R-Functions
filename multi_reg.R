multi_reg <- function(..., data=DT, hc="HC1") {
  # Function performs regressions for multiple regression equations specified as ...
  # returns lists with results for use with stargazer or outputs results straight 
  # as an html stargazer_file
  
  # Regression equations
  # if (!class(list())=="list") {
    reg_eqs <- list(...) # List of all regression equations defined above
  # }

  # Names
  # These vectors contain names for things computed for each equation
  fit_n <- seq_along(reg_eqs) # Sequence from one to n along equations, for naming the results
  fit_names <- paste("fit", fit_n, sep=".") # Regression outputs
  se_names <- paste("se", fit_n, sep=".") # Robust standard errors
  ll_names <- paste("ll", fit_n, sep=".") # Log likelihoods

  for (j in fit_n) {
    eq <- reg_eqs[[j]] # Select regression equation
    fit <- lm(eq, data=data) # Estimate regression
    cov <- vcovHC(fit, type = hc) # Covariance matrix
    se <- sqrt(diag(cov)) # Robust standard errors
    ll <- logLik(fit)[1] # Log likelihood of regression
    fit$AIC <- AIC(fit) # Add AIC to regression
    fit$BIC <- BIC(fit) # Add BIC to regression
    # Store (I'm not sure why the environment change is needed but this way it works...)
    assign(fit_names[j], fit, envir = parent.frame())
    assign(se_names[j], se, envir = parent.frame())
    assign(ll_names[j], ll, envir = parent.frame())
  }

  # Output: This is written to global workspace if stargazer_output not set
  models_list <- lapply(fit_names, get)
  se_list <- lapply(se_names, get)
  ll_list <- lapply(ll_names, get)
  

  # Return results
  results <- list(
    "coef" = models_list,
    "se" = se_list,
    "logLik" = ll_list
  ) 
  
  return(results)

}