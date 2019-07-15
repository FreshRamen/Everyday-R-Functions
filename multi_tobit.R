multi_tobit <- function(..., stargazer_file=NULL) {
  # Function performs regressions for multiple regression equations specified as ...
  # returns lists with results for use with stargazer or outputs results straight 
  # as an html stargazer_file
  
  # Regression equations
  reg_eqs <- list(...) # List of all regression equations defined above

  # Names
  # These vectors contain names for things computed for each equation
  fit_n <- seq_along(reg_eqs) # Sequence from one to n along equations, for naming the results
  fit_names <- paste("fit", fit_n, sep=".") # Regression outputs
  se_names <- paste("se", fit_n, sep=".") # Robust standard errors
  ll_names <- paste("ll", fit_n, sep=".") # Log likelihoods

  for (j in fit_n) {
    eq <- reg_eqs[[j]] # Select regression equation
    fit <- vglm(eq, data=DT) # Estimate regression
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
  
  # Stargazer output
  if (!is.null(stargazer_file)) {   # Only if file path is provided

    # col_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")

    quietly <- capture.output( # Produce stargazer output but capture its HTML
      stargazer(models_list,
        type="html",
        out=stargazer_file,
        report="vc*sp",
        dep.var.labels.include=FALSE,
        omit="years",
        single.row=FALSE,
        intercept.bottom=FALSE,
        # covariate.labels=var_names,
        # dep.var.labels="Capex",
        # column.labels=col_names,
        se=se_list
        # add.lines=
      )
    )
    htmltools::includeHTML(stargazer_file)
    ll_list <<- ll_list # Store log likelihood results in parent frame
    
  } else {
    # If no file is specified, store all results in parent frame
    models_list <<- models_list
    se_list <<- se_list
    ll_list <<- ll_list
  }
}