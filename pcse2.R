library(Matrix)
pcse2 <- function(object, groupN, groupT, pairwise=TRUE){
  # From https://stackoverflow.com/questions/5541010/xtpcse-from-stata-how-to-rewrite-in-r
  ## Extract basic model info
  groupT <- tail(as.character((match.call()$groupT)), 1)
  groupN <- tail(as.character((match.call()$groupN)), 1)
  dat <- eval(parse(text=object$call$data))

  ## Sanity checks
  if(!"lm" %in% class(object)){stop("Formula object must be of class 'lm'.")}
  if(!groupT %in% colnames(dat)){stop(paste(groupT, 'was not found in data', object$call$data))}
  if(!groupN %in% colnames(dat)){stop(paste(groupN, 'was not found in data', object$call$data))}
  if(anyDuplicated(paste(dat[,groupN], dat[,groupT]))>0){stop(paste('There are duplicate groupN-groupT observations in', object$call$data))}
  if(length(dat[is.na(dat[,groupT]),groupT])>0){stop('There are missing unit indices in the data.')}
  if(length(dat[is.na(dat[,groupN]),groupN])>0){stop('There are missing time indices in the data.')}

  ## Expand model frame to include groupT, groupN, resid columns.
  f <- as.formula(object$call$formula)
  f.expanded <- update.formula(f, paste(". ~ .", groupN, groupT, sep=" + "))
  dat.pcse <- model.frame(f.expanded, dat) 
  dat.pcse$e <- resid(object)  

  ## Extract basic model info (part II)
  N <- length(unique(dat.pcse[,groupN]))
  T <- length(unique(dat.pcse[,groupT]))
  nobs <- nrow(dat.pcse)
  is.balanced <- length(resid(object)) == N * T

  ## If balanced dataset, calculate as in Beck & Katz (1995)
  if(is.balanced){
    dat.pcse <- dat.pcse[order(dat.pcse[,groupN], dat.pcse[,groupT]),]
    X <- model.matrix(f, dat.pcse)
    E <- t(matrix(dat.pcse$e, N, T, byrow=TRUE))
    Omega <- kronecker((crossprod(E) / T), Matrix(diag(1, T)) )

  ## If unbalanced and pairwise, calculate as in Franzese (1996)
  }else if(pairwise==TRUE){
    ## Rectangularize
    rectangle <- expand.grid(unique(dat.pcse[,groupN]), unique(dat.pcse[,groupT]))
    names(rectangle) <- c(groupN, groupT)
    rectangle <- merge(rectangle, dat.pcse, all.x=TRUE)
    rectangle <- rectangle[order(rectangle[,groupN], rectangle[,groupT]),]
    valid <- ifelse(is.na(rectangle$e),0,1) 
    rectangle[is.na(rectangle)] <- 0
    X <- model.matrix(f, rectangle)
    X[valid==0,1] <- 0

    ## Calculate pcse
    E <- crossprod(t(matrix(rectangle$e, N, T, byrow=TRUE)))
    V <- crossprod(t(matrix(valid, N, T, byrow=TRUE)))
    if (length(V[V==0]) > 0){stop("Error! A CS-unit exists without any obs or without any obs in a common period with another CS-unit. You must remove that unit from the data passed to pcse().")}
    Omega <-  kronecker(E/V, Matrix(diag(1, T)))

  ## If unbalanced and casewise, caluate based on largest rectangular subset of data
  }else{ 
    ## Rectangularize
    rectangle <- expand.grid(unique(dat.pcse[,groupN]), unique(dat.pcse[,groupT]))
    names(rectangle) <- c(groupN, groupT)
    rectangle <- merge(rectangle, dat.pcse, all.x=TRUE)
    rectangle <- rectangle[order(rectangle[,groupN], rectangle[,groupT]),]
    valid <- ifelse(is.na(rectangle$e),0,1) 
    rectangle[is.na(rectangle)] <- 0
    X <- model.matrix(f, rectangle)
    X[valid==0,1] <- 0

    ## Keep only years for which we have the max number of observations
    large.panels <- by(dat.pcse, dat.pcse[,groupT], nrow) # How many valid observations per year?
    if(max(large.panels) < N){warning('There is no time period during which all units are observed. Consider using pairwise estimation.')}
    T.balanced <- names(large.panels[large.panels==max(large.panels)]) # Which years have max(valid observations)?
    T.casewise <- length(T.balanced)
    dat.balanced <- dat.pcse[dat.pcse[,groupT] %in% T.balanced,] # Extract biggest rectangular subset
    dat.balanced <- dat.balanced[order(dat.balanced[,groupN], dat.balanced[,groupT]),]
    e <- dat.balanced$e

    ## Calculate pcse as in Beck & Katz (1995)
    E <- t(matrix(dat.balanced$e, N, T.casewise, byrow=TRUE))
    Omega <- kronecker((crossprod(E) / T.casewise), Matrix(diag(1, T)))
  }

  ## Finish evaluation, clean and output
  salami <- t(X) %*% Omega %*% X
  bread <- solve(crossprod(X))
  sandwich <- bread %*% salami %*% bread
  colnames(sandwich) <- names(coef(object))
  row.names(sandwich) <- names(coef(object))
  pcse <- sqrt(diag(sandwich))
  b <- coef(object)
  tstats <- b/pcse
  df <- nobs - ncol(X)
  pval <- 2*pt(abs(tstats), df, lower.tail=FALSE)
  res <- list(vcov=sandwich, pcse=pcse, b=b, tstats=tstats, df=df, pval=pval, pairwise=pairwise, 
              nobs=nobs, nmiss=(N*T)-nobs, call=match.call())
  class(res) <- "pcse"
  return(res)
}