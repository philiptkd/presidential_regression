extract_data <- function(model) {
  data <- eval(model$call$data)
  return(data)
}

setup_model_obj <- function(mod) {
  
  model <- mod
  # Get original dataframe to help identify components
  model.data <- extract_data(model)
  
  # Coefficients
  model.coef           <- coefficients(model)
  model.coef.beta      <- model.coef[-1]
  model.coef.intercept <- model.coef[1]

  model.residuals    <- residuals(model)[2:length(residuals(model))]
  model.fitted       <- model$fitted.values

  # Response & Regressors 
  if(class(model) == "orcutt"){
    model.X <- as.matrix(model$model$XB[,-1]) # remove intercept
    model.Y <- as.matrix(model$model$YB)
    model.p <- length(model.coef.beta)
    model.n <- length(model.residuals)
    # Calculate R-Student
    model.sigmahat <- t(model.residuals) %*% model.residuals / (model.n - model.p) 
    model.H        <- model.X %*% (t(model.X) %*% model.X )^(-1) %*% t(model.X)
    model.rstudent <- model.residuals / (model.sigmahat * sqrt(1-diag(model.H)))
  }
  else{
    model.X <- as.matrix(model$model[,2:length(model$model)]) # remove intercept
    model.Y <- as.matrix(model$model[,1])
    model.p <- length(model.coef.beta)
    model.n <- length(model.residuals)
    model.rstudent = rstudent(model)
  }

  # Labeling
  model.labels   <- names(model.coef)
  model.labels.X <- names(model.coef)[-1] # Remove intercept from labels
  model.labels.Y <- toString(eval(model$call$formula)[2])
  return(model)
}

plot_residuals <- function(mod,mod.name) {
  
  model <- mod
  model.name <- mod.name
  # Get original dataframe to help identify components
  model.data <- extract_data(model)
  # Coefficients
  model.coef           <- coefficients(model)
  model.coef.beta      <- model.coef[-1]
  model.coef.intercept <- model.coef[1]
  
  # Response & Regressors 
  if(class(model) == "orcutt"){
    print("Orcutt Model")
    model.residuals    <- residuals(model)[-1]#[1:length(residuals(model))]
    model.fitted       <- model$fitted.values[-1]
    model.p <- length(model.coef.beta)
    model.n <- length(model.residuals)
    model.X <- as.matrix(model$model$XB[,-1]) # remove intercept
    model.Y <- as.matrix(model$model$YB)
    print(model)
    # Calculate R-Student
    model.sigmahat <- t(model.residuals) %*% model.residuals / (model.n - model.p) 
    model.H        <- model.X %*% (t(model.X) %*% model.X )^(-1) %*% t(model.X)
    #print(diag(model.H))
    model.rstudent <- model.residuals / (model.sigmahat * sqrt(1-diag(model.H)))
    print(model.residuals)
    print(diag(model.H))
    print(length(diag(model.H)))
  }
  else{
    model.residuals    <- residuals(model)
    model.fitted       <- model$fitted.values
    model.p <- length(model.coef.beta)
    model.n <- length(model.residuals)
    model.X <- as.matrix(model$model[,2:length(model$model)]) # remove intercept
    model.Y <- as.matrix(model$model[,1])
    model.rstudent = rstudent(model)
  }
  
  # Labeling
  model.labels   <- names(model.coef)
  model.labels.X <- names(model.coef)[-1] # Remove intercept from labels
  model.labels.Y <- toString(eval(model$call$formula)[2])

  ###############################################################################################################################################################
  #                                                                 QQ Plots  
  qqnorm(model.rstudent) # heavy-tailed. transformation or robust regression?
  qqline(model.rstudent)
  ###############################################################################################################################################################
  
  
  
  par(mfrow=c(3,2))
  ###############################################################################################################################################################
  #                                                               Residual Plots
  #
  labelpart <- paste(model.name,"Residuals:") # reused
  #
  #               Residuals vs. Fitted Values
  #
  Mlabel <- paste(labelpart,"fitted-values") # main=
  Ylabel <- "RStudent Residuals" # ylab=
  Xlabel <- model.labels.Y # xlab=
  #
  Y <- model.rstudent
  X <- model.fitted
  #
  plot(X, Y, ylab=Ylabel, xlab=Xlabel, main=model.name)
  #
  #               Residuals vs. Regressors
  #
  print("Residuals vs. Regressors")
  for(i in seq(model.p) ) {
    Mlabel <- paste(labelpart, model.labels.X[i]) 
    Xlabel <- model.labels.X[i]
    X      <- model.X[,i]
    plot(X, Y, ylab=Ylabel, xlab=Xlabel, main=Mlabel)
  }
  #
  ###############################################################################################################################################################
  
  
  ###############################################################################################################################################################
  #                                                          Partial Regression Plots
  #
  labelpart <- paste(model.name,"Partial Regression:") # reused
  #
  #               Partial Regression of Regressors
  #
  Mlabel <- paste(labelpart,"fitted-values") # main=
  Ylabel <- "RStudent Residuals" # ylab=
  #
  #
  print("Partial Regression")
  par(mfrow=c(3,2))
  for(j in seq(model.p) ) {
    i <- j
    Xformula  <- as.formula(paste(model.labels.X[i], "~."  ))
    Yformula <- as.formula(paste(model.labels.Y, "~."  ))
    Yresponse   <- lm(Yformula, data=model.data[,-c(i+1)])
    Xresponse   <- lm(Xformula, data=model.data[,-c(1)])
    xlab <- paste("e(", names(model.data)[i+1], "|", paste(model.labels[-c(1, i+1)], collapse=","), ")", sep="")
    ylab <- paste("e(", names(model.data)[1], "|", paste(model.labels[-c(1, i+1)], collapse=","), ")", sep="")
    
    if(class(model) == "orcutt"){
      x <- cochrane.orcutt(Xresponse,max.iter = 600)
      y <- cochrane.orcutt(Yresponse,max.iter = 600)
      if(length(residuals(  y )) != length(residuals(  x ))){
        plot(residuals( x ), residuals(  y )[-1], xlab=xlab, ylab=ylab, main="Cochrane-Orcutt")
      }
      else{
        plot(residuals( x ), residuals(  y ), xlab=xlab, ylab=ylab, main="Cochrane-Orcutt")
      }

      
    }
    else{
      print("lm")
      plot(residuals(Xresponse), residuals(Yresponse), xlab=xlab, ylab=ylab, main="LM")
    }
  }
  ###############################################################################################################################################################
  
  par(ask=FALSE)
}
