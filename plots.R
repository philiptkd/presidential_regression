plot_residuals <- function(data, fit, ar=FALSE) {
  ######not working
  if(ar==TRUE) {
    library(orcutt)
    fit <- lm(cochrane.orcutt(fit))
    data <- data[-1,]
  }
  
  # normal plot of residuals
  qqnorm(rstandard(fit)) # heavy-tailed. transformation or robust regression?
  qqline(rstandard(fit))
  
  par(ask=TRUE)
  # residuals vs y and x. outlier on every plot
  fittedValues<-predict(fit)
  plot(fittedValues, rstudent(fit),ylab="RStudent Residuals", xlab=names(data)[1])
  for(i in seq(2,length(data))) {
    plot(data.frame(data[i], rstudent(fit)),ylab="RStudent Residuals", xlab=names(data)[i])
  }
  
  # partial regression plots
  for(i in seq(2,length(data))) {
    fity <- lm(as.formula(paste(names(data)[1], "~.")), data=data[-c(i)])
    fitx <- lm(as.formula(paste(names(data)[i], "~.")), data=data[-c(1)])
    xlab <- paste("e(", names(data)[i], "|", paste(names(data)[-c(1, i)], collapse=","), ")", sep="")
    ylab <- paste("e(", names(data)[1], "|", paste(names(data)[-c(1, i)], collapse=","), ")", sep="")
    plot(residuals(fitx), residuals(fity), xlab=xlab, ylab=ylab)
  }
  
  # time sequence of residuals
  plot(rstudent(fit), xlab="Week of Presidency", ylab="Rstudent Residuals") # beginning and end have issues
  # positive autocorrelation
  
  par(ask=FALSE)
}

# plot regressors
plot_data <- function(pres) {
  plot(data.frame(seq(1:nrow(pres)), pres[1]), type="l", xlab="Week of Presidency", ylab=names(pres)[1])
  par(ask=TRUE)
  for(i in seq(2,length(pres))) {
    plot(data.frame(seq(1:nrow(pres)), pres[i]), type="l", xlab="Week of Presidency", ylab=names(pres)[i])
  }
  
  plot(data.frame(seq(1:nrow(pres)), pres[2]-pres[3]), type="l", xlab="Week of Presidency", 
       paste(ylab=names(pres)[2], sep = " - ", names(pres)[3]))
  par(ask=FALSE)
}