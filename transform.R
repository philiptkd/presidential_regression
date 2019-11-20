data <- data_positive
fit <- fit3
i <- 1

boxtidwell <- function(data, fit, i, delta=0.01) { # i is index of regressor to transform
  i <- i + 1 # index of column in data frame
  alpha_old <- 1
  v <- as.data.frame(data[i])
  v <- v^1.0

  repeat{
    w <- v*log(v)
    formula <- paste("y~", paste(colnames(data)[-c(1,i)], collapse="+"), "+v+w", sep="")
    fit2 <- lm(formula, data=data)
    summary(fit2)
    alpha_new <- alpha_old + fit2$coefficients["w"] / fit$coefficients[i]
    v <- v^alpha_new
    formula <- paste("y~", paste(colnames(data)[-c(1,i)], collapse="+"), "+v", sep="")
    fit <- lm(formula, data=data)
    summary(fit)
    
    if(abs(alpha_new - alpha_old)<delta){
      print("converged")
      print(alpha_new)
      break
    }
    if(abs(alpha_new)>10) {
      print("diverged")
      break
    }
    
    alpha_old <- alpha_new
  }
}