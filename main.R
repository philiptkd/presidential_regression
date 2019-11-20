trump <- read.csv("~/Desktop/Regression/project/data/full_trump2_weekly.csv")
obama <- read.csv("~/Desktop/Regression/project/data/full_obama2_weekly.csv")
pres <- obama

# getting squared terms
x <- pres[-1]
y <- pres[1]
x2 <- x^2
f <- function(name) paste(name,"2",sep="")
colnames(x2) <- lapply(colnames(x), f)
data_sq <- cbind(y, x, x2)
  
# initial fit
fit0 <- lm(Approval~., data=data_sq[1:length(pres)]) # only use linear terms for now
summary(fit0) # obama R2 = 0.547
plot(data_sq$Approval, type="l", col="red", xlab="Week of Presidency", ylab="% Approval", main="Obama LSE Fit")
lines(fitted(fit0), type="l", col="blue")
legend("topright", legend=c("True", "Predicted"), col=c("red", "blue"), lwd=c(2.5, 2.5))

source("~/Desktop/Regression/project/plots.R")
#plot_data(data_sq)
#plot_residuals(data_sq, fit0)
qqnorm(rstandard(fit0), main="Trump Normal Residuals")
qqline(rstandard(fit0))
plot(predict(fit0), rstudent(fit0), ylab="RStudent Residuals", xlab="y_hat", main="Trump d vs y_hat")

# test for and correct multicollinearity before doing partial residual analysis
# variance decomposition
library(perturb)
colldiag(fit0) # dang


# eigensystem analysis
X <- scale(data_sq[,2:length(data_sq)])
ee <- eigen(t(X)%*% X)
ee$values
ee$vectors

# transform to orthogonal space with PCA
pcr <- prcomp(data_sq[,2:length(data_sq)], scale=TRUE)
x_pc <- pcr$x # rotated X matrix
data_pc <- as.data.frame(cbind(scale(data_sq$Approval), x_pc)) # combine X with y in new data frame
colnames(data_pc) <- c("y", paste("V", seq(ncol(x_pc)), sep="")) # name the columns
fit1 <- lm(y~., data=data_pc)
colldiag(fit1) # multicollinearity removed
#plot_residuals(data_pc, fit1) # V1 residuals are definitely not normal

# influence test
barplot(dffits(fit1), ylab="DFFITS", xlab="data point", main="Trump Influential Points")
dffits_cutoff <- 2*sqrt(length(pres)/nrow(pres)) # use original number of regressors
abline(h=dffits_cutoff, lty=2)
abline(h=-dffits_cutoff, lty=2)
influential_points <- as.vector(which(abs(dffits(fit1)) > dffits_cutoff)) # gets points past the cutoff
data_clean <- data_pc[-influential_points,]
fit2 <- lm(y~., data=data_clean)
#plot_residuals(data_clean, fit2) # much better
summary(fit2)
anova(fit2)
qqnorm(rstandard(fit2), main="Trump Normal Residuals")
qqline(rstandard(fit2))
plot(predict(fit2), rstudent(fit2), ylab="RStudent Residuals", xlab="y_hat", main="Trump d vs y_hat")

# variable selection
library(cvTools)
Cols <- names(data_clean[-1])
n <- length(Cols)
id <- unlist(lapply(1:n, function(i)combn(1:n,i,simplify=FALSE) ), recursive=FALSE) # all binary vectors
Formulas <- sapply(id, function(i) paste("y~", paste(Cols[i],collapse="+") )) # all formulas
Fits <- lapply(Formulas, function(i) lm(as.formula(i), data=data_clean)) # all fits
R2adj <- lapply(Fits, function(i) summary(i)$adj.r.squared) # get adjusted R2
PRESS <- lapply(Fits, function(i) sum((residuals(i)/(1 - influence(i)$hat))^2)) # PRESS statistic
Pred_Err <- lapply(Fits, function (i) 
  cvFit(i, data=data_clean, y=data_clean[1], x=data_clean[-1], K=10, R=3, seed=123)$cv) # cross validation

# restrict search to fits with all significant regressors
is_sig <- lapply(Fits, function(i) all(anova(i)$`Pr(>F)`[1:i$rank-1] < 0.05))
sig_idxs <- which(unlist(is_sig, use.names=FALSE))

# choose best fit as the one with minimum prediction error
Pred_Err <- as.data.frame(Pred_Err)
min_pred_id <- which(Pred_Err[sig_idxs] == min(Pred_Err[sig_idxs]))
best_pred_regressors <- id[sig_idxs][min_pred_id][[1]]
best_fit <- Fits[sig_idxs][min_pred_id][[1]]
best_regressors <- names(data_clean[-1])[best_pred_regressors]
formula <- paste("y~", paste(best_regressors, collapse="+"))
summ <- summary(best_fit)

# get best fit statistics
best_r2adj <- summ$adj.r.squared
best_press <- sum((residuals(best_fit)/(1 - influence(best_fit)$hat))^2)
best_pred_err <- min(Pred_Err)
SST <- sum( (data_clean$y - mean(data_clean$y))^2 )
best_r2press <- 1 - best_press/SST
best_r2pred <- 1 - n*(best_pred_err)^2/SST

# best fit plots
qqnorm(rstandard(best_fit), main="Obama Normal Residuals")
qqline(rstandard(best_fit))
plot(predict(best_fit), rstudent(best_fit), ylab="RStudent Residuals", xlab="y_hat", main="Obama d vs y_hat")

# tranform solution back to original space for interpretability
non_zero <- summ$coefficients[,1]
coeff <- vector("numeric", length=n)
for(i in 1:n) {
  name <- paste("V",i,sep="")
  if(name %in% best_regressors) coeff[i] <- non_zero[name]
}
betas <- pcr$rotation%*%coeff # undo pcr rotation. betas for normalized space
X_scaled <- scale(data_sq[-1])
y_scaled <- scale(data_sq$Approval)
y_stddev <- sqrt(var(data_sq$Approval))
y_mean <- mean(data_sq$Approval)
plot(data_sq$Approval, type="l", col="red")
lines(X_scaled%*%betas*y_stddev + y_mean, col="blue")
