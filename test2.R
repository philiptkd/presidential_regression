require("orcutt")
library(perturb)
require(lmtest)
require(rms)
source("C:/Users/andre/OneDrive/Desktop/stat/plots2.R")

trump <- read.csv("C:/Users/andre/OneDrive/Desktop/stat/full_trump2_weekly.csv")
obama <- read.csv("C:/Users/andre/OneDrive/Desktop/stat/full_obama2_weekly.csv")
pres <- trump[,-c(2)]

print("Linear Model")
linear.model <- lm(Approval~ Wrong.Track + Unemployment + Stocks + Tweets,data=pres)
linear.model.name <- "Linear"
plot_residuals(linear.model,"Linear Model")
plot(pres$Approval,pch=1, col = "black")
points(linear.model$fitted.values,pch=3, col = "red")
lines(pres$Approval,pch=16, col = "black")
lines(linear.model$fitted.values,pch=16, col = "red")





#print("Linear Model w/ PCA")
# transform to orthogonal space with PCA
#pcr <- prcomp(pres[,2:length(pres)], scale=TRUE)
#k <- 4 # this choice could be explored in model selection.
#V <- pcr$rotation[,1:k] # top k principal directions
#x_pc <- pcr$x # rotated X matrix
#data_pc <- as.data.frame(cbind(scale(pres$Approval), x_pc)) # combine X with y in new data frame
#colnames(data_pc) <- c("Approval", paste("V", seq(ncol(x_pc)), sep="")) # name the columns
#linear.modelpc <- lm(Approval~., data=data_pc)
#colldiag(linear.modelpc) # multicollinearity removed
#final_betas = V%*%linear.modelpc$coefficients[-1] # will be done at the end
#plot_residuals(linear.modelpc, "Linear Model: PC")
#plot(data_pc$Approval,pch=1, col = "black")
#points(linear.modelpc$fitted.values,pch=3, col = "red")
#lines(data_pc$Approval,pch=16, col = "black")
#lines(linear.modelpc$fitted.values,pch=16, col = "red")


par(mfrow=c(1,1))
print("Generalized Least Squares")
mod <- Gls(Approval~ Wrong.Track + Unemployment + Stocks + Tweets,data=pres)
#plot_residuals(mod, "Generalized Least Squares")
plot(pres$Approval,pch=1, col = "black")
points(mod$fitted,pch=2, col = "blue")
lines(pres$Approval,pch=16, col = "black")
lines(mod$fitted,pch=16, col = "blue")
