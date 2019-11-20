require("orcutt")
library(perturb)
require(lmtest)
source("C:/Users/andre/OneDrive/Desktop/stat/plots2.R")

trump <- read.csv("C:/Users/andre/OneDrive/Desktop/stat/full_trump2_weekly.csv")
obama <- read.csv("C:/Users/andre/OneDrive/Desktop/stat/full_obama2_weekly.csv")
pres <- obama

linear.model <- lm(Approval~., data=pres)
linear.model.name <- "Linear"
cochrane.model <- cochrane.orcutt(linear.model)
cochrane.model.name <- "Cochrane-Orcutt"

plot_residuals(cochrane.model,"Cochrane-Orcutt")

par(mfrow=c(1,1))
plot(pres$Approval,pch=1, col = "black")
points(cochrane.model$fitted.values,pch=2, col = "blue")
points(linear.model$fitted.values,pch=3, col = "red")

lines(pres$Approval,pch=16, col = "black")
lines(cochrane.model$fitted.values,pch=16, col = "blue")
lines(linear.model$fitted.values,pch=16, col = "red")


# transform to orthogonal space with PCA
pcr <- prcomp(pres[,2:length(pres)], scale=TRUE)
k <- 4 # this choice could be explored in model selection.
V <- pcr$rotation[,1:k] # top k principal directions
x_pc <- pcr$x # rotated X matrix
data_pc <- as.data.frame(cbind(scale(pres$Approval), x_pc)) # combine X with y in new data frame
colnames(data_pc) <- c("y", paste("V", seq(ncol(x_pc)), sep="")) # name the columns

linear.modelpc <- lm(y~., data=data_pc)
colldiag(linear.modelpc) # multicollinearity removed
#final_betas = V%*%linear.modelpc$coefficients[-1] # will be done at the end


#cochrane.modelpc <- cochrane.orcutt(linear.modelpc)
#plot_residuals(cochrane.modelpc, "Cochrane-Orcutt: PC")
#par(mfrow=c(1,1))
#plot(data_pc$Approval,pch=1, col = "black")
#points(cochrane.modelpc$fitted.values,pch=2, col = "blue")
#points(linear.modelpc$fitted.values,pch=3, col = "red")

#lines(data_pcs$Approval,pch=16, col = "black")
#lines(cochrane.modelpc$fitted.values,pch=16, col = "blue")
#lines(linear.modelpc$fitted.values,pch=16, col = "red")

