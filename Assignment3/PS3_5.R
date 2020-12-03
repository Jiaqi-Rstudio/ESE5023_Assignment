#5.1
RV_Data <- read.delim("problem5.txt", head=TRUE)
plot(Distance ~ Velocity, data=RV_Data,
     xlab = "Velocity",
     ylab = "Distance",
     main = "Distance vs Velocity",
     pch = "+",
     cex = 2,
     col = "navy")
#5.2
fit <- lm(Distance ~ Velocity, data=RV_Data)
summary(fit)
abline(fit, lwd = 5, col = "red")
#5.3
summary(fit)$coefficients 
#explained by Yuan Li

# MingYANG noticed:
# if you set the intercept to be zero, the slop should be recalculated for the full use of data, instead of translate the line simply 
# the revised answer should be 1.83 billion years
# the end
