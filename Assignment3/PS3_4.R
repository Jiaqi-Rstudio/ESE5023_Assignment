temp_Data <- read.delim("problem4.txt", head=TRUE)
names(temp_Data)
temp_Data1 <- temp_Data%>%
  mutate(Elevation..km.=Elevation..m./1000)
plot(Temperature..degrees.C. ~ Elevation..km., data=temp_Data1,
     xlab = "Elevation",
     ylab = "Temperature",
     main = "Temperature vs Elevation",
     pch = "+",
     cex = 2,
     col = "navy")
fit <- lm(Temperature..degrees.C. ~ Elevation..km., data=temp_Data1)
summary(fit)
abline(fit, lwd = 5, col = "red")
# good work
