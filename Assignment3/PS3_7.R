#7.1
EX_Data <- read.delim("problem7.txt", head=TRUE)
CE2_Data <- EX_Data %>%
  filter(NO == "CE2")
CE3_Data <- EX_Data %>%
  filter(NO == "CE3")
sample1 <- CE2_Data$NO3
sample2 <- CE3_Data$NO3
t.test(sample1, sample2)
#7.2
anova_one_way <- aov(NO3 ~ NO, data = EX_Data)
summary(anova_one_way)
TukeyHSD(anova_one_way)
#7.3
vEX_Data <- read.delim("problem7_3.txt", head=TRUE)
names(vEX_Data)
fit <- lm (R_nit~Flow.rate, data = vEX_Data)
summary(fit)
plot(R_nit~Flow.rate, data = vEX_Data,
     xlab = "Flow.rate",
     ylab = "R_nit",
     main = "R_nit vs Flow.rate",
     pch = 20,
     cex = 2,
     col = "navy")
abline(fit, lwd = 5, col = "red")
