library(MASS)
data(cpus)
#6.1
sample_index <- sample(nrow(cpus),nrow(cpus)*0.80)
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]
model_1 <- lm(perf~syct+mmin+mmax+cach+chmin+chmax, data=cpus_train)
summary(model_1)
library(leaps)
subset_result <- regsubsets(perf~syct+mmin+mmax+cach+chmin+chmax, data=cpus_train, nbest=2, nvmax = 6)
plot(subset_result, scale="bic")
#6.2
perf_predict <- predict(model_1,cpus_test)
plot(cpus_test$perf, perf_predict)
cor(cpus_test$perf, perf_predict)
# Mean predicted value
mean(perf_predict)

# Mean actual value
mean(cpus_test$perf)

# Relative mean bias
(mean(perf_predict) - mean(cpus_test$perf))/
  mean(cpus_test$perf)*100
