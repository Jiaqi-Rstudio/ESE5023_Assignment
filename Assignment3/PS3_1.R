#1.1
Unseeded_Data <- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0)
Seeded_Data <- c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
boxplot(cbind(Unseeded_Data,Seeded_Data)) 
hist(Unseeded_Data)
hist(Seeded_Data)
day <- array(data=1:26)
plot(Unseeded_Data~day)
plot(Seeded_Data~day)
#1.2
t.test(Unseeded_Data, Seeded_Data)
# MingYANG noticed:
# explain it with p-value in report
# the end
