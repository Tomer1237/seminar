
library(lmtest)
library(sandwich)
dataaverage <- read.csv("C:/seminar/dataaverage.csv")

dataaverage$interaction_term <- dataaverage$group * dataaverage$aver_after

aver_before <- "aver_pre"
aver_after <- "aver_after"

model <- lm((aver_after) ~ group + get(aver_before) + interaction_term, data = dataaverage)
robust_model <- coeftest(model, vcov = vcovHC)
summary(robust_model)
print(summary(robust_model))


