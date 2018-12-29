setwd("~/Documents/Danny's Data Analyses")
rm(list = ls())
library(BaylorEdPsych)
library(psych)
raw <- read.csv("2018December.csv", stringsAsFactors = FALSE)
colkeep <- c(".*PID$", ".*Age$", ".*AverageFBBias", ".*AverageMCBias", ".*ChangeInLocation", "Egocentric")
data <- raw[,grep(paste(colkeep, collapse = "|"), names(raw))]
Y1data <- data[!is.na(data$ChangeInLocation1), c("PID", "Age", "AverageFBBias", "AverageMCBias", "ChangeInLocation1", "ChangeInLocation2")]
Y1dataNoFailControl <- subset(Y1data, ChangeInLocation2 == 1, select = -ChangeInLocation2)
Y1dataNoFailControl <- apply(Y1dataNoFailControl, 2, as.numeric)
print(LittleMCAR(Y1data))
#hist(Y1data$AverageFBBias)
#hist(Y1data$AverageMCBias)

sink("Regression.txt")
paste("Total Paritcipants:", (nrow(Y1data)))
paste("Participants who answered Control Question correctly:", nrow(Y1dataNoFailControl))

Y1dataNoFailControl <- as.data.frame(Y1dataNoFailControl)
print("Model for AverageFBBias and ChangeInLocation1")
results1 <- glm(formula = ChangeInLocation1 ~ AverageFBBias, data = Y1dataNoFailControl, family = binomial)
print(summary(results1), short = FALSE)

print("Model for AverageFBBias and ChangeInLocation1, Controlling for Age")
results2 <- glm(formula = ChangeInLocation1 ~ Age * AverageFBBias, data = Y1dataNoFailControl, family = binomial)
print(summary(results2), short = FALSE)

print("Model for AverageFBBias and ChangeInLocation1, Controlling for Age and AverageMCBias")
results3 <- glm(formula = ChangeInLocation1 ~ (AverageMCBias + Age + AverageFBBias)^3, data = Y1dataNoFailControl, family = binomial)
print(summary(results3), short = FALSE)

print("Model for AverageFBBias and ChangeInLocation1, Controlling for AverageMCBias")
results4 <- glm(formula = ChangeInLocation1 ~ AverageMCBias * AverageFBBias, data = Y1dataNoFailControl, family = binomial)
print(summary(results4), short = FALSE)

Y1dataNoFailControl$Combined <- Y1dataNoFailControl$AverageFBBias - Y1dataNoFailControl$AverageMCBias
results4 <- glm(formula = ChangeInLocation1 ~ Combined, data = Y1dataNoFailControl, family = binomial)
print(summary(results4), short = FALSE)
sink()