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
Y1dataNoFailControl <- as.data.frame(Y1dataNoFailControl)
#print(LittleMCAR(Y1data))
#hist(Y1data$AverageFBBias)
#hist(Y1data$AverageMCBias)

sink("Logistic Regression.txt")
paste("Total Paritcipants:", (nrow(Y1data)))
paste("Participants who answered Control Question correctly:", nrow(Y1dataNoFailControl))

cat("\n", "\n", "\n", "\n", "\n", "Model for AverageFBBias and ChangeInLocation1", "\n")
results1 <- glm(formula = ChangeInLocation1 ~ AverageFBBias, data = Y1dataNoFailControl, family = binomial)
print(summary(results1), short = FALSE)

cat("\n", "\n", "\n", "\n", "\n", "Model for AverageFBBias and ChangeInLocation1, Controlling for Age", "\n")
results2 <- glm(formula = ChangeInLocation1 ~ Age * AverageFBBias, data = Y1dataNoFailControl, family = binomial)
print(summary(results2), short = FALSE)

cat("\n", "\n", "\n", "\n", "\n", "Model for AverageFBBias and ChangeInLocation1, Controlling for Age and AverageMCBias", "\n")
results3 <- glm(formula = ChangeInLocation1 ~ (AverageMCBias + Age + AverageFBBias)^3, data = Y1dataNoFailControl, family = binomial)
print(summary(results3), short = FALSE)

cat("\n", "\n", "\n", "\n", "\n", "Model for AverageFBBias and ChangeInLocation1, Controlling for Age and AverageMCBias", "\n")
results4 <- glm(formula = ChangeInLocation1 ~ AverageMCBias + Age + AverageFBBias, data = Y1dataNoFailControl, family = binomial)
print(summary(results4), short = FALSE)

cat("\n", "\n", "\n", "\n", "\n", "Model for AverageFBBias - AverageMCBias and ChangeInLocation1", "\n")
Y1dataNoFailControl$Combined <- Y1dataNoFailControl$AverageFBBias - Y1dataNoFailControl$AverageMCBias
results4 <- glm(formula = ChangeInLocation1 ~ Combined, data = Y1dataNoFailControl, family = binomial)
print(summary(results4), short = FALSE)
sink()