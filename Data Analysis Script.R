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
sink(file = "SpearmanOutput.txt")
print(paste("Total Paritcipants:", (nrow(Y1data))))
print(paste("Participants who answered Control Question correctly:", nrow(Y1dataNoFailControl)))

cat("\n", "\n", "\n", "\n", "\n", "Spearman between AverageFBBias and ChangeInLocation1", "\n")
cor.results <- cor.test(formula = ~ AverageFBBias + ChangeInLocation1, data = Y1dataNoFailControl, method = "spearman")
print(cor.results)

cat("\n", "\n", "\n", "\n", "\n", "Spearman between AverageFBBias and ChangeInLocation1, Controlling for Age", "\n", "\n")
part1.r <- partial.r(data = Y1dataNoFailControl, x = c("AverageFBBias", "ChangeInLocation1"), y = "Age", method = "spearman")
part1.cor <- corr.p(part1.r, n = {nrow(Y1dataNoFailControl) - 1})
print(part1.cor, short = F)  

cat("\n", "\n", "\n", "\n", "\n", "Spearman between AverageFBBias and ChangeInLocation1, Controlling for Age and AverageMCBias", "\n", "\n")
part2.r <- partial.r(data = Y1dataNoFailControl, x = c("AverageFBBias", "ChangeInLocation1"), y = c("Age", "AverageMCBias"), method = "spearman")
part2.cor <- corr.p(part2.r, n = {nrow(Y1dataNoFailControl) - 1})
print(part2.cor, short = F) 

cat("\n", "\n", "\n", "\n", "\n", "Spearman between AverageFBBias - AverageMCBias and ChangeInLocation1", "\n")
Y1dataNoFailControl$Combined <- Y1dataNoFailControl$AverageFBBias - Y1dataNoFailControl$AverageMCBias
combined.result <- cor.test(formula = ~ Combined + ChangeInLocation1, data = Y1dataNoFailControl, method = "spearman")
print(combined.result, short = F) 

sink()