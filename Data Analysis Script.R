setwd("~/Documents/Danny's Data Analyses")
library(BaylorEdPsych)
library(ppcor)
raw <- read.csv("2018December.csv", stringsAsFactors = FALSE)
colkeep <- c(".*PID$", ".*Age$", ".*AverageFBBias", ".*AverageMCBias", ".*ChangeInLocation", "Egocentric")
data <- raw[,grep(paste(colkeep, collapse = "|"), names(raw))]
Y1data <- data[!is.na(data$ChangeInLocation1), c("PID", "Age", "AverageFBBias", "AverageMCBias", "ChangeInLocation1", "ChangeInLocation2")]
Y1dataNoFailControl <- subset(Y1data, ChangeInLocation2 == 1, select = -ChangeInLocation2)
print(LittleMCAR(Y1data))
#hist(Y1data$AverageFBBias)
#hist(Y1data$AverageMCBias)

paste("Total Paritcipants:", (nrow(Y1data)))
paste("Participants who answered Control Question correctly:", nrow(Y1dataNoFailControl))

print("Spearman between AverageFBBias and ChangeInLocation1")
cor.test(formula = ~ AverageFBBias + ChangeInLocation1, data = Y1dataNoFailControl, method = "spearman")

print("Spearman between AverageFBBias and ChangeInLocation1, Controlling for Age")
pcor.test(Y1dataNoFailControl$AverageFBBias, Y1dataNoFailControl$ChangeInLocation1, Y1dataNoFailControl$Age, method = "spearman")
