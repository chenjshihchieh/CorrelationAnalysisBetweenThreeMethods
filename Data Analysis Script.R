setwd("~/Documents/Danny's Data Analyses")
raw <- read.csv("2018December.csv", stringsAsFactors = FALSE)
colkeep <- c(".*PID$", ".*Age$", ".*AverageFBBias", ".*AverageMCBias", ".*ChangeInLocation", "Egocentric")
data <- raw[,grep(paste(colkeep, collapse = "|"), names(raw))]
Y1data <- data[!is.na(data$ChangeInLocation1), c("PID", "Age", "AverageFBBias1", "AverageFBBias2", "AverageFBBias", "AverageMCBias1", "AverageMCBias2", "AverageMCBias", "ChangeInLocation1", "ChangeInLocation2")]