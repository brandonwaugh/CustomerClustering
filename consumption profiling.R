library(data.table)

MeterData <- read.csv("meter-dump.csv")
MeterData <- data.table(MeterData)

MeterData[["Bucket"]] <- ifelse(MeterData[["Consumption"]] < 1,0,ceiling(log(MeterData[["Consumption"]])))

ans <- MeterData[, .(CNT= .N, AVG = mean(Consumption), mn = min(Consumption), mx = max(Consumption), sdv = sd(Consumption)), keyby = Bucket]

ConsumptionSlice <- MeterData[MeterType == 'KWH ' & !is.na(ReadingTime) , 
                              .(AccountNum, YearMonth = substr(ReadingTime, 1,7), Year = substr(ReadingTime,1,4), Month = substr(ReadingTime,6,7), Consumption, Bucket = ifelse(Consumption < 1,0,floor(log(Consumption,2))))]

AccountMonthlyProfile <- ConsumptionSlice[,
                                          .(AccountNum,Month,yearCnt = .N, MnBucket = min(Bucket), MxBucket = max(Bucket), AvgBucket=mean(Bucket), StdBucket = sd(Bucket), AvgCons = mean(Consumption), StdCons = sd(Consumption)),
                                          by = .(AccountNum,Month)]
AccountExtProfile <- ConsumptionSlice[, 
                                      .(AccountNum,Cnt= (.N), MnBucket= min(Bucket), MxBucket = max(Bucket), AvgBucket=mean(Bucket), StdBucket = sd(Bucket), AvgCons = mean(Consumption), StdCons = sd(Consumption)), by = .(AccountNum)]

