source("Initialize.R")
source("Universe.R")
source("BBGData.R")
source("DataUtils.R")
source("Factor.R")
source("Industry.R")
source("Model.R")

# use FTSE China A50 Index for testing
Universe <- Universe.Historical(as.Date("2017-02-28"))
Ref.Date <- as.Date("2016-12-30")
Input.Data <- Utils.InputData(Ref.Date)
Result <- Model.Regression(Input.Data)

write.csv(Result$FactorRetCum, file = paste("Result/FactorReturn", format(Ref.Date,"%Y%m%d"), ".csv", sep = ""))
