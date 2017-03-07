source("Initialize.R")
source("Universe.R")
source("BBGData.R")
source("DataUtils.R")
source("Factor.R")

# use FTSE China A50 Index for testing
Universe <- Universe.Historical(as.Date("2017-02-28"))
PX_LAST <- BBGData.Load("PX_LAST","2016")
CHG_PCT <- BBGData.Load("CHG_PCT","2016")
SH_OUT <- BBGData.Load("SH_OUT","2016")
PX_VOL <- BBGData.Load("PX_VOL","2016")

TOT_ASSET <- BBGData.Load("TOT_ASSET","2016")
BOOK_VAL <- BBGData.Load("BOOK_VAL","2016")
DEBT <- BBGData.Load("DEBT","2016")
EV <- BBGData.Load("EV","2016")

BEST_EPS <- BBGData.Load("BEST_EPS","2016")

BOOK_VAL <- BBGData.Load("BOOK_VAL","2015")
EARNING <- BBGData.Load("EARNING","2015")
EARNING <- BBGData.Load("EARNING","2016")