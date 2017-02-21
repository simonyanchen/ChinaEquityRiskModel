#TBD
#Cumulative return over one year (averaged),
#Skipping the most recent two weeks to mitigate the price reversal effect
Factor.Momentum <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    CHG_PCT <- Utils.CleanData("CHG_PCT", Ref.Date, FALSE, TRUE)
    
    DATE <- CHG_PCT$DATE
    TEMP <- as.data.frame(zoo::rollmean(subset(CHG_PCT, select = -DATE), 53, na.pad = TRUE, align = "right"))
    #Skipping the most recent two weeks
    TEMP <- rbind(TEMP[1,],head(TEMP,-1))
    Momentum <- cbind.data.frame(DATE, TEMP)
    
    ret(Momentum)
  }