#Factor functions
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
    
    Index <- match(Ref.Date, DATE)
    Momentum <- Momentum[(Index-53):Index,]
    
    return(Momentum)
  }

#Combination of the following descriptors:
#Book to Price (17%)
#Earnings to Price (19%)
#Cash Flow to Price (7%)
#Sales / EV (15%)
#EBITDA / EV (21%)
# Forecast Earnings to Price (21%)
Factor.Value <- 
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    BP_RATIO <- Utils.CleanData("BP_RATIO", Ref.Date, FALSE, FALSE)
    EP_RATIO <- Utils.CleanData("EP_RATIO", Ref.Date, FALSE, FALSE)
    CFP_RATIO <- Utils.CleanData("CFP_RATIO", Ref.Date, FALSE, FALSE)
    SE_RATIO <- Utils.CleanData("SE_RATIO", Ref.Date, FALSE, FALSE)
    EE_RATIO <- Utils.CleanData("EE_RATIO", Ref.Date, FALSE, FALSE)
    EEP_RATIO <- Utils.CleanData("EEP_RATIO", Ref.Date, FALSE, FALSE)
    
  }