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
    CHG_PCT <- Utils.CleanData("CHG_PCT", Ref.Date, FALSE, 1)
    
    DATE <- CHG_PCT$DATE
    TEMP <- as.data.frame(zoo::rollmean(subset(CHG_PCT, select = -DATE), 53, na.pad = TRUE, align = "right"))
    #Skipping the most recent two weeks
    TEMP <- rbind(TEMP[1,],head(TEMP,-1))
    Momentum <- cbind.data.frame(DATE, TEMP)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    
    Momentum <- Momentum[Index,]
    
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
    BP_RATIO <- Utils.CleanData("BP_RATIO", Ref.Date, FALSE, 0)
    EP_RATIO <- Utils.CleanData("EP_RATIO", Ref.Date, FALSE, 0)
    CFP_RATIO <- Utils.CleanData("CFP_RATIO", Ref.Date, FALSE, 0)
    SE_RATIO <- Utils.CleanData("SE_RATIO", Ref.Date, FALSE, 0)
    EE_RATIO <- Utils.CleanData("EE_RATIO", Ref.Date, FALSE, 0)
    EEP_RATIO <- Utils.CleanData("EEP_RATIO", Ref.Date, FALSE, 0)
    
    DATE <- BP_RATIO$DATE
    
    Value <- subset(BP_RATIO, select = -DATE) * 0.17 +
      subset(EP_RATIO, select = -DATE) * 0.19 +
      subset(CFP_RATIO, select = -DATE) * 0.07 +
      subset(SE_RATIO, select = -DATE) * 0.15 +
      subset(EE_RATIO, select = -DATE) * 0.21 +
      subset(EEP_RATIO, select = -DATE) * 0.21
    
    Value <- cbind.data.frame(DATE, Value)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    Value <- Value[Index,]
    
    return(Value)
  }

#Most recently announced net dividend divided by the current market price
Factor.Dividend_Yield <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    DVD_YIELD <- Utils.CleanData("DVD_YIELD", Ref.Date, TRUE, 0)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DVD_YIELD$DATE)
    Dividend_Yield <- DVD_YIELD[Index,]
    
    return(Dividend_Yield)
  }

#Combination of the following descriptors:
#Log(Market Capitalization) (32%)
#Log(Sales) (33%)
#Log(Total Assets) (35%)
Factor.Size <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    MKT_CAP <- Utils.CleanData("MKT_CAP", Ref.Date, TRUE, 0)
    SALES <- Utils.CleanData("SALES", Ref.Date, TRUE, 0)
    TOT_ASSET <- Utils.CleanData("TOT_ASSET", Ref.Date, TRUE, 0)
    
    DATE <- MKT_CAP$DATE
    
    Size <- log(subset(MKT_CAP, select = -DATE)) * 0.32 +
      log(subset(SALES, select = -DATE)) * 0.33 +
      log(subset(TOT_ASSET, select = -DATE)) * 0.35
    
    Size <- cbind.data.frame(DATE, Size)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    
    Size <- Size[Index,]
    
    return(Size)
  }

Factor.Trading <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    VOL_RATIO <- Utils.CleanData("VOL_RATIO", Ref.Date, FALSE, 2)
    
    DATE <- VOL_RATIO$DATE
    
    VOL_RATIO <- cbind.data.frame(DATE, lapply(subset(VOL_RATIO, select = -DATE),
                                               (function(x) TTR::EMA(x, n = 504, ratio = log(2)/180))))
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    
    VOL_RATIO <- VOL_RATIO[Index,]
  }

# Utis: Regression Period
Factor.Period <- 
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    filepath <- paste("Factor/Period", format(Ref.Date,"%Y%m%d"), ".RData", sep = "")
    if(file.exists(filepath)){
      load(filepath)
    }else{
      S.Date <- seq(Ref.Date, by = "-1 years", length = 2)[2]
      E.Date <- Ref.Date
      Options <- structure(c("WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                           names = c("periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
      DATE <- bdh("SHSZ300 Index","PX_LAST", start.date = S.Date, end.date = E.Date, options = Options)$date
      
      save(DATE, file = filepath)
    }
    return(DATE)
  }