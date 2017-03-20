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
#Forecast Earnings to Price (21%)
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
Factor.DivYld <-
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
    DivYld <- DVD_YIELD[Index,]
    return(DivYld)
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

#The exponential weighted average (EWMA) of the ratio of shares traded to shares outstanding
Factor.Trade <-
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
    Trade <- VOL_RATIO[Index,]
    return(Trade)
  }

#Combination of the following descriptors:
Factor.EarnVariab <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    
    INCOME <- Utils.CleanData("INCOME", Ref.Date, FALSE, 5)
    CASH_FLOW <- Utils.CleanData("CASH_FLOW", Ref.Date, FALSE, 5)
    SALES <- Utils.CleanData("SALES", Ref.Date, FALSE, 5)
    TOT_ASSET <- Utils.CleanData("TOT_ASSET", Ref.Date, FALSE, 5)
    
    DATE <- INCOME$DATE
    
    sd_Income <- as.data.frame(zoo::rollapply(subset(INCOME, select = -DATE), 265, sd, fill = NA, align = "right"))
    sd_CF <- as.data.frame(zoo::rollapply(subset(CASH_FLOW, select = -DATE), 265, sd, fill = NA, align = "right"))
    sd_Sales <- as.data.frame(zoo::rollapply(subset(SALES, select = -DATE), 265, sd, fill = NA, align = "right"))
    md_Asset <- as.data.frame(zoo::rollapply(subset(TOT_ASSET, select = -DATE), 265, function(x) median(x, na.rm = TRUE), fill = NA, align = "right"))
    
    EarnVariab <- cbind.data.frame(DATE, (sd_Income * 0.34 + sd_CF * 0.33 + sd_Sales * 0.33) / md_Asset)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    EarnVariab <- EarnVariab[Index,]
    return(EarnVariab)
  }

#Combination of the following descriptors:
#Return on Equity (28%)
#Return on Asset (28%)
#Return on Capital Employed (28%)
#EBITDA Margin (16%)
Factor.Profit <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    INCOME <- Utils.CleanData("INCOME", Ref.Date, FALSE, 0)
    BOOK_VAL <- Utils.CleanData("BOOK_VAL", Ref.Date, FALSE, 0)
    TOT_ASSET <- Utils.CleanData("TOT_ASSET", Ref.Date, FALSE, 0)
    
    DATE <- INCOME$DATE
    #Temp
    Profit <- subset(INCOME, select = -DATE) / subset(BOOK_VAL, select = -DATE) * 0.5 +
      subset(INCOME, select = -DATE) / subset(TOT_ASSET, select = -DATE) * 0.5
    Profit <- cbind.data.frame(DATE, Profit)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    Profit <- Profit[Index,]
    return(Profit)
  }

#Combination of the following descriptors:
#Rolling Volatility (29%)
#Rolling CAPM Beta (24%)
#Historical Sigma (25%)
#Cumulative Range (22%)
Factor.Volatility <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    CHG_PCT <- Utils.CleanData("CHG_PCT", Ref.Date, FALSE, 1)
    PX_LAST <- Utils.CleanData("PX_LAST", Ref.Date, TRUE, 1)
    BETA <- Utils.CleanData("BETA", Ref.Date, TRUE, 0)
    SIGMA <- Utils.CleanData("SIGMA", Ref.Date, TRUE, 0)
    
    DATE <- CHG_PCT$DATE
    #################
    #Need normalizing
    #################
    Vol <- as.data.frame(zoo::rollapply(subset(CHG_PCT, select = -DATE), 53, sd, fill = NA, align = "right")) * sqrt(53) / 100
    Range <- as.data.frame(zoo::rollmax(subset(CHG_PCT, select = -DATE), 53, sd, fill = NA, align = "right") /
                            -zoo::rollmax(-subset(PX_LAST, select = -DATE), 53, sd, fill = NA, align = "right"))
    
    Volatility1 <- Vol * 0.29 + Range * 0.22
    Volatility1 <- cbind.data.frame(DATE, Volatility1)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    Volatility1 <- Volatility1[Index,]
    
    DATE <- BETA$DATE
    Volatility2 <- subset(BETA, select = -DATE) * 0.24 +
      subset(SIGMA, select = -DATE) * 0.25
    Volatility2 <- cbind.data.frame(DATE, Volatility2)
    Index <- match(Period, DATE)
    Volatility2 <- Volatility2[Index,]
    
    DATE <- Period
    Volatility <- cbind.data.frame(DATE, subset(Volatility1, select = -DATE) + subset(Volatility2, select = -DATE))
    
    return(Volatility)
  }

#Combination of the following descriptors:
#Total Asset Growth (26%)
#Sales Growth (19%)
#Earnings Growth (23%)
#Forecast of Earnings Growth (11%)
#Forecast of Sales Growth (21%)
Factor.Growth <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    TOT_ASSET <- Utils.CleanData("TOT_ASSET", Ref.Date, FALSE, 5)
    SALES <- Utils.CleanData("SALES", Ref.Date, FALSE, 5)
    INCOME <- Utils.CleanData("INCOME", Ref.Date, FALSE, 5)
    
    TAG <- as.data.frame(zoo::rollapply(subset(TOT_ASSET, select = -DATE), 265, function(x) (tail(x,1) - head(x,1)), fill = NA, align = "right")) /
      as.data.frame(zoo::rollsum(subset(TOT_ASSET, select = -DATE), 265, na.pad = TRUE, align = "right"))
    SG <- as.data.frame(zoo::rollapply(subset(SALES, select = -DATE), 265, function(x) (tail(x,1) - head(x,1)), fill = NA, align = "right")) /
      as.data.frame(zoo::rollsum(subset(SALES, select = -DATE), 265, na.pad = TRUE, align = "right"))
    IG <- as.data.frame(zoo::rollapply(subset(INCOME, select = -DATE), 265, function(x) (tail(x,1) - head(x,1)), fill = NA, align = "right")) /
      as.data.frame(zoo::rollsum(subset(INCOME, select = -DATE), 265, na.pad = TRUE, align = "right"))
    
    
  }

#Utils: Regression Period
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