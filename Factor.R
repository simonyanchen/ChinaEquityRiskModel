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
    Momentum <- Utils.Normalize(Momentum)
    
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
    Value <- Utils.Normalize(Value)
    
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
    DivYld <- Utils.Normalize(DivYld)
    
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
    Size <- Utils.Normalize(Size)
    
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
    Trade <- Utils.Normalize(Trade)
    
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
    TOT_ASSET <- Utils.CleanData("TOT_ASSET", Ref.Date, TRUE, 5)
    
    DATE <- INCOME$DATE
    
    sd_Income <- as.data.frame(zoo::rollapply(subset(INCOME, select = -DATE), 265, sd, fill = NA, align = "right"))
    sd_CF <- as.data.frame(zoo::rollapply(subset(CASH_FLOW, select = -DATE), 265, sd, fill = NA, align = "right"))
    sd_Sales <- as.data.frame(zoo::rollapply(subset(SALES, select = -DATE), 265, sd, fill = NA, align = "right"))
    md_Asset <- as.data.frame(zoo::rollapply(subset(TOT_ASSET, select = -DATE), 265, function(x) median(x, na.rm = TRUE), fill = NA, align = "right"))
    
    EarnVariab <- cbind.data.frame(DATE, (sd_Income * 0.34 + sd_CF * 0.33 + sd_Sales * 0.33) / md_Asset)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    EarnVariab <- EarnVariab[Index,]
    EarnVariab <- Utils.Normalize(EarnVariab)
    
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
    Profit <- Utils.Normalize(Profit)
    
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

    Vol <- as.data.frame(zoo::rollapply(subset(CHG_PCT, select = -DATE), 53, sd, fill = NA, align = "right")) * sqrt(53) / 100
    Range <- as.data.frame(zoo::rollmax(subset(CHG_PCT, select = -DATE), 53, sd, fill = NA, align = "right") /
                            -zoo::rollmax(-subset(PX_LAST, select = -DATE), 53, sd, fill = NA, align = "right"))
    
    Volatility1 <- Vol * 0.29 + Range * 0.22
    Volatility1 <- cbind.data.frame(DATE, Volatility1)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    Volatility1 <- Volatility1[Index,]
    #Normalized
    Volatility1 <- Utils.Normalize(Volatility1)
    
    DATE <- BETA$DATE
    Volatility2 <- subset(BETA, select = -DATE) * 0.24 +
      subset(SIGMA, select = -DATE) * 0.25
    Volatility2 <- cbind.data.frame(DATE, Volatility2)
    Index <- match(Period, DATE)
    Volatility2 <- Volatility2[Index,]
    #Normalized
    Volatility2 <- Utils.Normalize(Volatility2)
    
    DATE <- Period
    Volatility <- cbind.data.frame(DATE, subset(Volatility1, select = -DATE) + subset(Volatility2, select = -DATE))
    Volatility <- Utils.Normalize(Volatility)
    
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
    
    DATE <- TOT_ASSET$DATE
    Growth1 <- TAG * 0.26 + SG * 0.19 + IG * 0.23
    Growth1 <- cbind.data.frame(DATE, Growth1)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    Growth1 <- Growth1[Index,]
    
    BEST_EPS1 <- Utils.CleanData("BEST_EPS1", Ref.Date, FALSE, 0)
    BEST_EPS2 <- Utils.CleanData("BEST_EPS2", Ref.Date, FALSE, 0)
    BEST_SALES1 <- Utils.CleanData("BEST_SALES1", Ref.Date, FALSE, 0)
    BEST_SALES2 <- Utils.CleanData("BEST_SALES2", Ref.Date, FALSE, 0)
    
    BEG <- as.data.frame(subset(BEST_EPS2, select = -DATE)/subset(BEST_EPS1, select = -DATE) - 1)
    BSG <- as.data.frame(subset(BEST_SALES2, select = -DATE)/subset(BEST_SALES1, select = -DATE) - 1)
    
    DATE <- BEST_EPS1$DATE
    Growth2 <- BEG * 0.11 + BSG * 0.21
    Growth2 <- cbind.data.frame(DATE, Growth2)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    Growth2 <- Growth2[Index,]
    
    DATE <- Period
    Growth <- cbind.data.frame(DATE, subset(Growth1, select = -DATE) + subset(Growth2, select = -DATE))
    Growth <- Utils.Normalize(Growth)
    
    return(Growth)
  }

Factor.Leverage <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    DEBT <- Utils.CleanData("DEBT", Ref.Date, FALSE, 0)
    BOOK_VAL <- Utils.CleanData("BOOK_VAL", Ref.Date, FALSE, 0)
    MKT_CAP <- Utils.CleanData("MKT_CAP", Ref.Date, FALSE, 0)
    TOT_ASSET <- Utils.CleanData("TOT_ASSET", Ref.Date, FALSE, 0)
    
    DATE <- TOT_ASSET$DATE
    
    BLeverage <- as.data.frame(subset(DEBT, select = -DATE) / (subset(BOOK_VAL, select = -DATE) + subset(DEBT, select = -DATE)))
    MLeverage <- as.data.frame(subset(DEBT, select = -DATE) / (subset(MKT_CAP, select = -DATE) + subset(DEBT, select = -DATE)))
    DTRAtio <- as.data.frame(subset(DEBT, select = -DATE) / subset(TOT_ASSET, select = -DATE))
    
    Leverage <- 0.34 * BLeverage + 0.33 * MLeverage + 0.33 * DTRAtio
    Leverage <- cbind.data.frame(DATE, Leverage)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    Leverage <- Leverage[Index,]
    Leverage <- Utils.Normalize(Leverage)
    
    return(Leverage)
  }

#Liquidity captures stocks' trading characteristics in terms of price impact, bid-ask spread and trade frequency.
Factor.Liquidity <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    CHG_PCT <- Utils.CleanData("CHG_PCT", Ref.Date, FALSE, 1)
    TURNOVER <- Utils.CleanData("TURNOVER", Ref.Date, FALSE, 1)
    
    DATE <- CHG_PCT$DATE
    Liquidity <- cbind.data.frame(DATE, lapply(abs(subset(CHG_PCT, select = -DATE)) / sqrt(subset(TURNOVER, select = -DATE)),
                                               (function(x) TTR::EMA(x, n = 53, ratio = log(2)/90))))
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, DATE)
    Trade <- Liquidity[Index,]
    Trade <- Utils.Normalize(Trade)
    
    return(Trade)
  }

#Response Variable: Return
Factor.Return <-
  function(Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    #Find Last Friday
    Ref.Date <- Ref.Date - (as.POSIXlt(Ref.Date)$wday + 2) %% 7
    #Bloomberg Data
    CHG_PCT <- Utils.CleanData("CHG_PCT", Ref.Date, FALSE, 0)
    
    Period <- Factor.Period(Ref.Date)
    Index <- match(Period, CHG_PCT$DATE)
    Return <- CHG_PCT[Index,]
    
    return(Return)
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

#Utils: Names
Factor.Names <-
  function()
  {
    Names <- list()
    #Style Factor
    Names[["Style"]] <-
      c("Momentum","Value","DivYld","Size","Trade","EarnVariab","Profit","Volatility","Growth","Leverage","Liquidity")
    #Industry Factor
    Names[["Industry"]] <- 
      c(
        "Banks",                          "Real Estate",
        "Transportation",                 "Capital Goods",
        "Utilities",                      "Health Care Equipment & Servic",
        "Software & Services",            "Technology Hardware & Equipmen",
        "Materials",                      "Food & Staples Retailing",
        "Consumer Services",              "Consumer Durables & Apparel",
        "Diversified Financials",         "Pharmaceuticals, Biotechnology",
        "Automobiles & Components",       "Food Beverage & Tobacco",
        "Media",                          "Commercial & Professional Serv",
        "Energy",                         "Retailing",
        "Semiconductors & Semiconductor", "Telecommunication Services",
        "Insurance",                      "Household & Personal Products"
      )
    return(Names)
  }