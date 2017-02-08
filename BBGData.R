#Prepare Bloomberg Data for Factor Calculation given Security Universe
BBGData.Load <-
  function(Data.Name, Ref.Year)
  {
    savename <- paste(Data.Name, Ref.Year, sep = "")
    filename <- paste(savename, ".RData", sep = "")
    filepath <- paste("Data/", filename, sep = "")
    
    if(file.exists(filepath)){
      load(filepath)
    }else{
      #Call Read Function
      eval(parse(text = paste(savename,"=BBGData.Read.", Data.Name, "(Ref.Year)", sep = "")))
      #Save Data
      eval(parse(text = paste("save(",savename,",file='",filepath,"')", sep = "")))
    }
    eval(parse(text = paste("ret=",savename, sep = "")))
    return(ret)
  }

BBGData.Read.PX_LAST <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    DATE <- BBGData.Calendar(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "PX_LAST", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- TEMP[Universe$Ticker]
    PX_LAST <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$PX_LAST)))
    return(PX_LAST)
  }

BBGData.Read.BP_RATIO <-
  function(Ref.Year)
  {
    #Book to Price Ratio
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    PRICE <- BBGData.Read.PX_LAST(Ref.Year)
    PRICE <- PRICE[Universe$Ticker]
    BOOK <- bdh(Universe$Ticker, "BOOK_VAL_PER_SH", start.date = S.Date, end.date = E.Date, options = Options)
    BOOK <- lapply(BOOK, (function(x) x$BOOK_VAL_PER_SH[x$date %in% DATE]))[Universe$Ticker]
    
    BP_RATIO <- as.data.frame(BOOK)/as.data.frame(PRICE)
    names(BP_RATIO) <- Universe$Ticker
    BP_RATIO <- cbind.data.frame(DATE, BP_RATIO)
    
    return(BP_RATIO)
  }

BBGData.Read.EP_RATIO <-
  function(Ref.Year)
  {
    #Earnings to Price Ratio
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    PRICE <- BBGData.Read.PX_LAST(Ref.Year)
    PRICE <- PRICE[Universe$Ticker]
    EARNING <- BBGData.Read.EARNING(Ref.Year)
    EARNING <- EARNING[Universe$Ticker]
    
    EP_RATIO <- EARNING/PRICE
    EP_RATIO <- cbind.data.frame(DATE, EP_RATIO)
    
    return(EP_RATIO)
  }

BBGData.Read.CFP_RATIO <-
  function(Ref.Year)
  {
    #Cash Flow to Price Ratio
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    PRICE <- BBGData.Read.PX_LAST(Ref.Year)
    PRICE <- PRICE[Universe$Ticker]
    CF <- bdh(Universe$Ticker, "TRAIL_12M_CASH_FROM_OPER", start.date = S.Date, end.date = E.Date, options = Options)
    CF <- lapply(CF, (function(x) x$TRAIL_12M_CASH_FROM_OPER[x$date %in% DATE]))[Universe$Ticker]
    
    CFP_RATIO <- as.data.frame(CF)/as.data.frame(PRICE)
    names(CFP_RATIO) <- Universe$Ticker
    CFP_RATIO <- cbind.data.frame(DATE, CFP_RATIO)
    
    return(CFP_RATIO)
  }

BBGData.Read.EBITDA <-
  function(Ref.Year)
  {
    #EBITDA = Operating Income + Depreciation & Amortization (+ Interest Expense)
    OverRides <- structure(Ref.Year, names = "EQY_FUND_YEAR")
    EBITDA <- bdp(Universe$Ticker, "EBITDA", overrides = OverRides)
    OI <- bdp(Universe$Ticker, "IS_OPER_INC", overrides = OverRides)$IS_OPER_INC
    DA <- bdp(Universe$Ticker, "CF_DEPR_AMORT", overrides = OverRides)$CF_DEPR_AMORT
    IE <- bdp(Universe$Ticker, "IS_INT_EXPENSES", overrides = OverRides)$IS_INT_EXPENSES
    Indx1 <- is.na(EBITDA$EBITDA)
    Indx2 <- is.na(IE)
    EBITDA$EBITDA[Indx1&Indx2] <- OI[Indx1&Indx2] + DA[Indx1&Indx2]
    EBITDA$EBITDA[Indx1&!Indx2] <- OI[Indx1&!Indx2] + DA[Indx1&!Indx2] + IE[Indx1&!Indx2]
    return(EBITDA)
  }

BBGData.Read.EV <-
  function(Ref.Year)
  {
    #EV = Market Cap + LT Debt + max(ST Debt - Cash, 0)
    #Cash Flow to Price Ratio
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    fileds <- c("CUR_TOT_ASSET","BS_LT_BORROW","BS_ST_BORROW","BS_CASH_NEAR_CASH_ITEM")
    TEMP <- bdh(Universe$Ticker, fileds, start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x[x$date %in% DATE,]))[Universe$Ticker]
    EV <- cbind.data.frame(DATE,lapply(TEMP, (function(x) 
      x$CUR_TOT_ASSET + x$BS_LT_BORROW + max(x$BS_ST_BORROW - x$BS_CASH_NEAR_CASH_ITEM, 0))))
    
    return(EV)
  }

BBGData.Read.TOT_ASSET <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "BS_TOT_ASSET", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x[x$date %in% DATE,]))[Universe$Ticker]
    TOT_ASSET <- as.data.frame(TEMP)
    names(TOT_ASSET) <- Universe$Ticker
    TOT_ASSET <- cbind.data.frame(DATE, TOT_ASSET)
    
    return(TOT_ASSET)
  }

BBGData.Read.MKT_CAP <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "CUR_MKT_CAP", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x[x$date %in% DATE,]))[Universe$Ticker]
    MKT_CAP <- as.data.frame(TEMP)
    names(MKT_CAP) <- Universe$Ticker
    MKT_CAP <- cbind.data.frame(DATE, MKT_CAP)
    
    return(MKT_CAP)
  }

BBGData.Read.SALES <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "SALES_REV_TURN", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x[x$date %in% DATE,]))[Universe$Ticker]
    SALES <- as.data.frame(TEMP)
    names(SALES) <- Universe$Ticker
    SALES <- cbind.data.frame(DATE, SALES)
    
    return(SALES)
  }

BBGData.Read.EARNING <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "TRAIL_12M_EPS_BEF_XO_ITEM", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x$TRAIL_12M_EPS_BEF_XO_ITEM[x$date %in% DATE]))[Universe$Ticker]
    
    EARNING <- as.data.frame(TEMP)
    names(EARNING) <- Universe$Ticker
    EARNING <- cbind.data.frame(DATE, EARNING)
    
    return(EARNING)
  }

BBGData.Read.BEST_EPS <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.Calendar(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "BEST_EPS", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x[x$date %in% DATE,]))[Universe$Ticker]
    BEST_EPS <- as.data.frame(TEMP)
    names(BEST_EPS) <- Universe$Ticker
    BEST_EPS <- cbind.data.frame(DATE, BEST_EPS)
    
    return(BEST_EPS)
  }

BBGData.Read.DVD_YIELD <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    TEMP <- bdh(Universe$Ticker, "EQY_DVD_YLD_IND_NET", start.date = S.Date, end.date = E.Date, options = Options)
    DATE <- BBGData.Calendar(Ref.Year)
    DVD_YIELD <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$EQY_DVD_YLD_IND_NET)))
    return(DVD_YIELD)
  }

BBGData.Read.SH_OUT <-
  function(Ref.Year)
  {
    #The value is quoted in millions.
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    TEMP <- bdh(Universe$Ticker, "EQY_SH_OUT", start.date = S.Date, end.date = E.Date, options = Options)
    DATE <- BBGData.Calendar(Ref.Year)
    SH_OUT <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$EQY_SH_OUT)))
    return(SH_OUT)
  }

BBGData.Read.PX_VOL <-
  function(Ref.Year)
  {
    #The value is quoted in number of shares.
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    TEMP <- bdh(Universe$Ticker, "PX_VOLUME", start.date = S.Date, end.date = E.Date, options = Options)
    DATE <- BBGData.Calendar(Ref.Year)
    PX_VOL <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$PX_VOLUME)))
    return(PX_VOL)
  }

BBGData.Calendar <-
  function(Ref.Year)
  {
    #Calendar Dates
    filepath <- paste("Data/Calendar", Ref.Year, ".RData", sep = "")
    
    if(file.exists(filepath)){
      load(filepath)
    }else{
      S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
      E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
      Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                           names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
      DATE <- bdh("SHSZ300 Index","PX_LAST", start.date = S.Date, end.date = E.Date, options = Options)$date
      
      save(DATE, file = filepath)
    }
    return(DATE)
  }

BBGData.Initialize <-
  function(Universe)
  {
    
  }