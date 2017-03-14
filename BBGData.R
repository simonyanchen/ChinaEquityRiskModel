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

#
# Basic Data
#

BBGData.Read.PX_LAST <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "PX_LAST", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- TEMP[Universe$Ticker]
    PX_LAST <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$PX_LAST)))
    return(PX_LAST)
  }

BBGData.Read.CHG_PCT <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "CHG_PCT_1D", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- TEMP[Universe$Ticker]
    CHG_PCT <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$CHG_PCT_1D)))
    return(CHG_PCT)
  }

BBGData.Read.SH_OUT <-
  function(Ref.Year)
  {
    #The value is quoted in millions.
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    TEMP <- bdh(Universe$Ticker, "EQY_SH_OUT", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- TEMP[Universe$Ticker]
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    SH_OUT <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$EQY_SH_OUT)))
    return(SH_OUT)
  }

BBGData.Read.VOL_RATIO <-
  function(Ref.Year)
  {
    #The value is quoted in units.
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    DATE <- BBGData.CDR_DAY(Ref.Year)
    Options <- structure(c("NON_TRADING_WEEKDAYS","NIL_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    fileds <- c("PX_VOLUME","EQY_SH_OUT")
    TEMP <- bdh(Universe$Ticker, fileds, start.date = S.Date, end.date = E.Date, options = Options, int.as.double = TRUE)
    TEMP <- TEMP[Universe$Ticker]
    VOL_RATIO <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$PX_VOLUME/10^6/x$EQY_SH_OUT)))
    return(VOL_RATIO)
  }

#
# Balance Sheet / Income Statement / CF Statement Data
#

BBGData.Read.TOT_ASSET <-
  function(Ref.Year)
  {
    #The value is quoted in millions.
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "BS_TOT_ASSET", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x$BS_TOT_ASSET[x$date %in% DATE]))[Universe$Ticker]
    TOT_ASSET <- as.data.frame(TEMP)
    names(TOT_ASSET) <- Universe$Ticker
    TOT_ASSET <- cbind.data.frame(DATE, TOT_ASSET)
    
    return(TOT_ASSET)
  }

BBGData.Read.BOOK_VAL <-
  function(Ref.Year)
  {
    #Book value 
    #The value is quoted in millions
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "TOT_COMMON_EQY", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x$TOT_COMMON_EQY[x$date %in% DATE]))[Universe$Ticker]
    BOOK_VAL <- as.data.frame(TEMP)
    names(BOOK_VAL) <- Universe$Ticker
    BOOK_VAL <- cbind.data.frame(DATE, BOOK_VAL)
    
    return(BOOK_VAL)
  }

BBGData.Read.INCOME <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "IS_INC_BEF_XO_ITEM", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x$IS_INC_BEF_XO_ITEM[x$date %in% DATE]))[Universe$Ticker]
    
    INCOME <- as.data.frame(TEMP)
    names(INCOME) <- Universe$Ticker
    INCOME <- cbind.data.frame(DATE, INCOME)
    
    return(INCOME)
  }

BBGData.Read.CASH_FLOW <-
  function(Ref.Year)
  {
    #Cash Flow
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    CF <- bdh(Universe$Ticker, "TRAIL_12M_CASH_FROM_OPER", start.date = S.Date, end.date = E.Date, options = Options)
    CF <- lapply(CF, (function(x) x$TRAIL_12M_CASH_FROM_OPER[x$date %in% DATE]))[Universe$Ticker]
    
    CASH_FLOW <- as.data.frame(CF)
    names(CASH_FLOW) <- Universe$Ticker
    CASH_FLOW <- cbind.data.frame(DATE, CASH_FLOW)
    
    return(CASH_FLOW)
  }

BBGData.Read.DEBT <-
  function(Ref.Year)
  {
    #The value is quoted in millions.
    #LT Debt + max(ST Debt - Cash, 0)
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    fileds <- c("BS_LT_BORROW","BS_ST_BORROW","BS_CASH_NEAR_CASH_ITEM")
    TEMP <- bdh(Universe$Ticker, fileds, start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x[x$date %in% DATE,]))[Universe$Ticker]
    DEBT <- cbind.data.frame(DATE,lapply(TEMP, (function(x) 
      x$BS_LT_BORROW + max(x$BS_ST_BORROW - x$BS_CASH_NEAR_CASH_ITEM, 0))))
    
    return(DEBT)
  }

BBGData.Read.MKT_CAP <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "CUR_MKT_CAP", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x$CUR_MKT_CAP[x$date %in% DATE]))[Universe$Ticker]
    MKT_CAP <- as.data.frame(TEMP)
    names(MKT_CAP) <- Universe$Ticker
    MKT_CAP <- cbind.data.frame(DATE, MKT_CAP)
    
    return(MKT_CAP)
  }

BBGData.Read.EV <-
  function(Ref.Year)
  {
    #EV = Market Cap + LT Debt + max(ST Debt - Cash, 0)
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    MKT_CAP <- BBGData.Read.MKT_CAP(Ref.Year)
    MKT_CAP <- MKT_CAP[Universe$Ticker]
    DEBT <- BBGData.Load("DEBT",Ref.Year)
    DEBT <- DEBT[Universe$Ticker]
    
    EV <- as.data.frame(MKT_CAP) + as.data.frame(DEBT)
    names(EV) <- Universe$Ticker
    EV <- cbind.data.frame(DATE, EV)
    
    return(EV)
  }

BBGData.Read.EBITDA <-
  function(Ref.Year)
  {
    #EBITDA = Operating Income + Depreciation & Amortization (+ Interest Expense)
    #OverRides <- structure(Ref.Year, names = "EQY_FUND_YEAR")
    #EBITDA <- bdp(Universe$Ticker, "EBITDA", overrides = OverRides)
    #OI <- bdp(Universe$Ticker, "IS_OPER_INC", overrides = OverRides)$IS_OPER_INC
    #DA <- bdp(Universe$Ticker, "CF_DEPR_AMORT", overrides = OverRides)$CF_DEPR_AMORT
    #IE <- bdp(Universe$Ticker, "IS_INT_EXPENSES", overrides = OverRides)$IS_INT_EXPENSES
    #Indx1 <- is.na(EBITDA$EBITDA)
    #Indx2 <- is.na(IE)
    #EBITDA$EBITDA[Indx1&Indx2] <- OI[Indx1&Indx2] + DA[Indx1&Indx2]
    #EBITDA$EBITDA[Indx1&!Indx2] <- OI[Indx1&!Indx2] + DA[Indx1&!Indx2] + IE[Indx1&!Indx2]
    #return(EBITDA)
    
    #Temp
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "SALES_REV_TURN", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x$SALES_REV_TURN[x$date %in% DATE]))[Universe$Ticker]
    
    SALES <- as.data.frame(TEMP)
    names(SALES) <- Universe$Ticker
    SALES <- cbind.data.frame(DATE, SALES)
    
    #TEMP <- TEMP[Universe$Ticker]
    #SALES <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$SALES_REV_TURN)))
    
    return(SALES)
    
  }

BBGData.Read.SALES <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "SALES_REV_TURN", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x$SALES_REV_TURN[x$date %in% DATE]))[Universe$Ticker]
    
    SALES <- as.data.frame(TEMP)
    names(SALES) <- Universe$Ticker
    SALES <- cbind.data.frame(DATE, SALES)
    
    #TEMP <- TEMP[Universe$Ticker]
    #SALES <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$SALES_REV_TURN)))
    
    return(SALES)
  }

#Per Share
BBGData.Read.EARNING <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "TRAIL_12M_EPS_BEF_XO_ITEM", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x$TRAIL_12M_EPS_BEF_XO_ITEM[x$date %in% DATE]))[Universe$Ticker]
    
    EARNING <- as.data.frame(TEMP)
    names(EARNING) <- Universe$Ticker
    EARNING <- cbind.data.frame(DATE, EARNING)
    
    return(EARNING)
  }

#
# Ratio
#

BBGData.Read.BP_RATIO <-
  function(Ref.Year)
  {
    #Book to Price Ratio
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    PRICE <- BBGData.Load("PX_LAST",Ref.Year)
    PRICE <- PRICE[Universe$Ticker]
    BOOK <- BBGData.Load("BOOK_VAL",Ref.Year)
    BOOK <- BOOK[Universe$Ticker]
    SH_OUT <- BBGData.Load("SH_OUT",Ref.Year)
    SH_OUT <- SH_OUT[Universe$Ticker]
    
    BP_RATIO <- as.data.frame(BOOK)/as.data.frame(SH_OUT)/as.data.frame(PRICE)
    names(BP_RATIO) <- Universe$Ticker
    BP_RATIO <- cbind.data.frame(DATE, BP_RATIO)
    
    return(BP_RATIO)
  }

BBGData.Read.EP_RATIO <-
  function(Ref.Year)
  {
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    PRICE <- BBGData.Load("PX_LAST",Ref.Year)
    PRICE <- PRICE[Universe$Ticker]
    EARNING <- BBGData.Load("EARNING",Ref.Year)
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
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    PRICE <- BBGData.Load("PX_LAST",Ref.Year)
    PRICE <- PRICE[Universe$Ticker]
    CF <- bdh(Universe$Ticker, "TRAIL_12M_CASH_FROM_OPER", start.date = S.Date, end.date = E.Date, options = Options)
    CF <- lapply(CF, (function(x) x$TRAIL_12M_CASH_FROM_OPER[x$date %in% DATE]))[Universe$Ticker]
    SH_OUT <- BBGData.Load("SH_OUT",Ref.Year)
    SH_OUT <- SH_OUT[Universe$Ticker]
    
    CFP_RATIO <- as.data.frame(CF)/as.data.frame(SH_OUT)/as.data.frame(PRICE)
    names(CFP_RATIO) <- Universe$Ticker
    CFP_RATIO <- cbind.data.frame(DATE, CFP_RATIO)
    
    return(CFP_RATIO)
  }

BBGData.Read.SE_RATIO <-
  function(Ref.Year)
  {
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    EV <- BBGData.Load("EV",Ref.Year)
    EV <- EV[Universe$Ticker]
    SALES <- BBGData.Load("SALES",Ref.Year)
    SALES <- SALES[Universe$Ticker]
    
    SE_RATIO <- as.data.frame(SALES)/as.data.frame(EV)
    SE_RATIO <- cbind.data.frame(DATE, SE_RATIO)
    
    return(SE_RATIO)
  }

BBGData.Read.EE_RATIO <-
  function(Ref.Year)
  {
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    EV <- BBGData.Load("EV",Ref.Year)
    EV <- EV[Universe$Ticker]
    EBITDA <- BBGData.Load("EBITDA",Ref.Year)
    EBITDA <- EBITDA[Universe$Ticker]
    
    EE_RATIO <- as.data.frame(EBITDA)/as.data.frame(EV)
    EE_RATIO <- cbind.data.frame(DATE, EE_RATIO)
    
    return(EE_RATIO)
  }

BBGData.Read.EEP_RATIO <-
  function(Ref.Year)
  {
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    PRICE <- BBGData.Load("PX_LAST",Ref.Year)
    PRICE <- PRICE[Universe$Ticker]
    BEST_EPS <- BBGData.Load("BEST_EPS",Ref.Year)
    BEST_EPS <- BEST_EPS[Universe$Ticker]
    
    EEP_RATIO <- as.data.frame(BEST_EPS)/as.data.frame(PRICE)
    EEP_RATIO <- cbind.data.frame(DATE, EEP_RATIO)
    
    return(EEP_RATIO)
  }

BBGData.Read.DVD_YIELD <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    TEMP <- bdh(Universe$Ticker, "EQY_DVD_YLD_IND_NET", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- TEMP[Universe$Ticker]
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    DVD_YIELD <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$EQY_DVD_YLD_IND_NET)))
    return(DVD_YIELD)
  }

#
# Forecast
#

#The BEst (Bloomberg Estimates) Earnings Per Share
BBGData.Read.BEST_EPS <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "BEST_EPS", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- TEMP[Universe$Ticker]
    BEST_EPS <- cbind.data.frame(DATE, lapply(TEMP, (function(x) x$BEST_EPS)))
    return(BEST_EPS)
  }

BBGData.Read.BEST_SALES <-
  function(Ref.Year)
  {
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("CALENDAR","WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("periodicityAdjustment","periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
    #Calendar Dates
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    TEMP <- bdh(Universe$Ticker, "BEST_SALES", start.date = S.Date, end.date = E.Date, options = Options)
    TEMP <- lapply(TEMP, (function(x) x[x$date %in% DATE,]))[Universe$Ticker]
    BEST_SALES <- as.data.frame(TEMP)
    names(BEST_SALES) <- Universe$Ticker
    BEST_SALES <- cbind.data.frame(DATE, BEST_SALES)
    
    return(BEST_SALES)
  }

#
# Special
#
BBGData.Read.BETA <-
  function(Ref.Year)
  {
    #One Year Rolling Regression
    #Beta
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    BETA <- matrix(0,nrow = length(DATE), ncol = length(Universe$Ticker))
    
    for (i in 1:length(DATE))
    {
      S.DATE <- seq(DATE[i], by = "-1 years", length = 2)[2]
      E.Date <- DATE[i]
      Overrides <- structure(c("SH000906 Index",format(S.DATE,"%Y%m%d"),format(E.Date,"%Y%m%d")),
                             names = c("BETA_OVERRIDE_REL_INDEX","BETA_OVERRIDE_START_DT","BETA_OVERRIDE_END_DT"))
      TEMP <- bdp(Universe$Ticker, "BETA_RAW_OVERRIDABLE", overrides = Overrides)$BETA_RAW_OVERRIDABLE
      BETA[i,] <- TEMP
    }
    BETA <- as.data.frame(BETA)
    names(BETA) <- Universe$Ticker
    BETA <- cbind.data.frame(DATE, BETA)
    
    return(BETA)
  }

BBGData.Read.SIGMA <-
  function(Ref.Year)
  {
    #One Year Rolling Regression
    #Sigma
    DATE <- BBGData.CDR_WEEK(Ref.Year)
    SIGMA <- matrix(0,nrow = length(DATE), ncol = length(Universe$Ticker))
    
    for (i in 1:length(DATE))
    {
      S.DATE <- seq(DATE[i], by = "-1 years", length = 2)[2]
      E.Date <- DATE[i]
      Overrides <- structure(c("SH000906 Index",format(S.DATE,"%Y%m%d"),format(E.Date,"%Y%m%d")),
                             names = c("BETA_OVERRIDE_REL_INDEX","BETA_OVERRIDE_START_DT","BETA_OVERRIDE_END_DT"))
      TEMP <- bdp(Universe$Ticker, "STD_DEV_ERR_OVERRIDABLE", overrides = Overrides)$STD_DEV_ERR_OVERRIDABLE
      SIGMA[i,] <- TEMP
    }
    SIGMA <- as.data.frame(SIGMA)
    names(SIGMA) <- Universe$Ticker
    SIGMA <- cbind.data.frame(DATE, SIGMA)
    
    return(SIGMA)
  }

#
# Utilities
#

BBGData.CDR_DAY <-
  function(Ref.Year)
  {
    #Calendar Dates Daily
    filepath <- paste("Data/CDR_DAY", Ref.Year, ".RData", sep = "")
    
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

BBGData.CDR_WEEK <-
  function(Ref.Year)
  {
    #Calendar Dates Weekly
    filepath <- paste("Data/CDR_WEEK", Ref.Year, ".RData", sep = "")
    
    if(file.exists(filepath)){
      load(filepath)
    }else{
      S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = ""))
      E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
      Options <- structure(c("WEEKLY","NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                           names = c("periodicitySelection","nonTradingDayFillOption","nonTradingDayFillMethod"))
      DATE <- bdh("SHSZ300 Index","PX_LAST", start.date = S.Date, end.date = E.Date, options = Options)$date
      
      save(DATE, file = filepath)
    }
    return(DATE)
  }

BBGData.Initialize <-
  function(Universe)
  {
    
  }