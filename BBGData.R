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
    S.Date <- as.Date(paste(Ref.Year,"-01-01",sep = "")) - 1
    E.Date <- as.Date(paste(Ref.Year,"-12-31",sep = ""))
    Options <- structure(c("NON_TRADING_WEEKDAYS","PREVIOUS_VALUE"), 
                         names = c("nonTradingDayFillOption","nonTradingDayFillMethod"))
    TEMP <- bdh(Universe$Ticker, "PX_LAST", start.date = S.Date, end.date = E.Date, options = Options)
    DATE <- TEMP$`000001 CH Equity`$date
    PX_LAST <- cbind.data.frame(DATE,lapply(TEMP, (function(x) x$PX_LAST)))
    return(PX_LAST)
  }

BBGData.Initialize <-
  function(Universe)
  {
    
  }