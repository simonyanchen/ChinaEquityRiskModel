#TBD
#Cumulative return over one year (averaged),
#skipping the most recent two weeks to mitigate the price reversal effect
Factor.Momentum <-
  function(Universe, Ref.Date = NULL)
  {
    if(is.null(Ref.Date))
      Ref.Date <- Sys.Date()
    Sta.Date <- seq(Ref.Date, length = 2, by = "-12 months")[2]
    weeklyOpt <- structure(c("ACTUAL", "WEEKLY"),
                           names = c("periodicityAdjustment", "periodicitySelection"))
    Price <- bdh(securities = Universe$Ticker,fields = "PX_LAST",start.date = Sta.Date,end.date = Ref.Date,options = weeklyOpt)
  }