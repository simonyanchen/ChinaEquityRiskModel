#Industry Function
#Currently base on 24 industry groups of GICS classification
Industry <-
  function(Universe)
  {
    data <- bdp(Universe$Ticker, "GICS_INDUSTRY_GROUP")
  }