#Industry Function
#Currently base on 24 industry groups of GICS classification
Factor.Industry <-
  function(Universe, Standard = "GICS")
  {
    Ticker <- Universe$Ticker
    if(Standard == "GICS"){
      Industry <- bdp(Ticker, "GICS_INDUSTRY_GROUP_NAME")$GICS_INDUSTRY_GROUP
    }
    ret <- cbind.data.frame(Ticker,Industry)
    return(ret)
  }