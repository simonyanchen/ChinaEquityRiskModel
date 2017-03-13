#Data Utilities Functions

#Combine data for factor calculation
Utils.CleanData <-
  function(Data.Name, Ref.Date, NA.Fill = FALSE, NumRolling = 0)
  {
    Data <- BBGData.Load(Data.Name, format(Ref.Date,"%Y"))
    
    for(i in (0:NumRolling))
    {
      TEMP <- BBGData.Load(Data.Name, as.character(as.POSIXlt(Ref.Date)$year + 1900 - 1 - i))
      Data <- rbind.data.frame(TEMP, Data)
    }
    ret <- Data
    
    if(NA.Fill){
      #Fill NA using previous value; No action for leading NA temporarily
      #ret <- zoo::na.locf(ret, na.rm = FALSE)
      DATE <- ret$DATE
      ret <- cbind.data.frame(DATE, lapply(subset(ret, select = -DATE),
                                    (function(x) zoo::na.locf(x, na.rm = FALSE))))
    }else{
      #Replace NA with zero
      ret[is.na(ret)] <- 0
    }
    return(ret)
  }