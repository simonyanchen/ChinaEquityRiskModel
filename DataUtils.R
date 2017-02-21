#Data Utilities Functions

#Combine data for factor calculation
Utils.CleanData <-
  function(Data.Name, Ref.Date, NA.Fill = FALSE, Rolling = FALSE)
  {
    if(Rolling){
      Data1 <- BBGData.Load(Data.Name, as.character(as.POSIXlt(Ref.Date)$year + 1900 - 2))
      Data2 <- BBGData.Load(Data.Name, as.character(as.POSIXlt(Ref.Date)$year + 1900 - 1))
      Data3 <- BBGData.Load(Data.Name, format(Ref.Date,"%Y"))
      
      ret <- rbind.data.frame(Data1, Data2, Data3)
    }else{
      Data1 <- BBGData.Load(Data.Name, as.character(as.POSIXlt(Ref.Date)$year + 1900 - 1))
      Data2 <- BBGData.Load(Data.Name, format(Ref.Date,"%Y"))
      
      ret <- rbind.data.frame(Data1, Data2)
    }
    
    if(NA.Fill){
      #Fill NA using previous value; No action for leading NA temporarily
      ret <- zoo::na.locf(ret, na.rm = FALSE)
      
    }else{
      #Replace NA with zero
      ret[is.na(ret)] <- 0
    }
    
    return(ret)
  }