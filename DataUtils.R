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

#Simply normalization
Utils.Normalize <-
  function(raw)
  {
    tmp <- subset(raw, select = -DATE)
    DATE <- raw$DATE
    colnames <- names(tmp)
    avg <- rowMeans(tmp, na.rm = TRUE)
    std <- apply(tmp, 1, function(x) sd(x, na.rm = TRUE))
    #standardization
    ret <- as.data.frame(lapply(tmp, function(x) (x - avg)/std))
    names(ret) <- colnames
    #Capped with +/-3
    ret[ret>3] <- 3
    ret[ret<-3] <- -3
    
    ret <- cbind.data.frame(DATE, ret)
    return(ret)
  }

#Prepare input data for regression
Utils.InputData <-
  function(Ref.Date)
  {
    Input.Data <- list()
    DATE <- Factor.Period(Ref.Date)
    Names <- Factor.Names()
    IndustryInfo <- Factor.Industry(Universe,"GICS")
    #Industry Factor
    IndustryFactor <- matrix(0,
                             nrow = length(Universe$Ticker),
                             ncol = length(Names$Industry),
                             dimnames = list(Universe$Ticker,Names$Industry))
    Index <- match(IndustryInfo$Industry,Names$Industry)
    for(i in (1:length(Universe$Ticker))){
      IndustryFactor[i,Index[i]] <- 1
    }
    IndustryFactor <- as.data.frame(IndustryFactor)
    #Style Factor
    Momentum <- Factor.Momentum(Ref.Date)
    Value <- Factor.Value(Ref.Date)
    DivYld <- Factor.DivYld(Ref.Date)
    Size <- Factor.Size(Ref.Date)
    Trade <- Factor.Trade(Ref.Date)
    EarnVariab <- Factor.EarnVariab(Ref.Date)
    Profit <- Factor.Profit(Ref.Date)
    Volatility <- Factor.Volatility(Ref.Date)
    Growth <- Factor.Growth(Ref.Date)
    Leverage <- Factor.Leverage(Ref.Date)
    Liquidity <- Factor.Liquidity(Ref.Date)
    #Response Variable
    Ret <- Factor.Return(Ref.Date)
    #Cosmetics
    for(j in (1:length(DATE))){
      StyleFactor <- matrix(0,
                       nrow = length(Universe$Ticker),
                       ncol = length(Names$Style),
                       dimnames = list(Universe$Ticker,Names$Style))
      StyleFactor[,match("Momentum",Names$Style)] <- unlist(subset(Momentum, select = - DATE)[j,])
      StyleFactor[,match("Value",Names$Style)] <- unlist(subset(Value, select = - DATE)[j,])
      StyleFactor[,match("DivYld",Names$Style)] <- unlist(subset(DivYld, select = - DATE)[j,])
      StyleFactor[,match("Size",Names$Style)] <- unlist(subset(Size, select = - DATE)[j,])
      StyleFactor[,match("Trade",Names$Style)] <- unlist(subset(Trade, select = - DATE)[j,])
      StyleFactor[,match("EarnVariab",Names$Style)] <- unlist(subset(EarnVariab, select = - DATE)[j,])
      StyleFactor[,match("Profit",Names$Style)] <- unlist(subset(Profit, select = - DATE)[j,])
      StyleFactor[,match("Volatility",Names$Style)] <- unlist(subset(Volatility, select = - DATE)[j,])
      StyleFactor[,match("Growth",Names$Style)] <- unlist(subset(Growth, select = - DATE)[j,])
      StyleFactor[,match("Leverage",Names$Style)] <- unlist(subset(Leverage, select = - DATE)[j,])
      StyleFactor[,match("Liquidity",Names$Style)] <- unlist(subset(Liquidity, select = - DATE)[j,])
      
      StyleFactor <- as.data.frame(StyleFactor)
      Return <- unlist(subset(Ret, select = - DATE)[j,])
      Input.Data[[format(DATE[j])]] <- cbind.data.frame(Return,StyleFactor,IndustryFactor)
    }
    
    return(Input.Data)
  }