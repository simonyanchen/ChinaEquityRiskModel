Universe.Historical <-
  function(Ref.Date)
  {
    if(!class(Ref.Date) == "Date")
      stop("Ref.Date must be a Date Object", call. = FALSE)
    Ref.Date <- format(Ref.Date,"%Y%m%d")
    filepath <- paste("Universe/", Ref.Date, ".csv", sep = "")
    if(!file.exists(filepath))
      stop("universe as of reference date doesn't exist", call. = FALSE)
    ret <- read.csv(filepath)
    ret <- ret[, -1, drop = FALSE]
    ret$Ticker <- as.character(ret$Ticker)
    return(ret)
  }

Universe.New <-
  function()
  {
    New.Date <- format(Sys.Date(),"%Y%m%d")
    filepath <- paste("Universe/", New.Date, ".csv", sep = "")
    if(file.exists(filepath)){
      ret <- read.csv(filepath)
      ret <- ret[, -1, drop = FALSE]
    }else{
      SZASHR <- bds("SZASHR Index","INDX_MEMBERS")
      #SZASHR$Exchange <- "SZ"
      SHASHR <- bds("SHASHR Index","INDX_MEMBERS")
      #SHASHR$Exchange <- "SH"
      ret <- rbind(SZASHR,SHASHR)
      names(ret)[1] <- "Ticker"
      ret$Ticker <- as.character(paste(ret$Ticker,"Equity"))
      write.csv(ret,file = filepath)
    }
    return(ret)
  }