#Model
Model.Regression <-
  function(Input.Data)
  {
    FactorRet <- matrix(0, nrow = length(Input.Data), ncol = length(names(Input.Data[[1]]))-1,
                     dimnames = list(names(Input.Data),c(names(Input.Data[[1]])[-1])))
    RSquared <- matrix(0, nrow = length(Input.Data), ncol = 1,
                       dimnames = list(names(Input.Data),"RSquared"))
    for(i in c(1:length(Input.Data))){
      Test <- Input.Data[[i]]
      Test[is.na(Test)] <- 0
      #Temporary replace NA with zero
      #TBD
      FitResult <- lm(Return~.-1,data = Test)
      FitResult.Summary <- summary(FitResult)
      
      FactorRet[i,] <- FitResult$coefficients
      RSquared[i,] <- FitResult.Summary$r.squared
    }
    FactorRet <- as.data.frame(FactorRet)
    RSquared <- as.data.frame(RSquared)
    
    Result <- list()
    Result[["FactorRet"]] <- FactorRet
    Result[["FactorRetCum"]] <- cumprod(FactorRet / 100 + 1)
    Result[["RSquared"]] <- RSquared
    return(Result)
  }