"importDataFrame" <-
  function(rng = NULL, wks = NULL, n.guess = 5)
    ## Create a data.frame from the range rng or from the "Used range" in
    ## the worksheet wks.  The excel data is assumed to be a "database" (sic) 
    ## excel of primitive type (and possibly time/dates).
    ## We guess at the type of each "column" by looking at the first
    ## n.guess entries ... but it is only a very rough guess.
  {
    if(is.null(rng) && is.null(wks))
      stop("need to specify either a range or a worksheet")
    if(is.null(rng))
      rng <- wks$UsedRange()          ## actual region
    else
      wks <- rng[["Worksheet"]]       ## need to query rng for its class
    n.areas <- rng$Areas()$Count()     ## must have only one region
    if(n.areas!=1)
      stop("data must be in a contigious block of cells")
    
    c1 <- rng$Column()                 ## first col
    c2 <- rng$Columns()$Count()        ## last col, provided contiguous region
    r1 <- rng$Row()                    ## first row
    r2 <- rng$Rows()$Count()           ## last row, provided contiguous region
    
    ## headers
    
    n.hdrs <- rng$ListHeaderRows()
    if(n.hdrs==0)
      hdr <- paste("V", seq(form=1, to=c2-c1+1), sep="")
    else if(n.hdrs==1) 
      hdr <- unlist(rng$Rows()$Item(1)$Value2())   ## change: r1 to 1
    else {    ## collapse multi-row headers
      h <- vector("list", c2-c1+1)     ## list by column  ## should have the same bug here
      r <- rng$Range(rng$Cells(r1,c1), rng$Cells(r1+n.hdrs-1, c2))
      jj <- 1
      for(j in seq(from=c1, to=c2)){
        h[[jj]] <- unlist(r$Columns(j)$Value2()[[1]])
        jj <- jj+1
      }
      hdr <- sapply(h, paste, collapse=".")
    }
    r1 <- r1 + n.hdrs
    r2 <- r2 - n.hdrs ## add one line here
    
    ## Data region 
    
    d1 <- wks$Cells(r1, c1)
    d2 <- wks$Cells(r1+r2-1, c1+c2-1) ## change: r2 to r1+r2-1, c2 to c1+c2-1
    dataCols <- wks$Range(d1, d2)$Columns()
    out <- vector("list", length(hdr))
    for(j in seq(along = out)){
      f1 <- dataCols$Item(j)
      f2 <- f1$Value2()[[1]]
      f <- unlist(lapply(f2, function(x) if(is.null(x)) NA else x))
      cls <- guessExcelColType(f1)
      out[[j]] <- if(cls=="logical") as.logical(f) else f
    }
    ## try to convert Excel Date to R Date
    dateIdx <- match("Date",hdr)
    if(!is.na(dateIdx))
      out[[dateIdx]] <- as.Date(out[[dateIdx]], origin = "1899-12-30")
    ##
    names(out) <- make.names(hdr)
    as.data.frame(out, stringsAsFactors = FALSE)
  }

"guessExcelColType" <-
  function(colRng, n.guess = 5, hint = NULL)
    ## colRng points to an range object corresponding to one excel column
    ## e.g., colRng = rng$Columns()$Item("H")
    ## TODO: currently we return one of "logical", "numeric", "character"
    ## need to add "SCOMIDispatch"
  {
    wf <- colRng[["Application"]][["WorksheetFunction"]]
    S.avail <- c("logical", "numeric", "integer", "character")
    ## we should get the following from the Excel type library
    fmt <- colRng[["NumberFormat"]]
    num.fmt <- c("general", "number", "currency", "accounting", 
                 "percentage", "fraction", "scientific")
    
    fld <- colRng$Rows()
    n <- fld$Count()
    k <- min(n.guess, n)
    cls <- character(k)
    for(i in 1:k){
      x <- fld$Item(i)
      if(wf$IsText(x)) 
        cls[i] <- "character"
      else if(wf$IsNumber(x)) {
        if(tolower(fmt) %in% num.fmt)
          cls[i] <- "numeric"
        else 
          cls[i] <- "character"
      }
      else if(wf$IsLogical(x)) 
        cls[i] <- "logical"
      else if(wf$IsNA(x)) 
        cls[i] <- "NA"
      else 
        cls[i] <- "character"
    }
    ## if not all rows agree, use character type
    cls <- cls[cls %in% S.avail]
    if(length(cls)==0 || length(unique(cls))>1)
      return("character")
    else
      return(cls[1])
  }