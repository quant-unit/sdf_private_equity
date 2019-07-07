######################
## create preqin basis SDF
######################
# Prologue ---------
# library(readxl)
# library(alfred)
# library(zoo)
if(sys.nframe() == 0L) rm(list = ls())

preqin_basis <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Read Fama French ----
preqin_basis$prep.ff <- function() {
  
  prep.df.ff <- function(df) {
    colnames(df)[1] <- "Date"
    to.eom.date <- function(vec) {
      inner <- function(x) {
        year <- substr(x,1,4)
        month <- substr(x,5,6)
        day <- "01"
        str.date <- paste(year, month, day, sep ="-")
        y <- seq(as.Date(str.date),length=2,by="months")-1
        return(as.character(y[2]))
      }
      dates <- as.Date(sapply(vec, inner))
      return(dates)
    }
    df$Date <- to.eom.date(df$Date)
    
    df[, 2:ncol(df)] <- apply(df[, 2:ncol(df)], 2, function(x){
      as.numeric(x)/100
    })
    
    return(df)
  }
  
  df.ff.5factors <- read.csv("data_in/F-F_Research_Data_5_Factors_2x3.CSV", skip = 3)
  df.ff.5factors <- prep.df.ff(df.ff.5factors[1:670, ])
  
  df.32pofos <- read.csv("data_in/32_Portfolios_ME_OP_INV_2x4x4.CSV", skip = 18)
  df.32pofos <- prep.df.ff(df.32pofos[1:670, ])
  df.32pofos[, 2:ncol(df.32pofos)] <- apply(df.32pofos[, 2:ncol(df.32pofos)], 2, function(x){
    x - df.ff.5factors$Mkt.RF - df.ff.5factors$RF
  })
  
  df.ff <- merge(df.ff.5factors, df.32pofos, by="Date")
  return(df.ff)
}


# Get public data ----------
preqin_basis$get.factor.predictor.data <- function(fred.vec) {
  df.xl <- readxl::read_excel(path = "data_in/Public_Indices_Prepared.xlsx", sheet = "gross_returns")
  df.xl <- data.frame(df.xl)
  df.xl$Date <- as.Date(df.xl$Date)

  # merge with fama french data
  df.ff <- preqin_basis$prep.ff()
  df_pubin <- merge(df.xl, df.ff, by = "Date", all.x = TRUE)
  df_pubin <- df.ff
  
  factor.set <- c("Mkt.RF", "SMB", "HML", "RMW", "CMA")
  factor.set <- colnames(df.ff)[!(colnames(df.ff) %in% c("Date", "SMB", "HML", "RMW", "CMA"))]
  
  df_pubin <- df_pubin[complete.cases(df_pubin), ]
  df_pubin <- df_pubin[df_pubin$Date > as.Date("1979-12-31"), ]
  
  # fred.. data 
  dl.fred <- function(new.var) {
    df.will <- alfred::get_fred_series(series_id = new.var)
    colnames(df.will)[colnames(df.will) == "date"] <- "Date"
    df.will$Date <- as.Date(df.will$Date)
    min.date <- min(df.will$Date)
    max.date <- max(df.will$Date)
    df.will <- merge(df.will,
                     data.frame(Date = seq.Date(from = min.date, to = max.date, by = "day")),
                     by = "Date", all = TRUE)
    df.will[, 2] <- zoo::na.locf(df.will[, 2])
    return(df.will)
  }
  
  for(fred.factor in fred.vec) {
    df_pubin <- merge(df_pubin, dl.fred(fred.factor), by = "Date", all.x = TRUE)
    df_pubin[is.na(df_pubin[, fred.factor]), fred.factor] <- mean(df_pubin[, fred.factor], na.rm = TRUE)
  }
  
  list_pubin = list(df_pubin = df_pubin, factor.set = factor.set)
  write.csv(df_pubin, "data_in/df_pubin.csv")
  saveRDS(list_pubin, "data_in/list_pubin.RDS")
}
preqin_basis$read.list_pubin <- function() {
  return(readRDS("data_in/list_pubin.RDS"))
}


# Create preqin.basis ------
preqin_basis$value.weight.cfs <- function(df) {
  # Fill Fund Size USD by Vintage Median
  df.median.size <- aggregate(list(Median.size.USD = df$Fund.Size..mn.USD.), by = list(Vintage = df$Vintage), function(x) median(x, na.rm = TRUE))
  df.median.size$Median.size.USD[is.na(df.median.size$Median.size.USD)] <- min(df.median.size$Median.size.USD, na.rm = TRUE)
  df <- merge(df, df.median.size, by="Vintage", all.x = TRUE)
  df$Fund.Size..mn.USD. <- ifelse(is.na(df$Fund.Size..mn.USD.), df$Median.size.USD, df$Fund.Size..mn.USD.)
  for(col in c("Transaction.Amount", "Cumulative.Contribution", "Cumulative.Distribution", "Net.Cash.Flow")) {
    df[, col] <- df[, col] * df$Fund.Size..mn.USD. / 10 # in USD
  }
  return(df)
}


preqin_basis$create.preqin.basis <- function(do.boostrap, do.value.weighting, 
                                             numeraire, fred.vec, list_pubin) {
  # Preqin Fund Types Mapping
  Fund.Types <- list(
    VC = c("Balanced", "Early Stage", "Early Stage: Seed", "Early Stage: Start-up", 
           "Expansion / Late Stage", "Venture (General)", "Venture Debt"),
    BO = c("Buyout", "Growth"),
    Debt = c("Direct Lending", "Distressed Debt", "Mezzanine", "Special Situations", "Turnaround"),
    Real = c("Infrastructure", "Natural Resources", "Real Asset", "Real Estate", "Timber")
  )
  
  # load preqin data
  df.p <- readxl::read_excel(path = "data_in/20190520_Preqin_PE_CashFlow_201952000_20190520152325.xlsx", 
                             skip=8)
  df.p <- data.frame(df.p)
  df.p$Transaction.Date <- as.Date(df.p$Transaction.Date)
  df.p$Category.Type <- as.factor(df.p$Category.Type)
  
  if(do.value.weighting) {
    df.p <- preqin_basis$value.weight.cfs(df.p)
  }
  
  # utils function
  lag.it <- function(x) {
    c(0, x[1:(length(x)-1)])
  }
  
  type.list <- list()
  for(fund.type in names(Fund.Types)) {
    print(fund.type)
    
    out.list <- list()
    for(vintage in seq(1986,2015)) {
      print(vintage)
      df.type <- data.frame(df.p)
      types <- Fund.Types[[fund.type]]
      df.type <- df.type[df.type$Category.Type %in% types, ]
      df.type <- df.type[df.type$Fund.Focus == "US", ]
      df.type <- df.type[df.type$Vintage == vintage, ]
      # df.type <- df.type[df.type$Fund.Status == "Liquidated", ]
      no.funds <- length(unique(df.type$Fund.ID))
      print(paste("# funds:", no.funds))
      if(no.funds == 0) next
      
      # bootstrap
      if(do.boostrap) {
        boostrap.list <- list()
        for (id in sample(levels(as.factor(df.type$Fund.ID)), replace = TRUE)) {
          boostrap.list[[id]] <- df.type[df.type$Fund.ID == id, ]
        }
        df.type <- data.frame(do.call(rbind, boostrap.list))
      }
      
      # Transaction Month & Quarter
      df.type$Transaction.Month <- sapply(df.type$Transaction.Date, function(x){
        as.character(seq(from=as.Date(paste(format(x, format="%Y-%m"),"01",sep="-")), by='months', length.out=2)[2] -1)
      })
      df.type$Transaction.Month <- as.Date(df.type$Transaction.Month)
      df.type$Transaction.Quarter <- sapply(df.type$Transaction.Date, function(x){
        year.quarter <- NA
        month.str <- substr(x, 6, 7)
        year.str <- substr(x, 1, 4)
        if(month.str %in% c("01","02","03")){
          year.quarter <- paste(year.str, "03-31",sep="-")
        }
        if(month.str %in% c("04","05","06")){
          year.quarter <- paste(year.str, "06-30",sep="-")
        }
        if(month.str %in% c("07","08","09")){
          year.quarter <- paste(year.str, "09-30",sep="-")
        }
        if(month.str %in% c("10","11","12")){
          year.quarter <- paste(year.str, "12-31",sep="-")
        }
        return(year.quarter)
      })
      df.type$Transaction.Quarter <- as.Date(df.type$Transaction.Quarter)
      
      df.type1 <- data.frame(aggregate(Transaction.Amount ~ Transaction.Quarter, 
                                       data = df.type, subset = Transaction.Category != "Value", FUN=sum))
      colnames(df.type1) <- c("Date", "CashFlow")
      min.date <- min(df.type1$Date)
      max.date <- max(df.type1$Date)
      df.type2 <- data.frame(aggregate(Transaction.Amount ~ Transaction.Quarter, 
                                       data = df.type, subset = Transaction.Category == "Value", FUN=sum))
      df.type2 <- df.type2[df.type2$Transaction.Amount > 0, ]
      colnames(df.type2) <- c("Date", "NAV")
      df.type <- merge(df.type1, df.type2, by = "Date", all = TRUE)
      rm(df.type1, df.type2)
      
      
      df.type <- merge(df.type, list_pubin[["df_pubin"]] , by="Date", all=TRUE)
      df.type$NAV <- zoo::na.locf(df.type$NAV, na.rm=FALSE)
      df.type[is.na(df.type)] <- 0
      df.type <- df.type[df.type$Date >= min.date & df.type$Date <= max.date, ]
      df.type <- df.type[df.type$Date <= as.Date("2017-12-31"), ] # to assure valid final NAVs
      
      # scaled cash flows and NAVs 
      commitment.funds <- no.funds * 1000 * 1000 * 10
      df.type$CashFlow <- df.type$CashFlow / commitment.funds
      df.type$NAV <- df.type$NAV / commitment.funds
      
      # numeraire index
      df.type$Numeraire.Return <- df.type[, numeraire]
      df.type$Numeraire.Index <- cumprod(1+df.type[, numeraire])
      
      # factor set (excess returns)
      factor.set <- list_pubin[["factor.set"]]
      
      # predictor set
      old.colnames <- colnames(df.type)
      df.type$One <- 1
      df.type$NAV.lag1 <- lag.it(df.type$NAV)
      df.type$Age <- as.numeric(df.type$Date - df.type$Date[1]) / 365.25
      for(fred.pred in fred.vec) {df.type[, paste(fred.pred, "lag1", sep=".")] <- lag.it(df.type[, fred.pred])}
      
      predictor.set <- colnames(df.type)[!(colnames(df.type) %in% old.colnames)]
      
      # calculate standard deviation
      fac.pred.sds <- data.frame(expand.grid(Factor = factor.set, Predictor = predictor.set), SD=0)
      for(factor in factor.set) {
        for(predictor in predictor.set) {
          fac.pred.sds$SD[fac.pred.sds$Factor == factor & 
                            fac.pred.sds$Predictor == predictor] <- sd(df.type[, factor] * df.type[, predictor])
        }
      }
      
      out.list[["SD"]][[paste(vintage)]] <- fac.pred.sds
      out.list[["df"]][[paste(vintage)]] <- df.type
      out.list[["factor.set"]] <- factor.set
      out.list[["predictor.set"]] <- predictor.set
    }
    type.list[[fund.type]] <- out.list
  }
  
  return(type.list)
}


# Save preqin.basis ------------ 
preqin_basis$save.preqin.basis <- function(numeraire, fred.vec) {
  preqin.basis <- list()
  do.value.weighting <- TRUE
  preqin.basis[["original"]] <- preqin_basis$create.preqin.basis(do.boostrap = FALSE, 
                                                                 do.value.weighting = do.value.weighting,
                                      numeraire = numeraire,
                                      fred.vec = fred.vec,
                                      list_pubin = preqin_basis$read.list_pubin())
  
  output.folder <- "preqin_basis"
  dir.create(output.folder, showWarnings = FALSE)
  weighting <- ifelse(do.value.weighting, "valueweighted", "equalweighted")
  file.name <- paste(file.path(output.folder, numeraire), "_", weighting, "_preqin_basis.RDS", sep = "")
  saveRDS(preqin.basis, file.name)
}
preqin_basis$save.preqin.basis.boostrap <- function(numeraire, fred.vec, iter) {
  preqin.basis <- list()
  for(i in 1:iter) {
    preqin.basis[[paste("b",i,sep="")]] <-preqin_basis$create.preqin.basis(do.boostrap = TRUE, 
                                                    numeraire = numeraire,
                                                    fred.vec = fred.vec,
                                                    list_pubin = preqin_basis$read.list_pubin())
  }
  
  output.folder <- "preqin_basis"
  dir.create(output.folder, showWarnings = FALSE)
  file.name <- paste(file.path(output.folder, numeraire), "_preqin_basis_boostrap.RDS", sep = "")
  saveRDS(preqin.basis, file.name)
}

# Epilogue ----------

preqin_basis$main <- function(seed) {
  fred.vec = c('TB3MS', 'T10Y3M', 'TEDRATE','USSLIND', 'VXOCLS', 'CFNAIDIFF', 'BAA10Y')
  # preqin_basis$get.factor.predictor.data(fred.vec)
  preqin_basis$save.preqin.basis(numeraire = "RF", fred.vec = fred.vec)
  set.seed(seed)
  #preqin_basis$save.preqin.basis.boostrap(numeraire = "RF", fred.vec = fred.vec, iter = 100)
}

if(sys.nframe() == 0L) {
  system.time(
    preqin_basis$main(2)
  )
}

# Number of VC funds in preqin data set
sum(c(7, 4, 3, 5, 7, 2, 9, 9, 11, 15, 14, 20, 31, 37, 73))