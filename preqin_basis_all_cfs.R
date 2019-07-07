################################
## Preqin Basis - All Cash Flows
################################
# Prologue ------------
if(sys.nframe() == 0L) rm(list = ls())
# pme_sdf <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Preqin Basis ----
make.df.all.cfs <- function() {
  #RF_preqin.basis <- readRDS("preqin_basis/RF_preqin_basis_boostrap_seed1.RDS")
  RF_preqin.basis <- list()
  RF_preqin.basis$original <- readRDS("preqin_basis/RF_preqin_basis.RDS")[["original"]]
  
  df_pubin <- readRDS("data_in/list_pubin.RDS")$df[, 1:7]
  df_pubin$alpha <- 1
  
  for(fund.type in names(RF_preqin.basis$original)) {
    all.vintages <- list()
    for(vintage in names(RF_preqin.basis$original[[fund.type]]$df)) {
      df <- RF_preqin.basis$original[[fund.type]]$df[[vintage]]
      df$CashFlowEndNAV <- df$CashFlow
      df$CashFlowEndNAV[nrow(df)] <- df$CashFlowEndNAV[nrow(df)] + df$NAV[nrow(df)]
      df <- merge(df_pubin, df, by="Date", all.x = TRUE)
      df <- df[, c("Date", "CashFlowEndNAV")]
      df[is.na(df)] <- 0
      all.vintages[[vintage]] <- df$CashFlowEndNAV
    }
    df_pubin[, paste("CF", fund.type, sep=".")] <- rowSums(data.frame(do.call(cbind, all.vintages)))
  }
  
  return(df_pubin)
}

df.acfs <- make.df.all.cfs()
df.acfs <- df.acfs[df.acfs$Date >= as.Date("1986-01-01"), ]
df.acfs <- df.acfs[df.acfs$Date <= as.Date("2017-12-31"), ]
df.acfs[,2:7] <- log(1 + df.acfs[,2:7]) # make log-returns

# optimize ----
method <- "nlminb"
fund.type <- "BO"

model <- c("alpha", "Mkt.RF", "HML")
coef <- c(alpha = 0,
          Mkt.RF = 2, 
          SMB = 0,
          HML = 0,
          RMW = 0,
          CMA = 0)

lo.fun <- function(par, cfs, factors) {
  sdf <- 1/exp(cumsum(df.acfs$RF + factors %*% par))
  
  loss <- sum(sapply(sdf, function(x){
    (sum(cfs * sdf) / x)^2
  }))
  return(loss)
}

lo.fun(coef[names(coef) %in% model],
       df.acfs[, paste("CF", fund.type, sep=".")],
       as.matrix(df.acfs[, names(coef[names(coef) %in% model])]))

res <- optimx::optimx(par = coef[names(coef) %in% model], 
                      cfs = df.acfs[, paste("CF", fund.type, sep=".")], 
                      factors = as.matrix(df.acfs[, names(coef[names(coef) %in% model])]),
                      fn = lo.fun, method = method)
res # problem: overdetermined
