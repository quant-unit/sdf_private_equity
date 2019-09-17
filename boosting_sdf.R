##################################
## Private Market Equivalent - SDF
## Componentwise L2 Boosting
##################################
# Prologue ------------
if(sys.nframe() == 0L) rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Preqin Basis ----
#RF_preqin.basis <- readRDS("preqin_basis/RF_preqin_basis_boostrap_seed1.RDS")
RF_preqin.basis <- list()
RF_preqin.basis$original <- readRDS("preqin_basis/RF_valueweighted_preqin_basis.RDS")[["original"]]

max.vintage <- 2010
for(fund.type in names(RF_preqin.basis$original)) {
  lod <- RF_preqin.basis$original[[fund.type]]$df
  for(vintage in names(lod)) {
    if(as.numeric(vintage) > max.vintage) {
      lod[[vintage]] <- NULL
    }
  }
  RF_preqin.basis$original[[fund.type]]$df <- lod
}



# base procedure ----
fund.type <- "BO"
max.horizons <- 1:15
iterations <- 1000
method <- "nlminb"
step.size <- 0.3



npv.sqr <- function(par, fund.type, max.horizon){
  list.of.dfs <- RF_preqin.basis$original[[fund.type]]$df
  
  discounted.cashflow <- list()
  for(vintage.year in names(list.of.dfs)) {
    df <- list.of.dfs[[vintage.year]]

    linear <- log(1 + as.matrix(df[, names(par)])) %*% par # log-returns
    df$CompoundReturn <- exp(cumsum(log(1 + df$RF) + linear))

    df$CashFlow[nrow(df)] <- df$CashFlow[nrow(df)] + df$NAV[nrow(df)]
    
    iter <- min(max.horizon, round(nrow(df)/12,0)-1)
    weigthed.NPV <- function(x) {
      y <- sum(df$CashFlow * 1000 / df$CompoundReturn) * df$CompoundReturn[max(1, 12*x)]
      if(x > iter) y <- NA
      return(y)
    }
    
    horizon <- 0:max.horizon
    discounted.cashflow[[vintage.year]] <- sapply(horizon, weigthed.NPV)
  }
  
  dcf <- (do.call(rbind, discounted.cashflow))
  out <- log(sum(colMeans(dcf, na.rm=TRUE)^2, na.rm=TRUE)) / (max.horizon+1) # horizon GMM condition

  return(out)
}
One.par <- 0
names(One.par) <- "One"
npv.sqr(One.par, fund.type, tail(max.horizons,1))


alpha.start <- optimx::optimx(par = One.par, fund.type = fund.type, max.horizon = 15, fn = npv.sqr, method = method)[1, 1]
vars <- c("One", "Mkt.RF", "SMB", "HML", "RMW", "CMA")
start.par <- c(alpha.start, rep(0,length(vars)-1))
start.par <- rep(0,length(vars))
names(start.par) <- vars
start.par

wrapper <- function(par, all.par, fund.type, max.horizon) {
  all.par[names(par)] <- all.par[names(par)] + par
  return(npv.sqr(all.par, fund.type, max.horizon))
}
wrapper(One.par, start.par, fund.type, tail(max.horizons,1))

cwl2b <- function(all.par, max.horizon, fund.type, method, step.size) {
  res.list <- list()
  for(i in 1:length(all.par)) {
    res <- optimx::optimx(par = all.par[i], 
                          all.par = all.par, fund.type = fund.type, max.horizon = max.horizon,
                          fn = wrapper, method = method)
    res.list[[paste(i)]] <- data.frame(var = names(all.par[i]), par0 = all.par[i], par1 = res[1, 1], val = res[1, 2])
  }
  df.res <- do.call(rbind, res.list)
  df.min <- df.res[df.res$val == min(df.res$val), ]
  
  all.par[df.min[, "var"]] <- all.par[df.min[, "var"]] + step.size * df.min[, "par1"]
  
  #print(df.res)
  print(all.par)
  return(all.par)
}
cwl2b(start.par, tail(max.horizons,1), fund.type, method, step.size)

# boosting iterations ----

booster <- function(fund.type, max.horizons, iterations, method, step.size) {
  horizon.list <- list()
  for(max.h in max.horizons) {
    para <- start.par
    boost.trace <- list()
    for(a in 1:iterations) {
      boost.trace[[paste(a)]] <- para
      para <- cwl2b(para, max.h, fund.type, method, step.size)
    }
    df.par <- data.frame(do.call(rbind, boost.trace))
    df.par$iter <- rownames(df.par)
    horizon.list[[paste(max.h)]] <- df.par
  }
  
  df.raw <- data.frame(do.call(rbind, horizon.list))
  df.raw$iter <- as.numeric(df.raw$iter)
  df.agg <- aggregate(df.raw[, vars], list(iter = df.raw$iter), mean)
  df.agg$iter <- as.numeric(df.agg$iter)
  df.agg <- df.agg[order(df.agg$iter), ]
  
  df.final.step <- df.raw[df.raw$iter == iterations, ]
  df.final.step$iter <- NULL
  df.final.step <- data.frame(do.call(rbind, list(df.final.step, colMeans(df.final.step), apply(df.final.step, 2, sd))))
  old.col.order <- colnames(df.final.step)
  df.final.step$Horizon <- c(1:15, "Mean", "SD")
  df.final.step <- df.final.step[, c("Horizon", old.col.order)]
  
  final.par <- df.agg[df.agg$iter == iterations, ]
  return(list(raw = df.raw, agg = df.agg, final.step = df.final.step, final.par = final.par,
              fund.type=fund.type, max.horizons=max.horizons, iterations=iterations, method=method, step.size=step.size))
}

system.time(
  boost <- booster(fund.type, max.horizons, iterations, method, step.size)
)

saveRDS(boost, paste("boost/", format(Sys.Date(),"%Y%m%d"), "_", fund.type, "_", tail(max.horizons,1), "_", iterations, ".RDS", sep=""))
#boost2 <- readRDS("boost/20190813_VC_15_50.RDS")

boost$final.step
boost$agg

# analyze result ----
df.raw <- boost$raw
df.agg <- boost$agg
df.final.step <- boost$final.step

df.agg[nrow(df.agg), 2:ncol(df.agg)]
# BO (alpha start)
# One      Mkt.RF SMB        HML          RMW           CMA
# 0.01349772 0.003433567   0 0.01901155 -0.008143353 -0.0004060195
# VC (alpha start)
# One      Mkt.RF        SMB       HML        RMW        CMA
# 0.01512794 -0.07499263 -0.2202831 0.1345378 -0.1358269 -0.1862779
# VC (zero start)
# One       Mkt.RF          SMB          HML          RMW          CMA 
# 0.008310964  0.336374170  0.072455393 -0.192358407  0.401091561  0.177062246 

plot.by.iter <- function(boost) {
  df.agg <- boost$agg

  # plot
  plot(df.agg[, 1], df.agg[, 2], type = "l", 
       xlab = "Iteration", ylab = "Coefficient", main = boost$fund.type, lwd = 2,
       ylim = c(min(df.agg[,2:ncol(df.agg)]), max(df.agg[,2:ncol(df.agg)])))
  for(i in 3:ncol(df.agg)) {
    lines(df.agg[, 1], df.agg[, i], col=i-1, lwd = 2)
  }
  legend("right", bty="n", legend= c(colnames(df.agg[, 2:ncol(df.agg)])), col = 1:(ncol(df.agg)-1), lty = 1, lwd=2)
}
plot.by.iter(boost)


# plot SDF index ----
plot.sdf.return <- function(boost,
                            start.date = "1990-01-01", 
                            tag = "boosting",
                            ymax=30, do.eps = FALSE) {
  fund.type <- boost$fund.type
  df.agg <- boost$agg
  df.par <- df.agg[, 2:ncol(df.agg)] 
  
  df_pubin <- readRDS("data_in/list_pubin.RDS")$df[, 1:7]
  df_pubin <- df_pubin[df_pubin$Date >= as.Date(start.date), ]
  df_pubin$One <- 1
  
  # Plotting Section
  if(do.eps) {
    setEPS()
    postscript(paste("EPS/", fund.type, "_valwei", paste(max.hor, collapse = "_"), tag, ".eps", sep=""), 
               width = 5.5, height = 4, family = "Helvetica", pointsize = 5)
    par(mfrow=c(1,1), cex=1.7)
  }
  
  plot(df_pubin$Date, exp(cumsum(log(1+df_pubin$Mkt.RF+df_pubin$RF))), 
       type = "l", lwd = 2, ylim=c(0, ymax),
       main= fund.type, xlab = "Date", ylab="Cumulative Return")
  
  for(i in 1:nrow(df.par)) {
    par <- as.numeric(df.par[i, ])
    names(par) <- colnames(df.par)
    linear <- log(1 + as.matrix(df_pubin[, names(par)])) %*% par # log-returns
    df_pubin[, paste("CompoundReturn", i, sep = "_")] <- exp(cumsum(log(1 + df_pubin$RF) + linear))
    df_pubin$CompoundReturn <- exp(cumsum(log(1 + df_pubin$RF) + linear))
    
    lines(df_pubin$Date, df_pubin[, paste("CompoundReturn", i, sep = "_")], col="grey", lwd=1)
  }
  lines(df_pubin$Date, df_pubin[, "CompoundReturn"], col="red", lwd=2)
  legend("topleft", bty = "n", legend = c("Market", "SDF"), col = c("black", "red"), lty=1,lwd=2)
  
  
  if(do.eps) {
    par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
  
  invisible(df_pubin)
}
df.pubin <- plot.sdf.return(boost)

# stationarity analysis ------

list.type <- list()
for(fund.type in c("BO", "VC")) {
  list.of.dfs <- RF_preqin.basis$original[[fund.type]]$df
  list.ts <- list()
  for(vin in names(list.of.dfs)) {
    print(vin)
    df <- list.of.dfs[[vin]]
    ncf <- sum(df$CashFlow) + df$NAV[nrow(df)]
    list.ts[[vin]] <- ncf
  }
  df <- data.frame(fund.type = unlist(list.ts))
  colnames(df) <- fund.type
  list.type[[fund.type]] <- df
  
  
  df <- data.frame(do.call(cbind, list.type))
  plot(rownames(df), df[, fund.type], main = fund.type, xlab = "Vintage", ylab = "Net Cash Flow", type = "l")
  points(rownames(df), df[, fund.type])
  abline(h = mean(df[, fund.type]), lty=3)
  abline(h = 0, col = "red")
  
  aTSA::adf.test(df[, fund.type])
  #?aTSA::adf.test
}


