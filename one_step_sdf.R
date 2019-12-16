##################################
## Private Market Equivalent - SDF
## One Step optimization
##################################
# Prologue ------------
if(sys.nframe() == 0L) rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Preqin Basis ----
weighting <- c("EW")
do.vin.weight <- FALSE # seems not to work (stationarity) ?!

#RF_preqin.basis <- readRDS("preqin_basis/RF_preqin_basis_boostrap_seed1.RDS")
RF_preqin.basis <- list()
RF_preqin.basis$VW <- readRDS("preqin_basis/RF_valueweighted_preqin_basis.RDS")[["original"]]
RF_preqin.basis$EW <- readRDS("preqin_basis/RF_equalweighted_preqin_basis.RDS")[["original"]]

RF_preqin.basis$VW$BO_Buyout$mean.fund.sizes / mean(RF_preqin.basis$VW$BO_Buyout$mean.fund.sizes)
RF_preqin.basis$VW$BO_Growth$mean.fund.sizes / mean(RF_preqin.basis$VW$BO_Growth$mean.fund.sizes)

RF_preqin.basis$VW$VC_Late$mean.fund.sizes['1995'] / mean(RF_preqin.basis$VW$VC_Late$mean.fund.sizes)


for(w in names(RF_preqin.basis)) {
  for(fund.type0 in names(RF_preqin.basis[[w]])) {
    RF_preqin.basis[["original"]][[paste(fund.type0, w, sep="_")]] <- RF_preqin.basis[[w]][[fund.type0]]
  }
}
names(RF_preqin.basis$original)

trim.preqin.basis <- function(RF_preqin.basis, max.vintage) {
  for(fund.type0 in names(RF_preqin.basis$original)) {
    lod <- RF_preqin.basis$original[[fund.type0]]$df
    no.funds.total <- 0
    for(vintage in names(lod)) {
      if(as.numeric(vintage) > max.vintage) {
        lod[[vintage]] <- NULL
        vec <- RF_preqin.basis$original[[fund.type0]]$mean.fund.sizes
        RF_preqin.basis$original[[fund.type0]]$mean.fund.sizes <- vec[!(names(vec) %in% vintage)] 
      } else {
        no.funds.total <- no.funds.total + RF_preqin.basis$original[[fund.type0]]$no.funds[[vintage]]
      }
    }
    RF_preqin.basis$original[[fund.type0]]$df <- lod
    #print(paste(fund.type0, no.funds.total))
    RF_preqin.basis$original[[fund.type0]]$no.funds.total <- no.funds.total
  }
  return(RF_preqin.basis)
}
preqin.basis.test <- trim.preqin.basis(RF_preqin.basis, 2005)
df <- preqin.basis.test$original$BO_VW$df$`1986`


ftl.in <- list(
  BO = c("BO_Buyout", "BO_Growth"),
  VC = c("VC_Balanced", "VC_Early", "VC_Late"),
  Debt =  c("Debt_DD", "Debt_MEZZ", "Debt_Other"),
  Real = c("Real_INF", "Real_NR_TIM", "Real_Estate"),
  PE = c("BO", "VC", "Debt", "Real", "FOFSEC")
)

ftl <- list()
for(ft in names(ftl.in)) {
  for(weight in weighting) {
    ftl[[ft]] <- c(ftl[[ft]], paste(ftl.in[[ft]], weight, sep = "_"))
  }
}
ftl

# no of funds & stationarity analysis ------

make.no.of.funds.table <- function() {
  no.of.funds <- list()
  for(asset.class in names(ftl)) {
    for(fund.type in ftl[[asset.class]]) {
      df <- data.frame(RF_preqin.basis$original[[fund.type]]$no.funds)
      colnames(df) <- fund.type
      df$Vintage <- rownames(df)
      no.of.funds[[fund.type]] <- df
    }
  }
  
  df.no.of.funds <- Reduce(function(x, y) merge(x, y, all=T,  by="Vintage"), no.of.funds, accumulate=F)
  rownames(df.no.of.funds) <- df.no.of.funds$Vintage
  df.no.of.funds$Vintage <- NULL
  df.no.of.funds[is.na(df.no.of.funds)] <- 0
  df.no.of.funds <- df.no.of.funds[, 1:5]
  print(xtable::xtable(df.no.of.funds, caption = "", label = "", digits=0))
  
  
}
make.no.of.funds.table()

plot.cum.net.cash.flow <- function(trim.year = 2010, do.eps = FALSE) {
  preqin.basis.test <- trim.preqin.basis(RF_preqin.basis, trim.year)
  
  list.type <- list()
  for(asset.class in names(ftl)) {
    for(fund.type in ftl[[asset.class]]) {
      list.of.dfs <- preqin.basis.test$original[[fund.type]]$df
      list.ts <- list()
      for(vin in names(list.of.dfs)) {
        df <- list.of.dfs[[vin]]
        vin.weight <- RF_preqin.basis$original[[fund.type]]$mean.fund.sizes[[vin]] / mean(RF_preqin.basis$original[[fund.type]]$mean.fund.sizes)
        ncf <- sum(df$CashFlow) + df$NAV[nrow(df)]
        if(do.vin.weight) ncf <- ncf * vin.weight
        list.ts[[vin]] <- ncf
      }
      df <- data.frame(fund.type = unlist(list.ts))
      colnames(df) <- fund.type
      df$Vintage <- rownames(df)
      list.type[[fund.type]] <- df
      
      #aTSA::adf.test(df[, fund.type])
      #?aTSA::adf.test
    }
  }
  df <- Reduce(function(x, y) merge(x, y, all=T,  by="Vintage"), list.type, accumulate=F)
  
  
  # PLOT
  
  if(do.eps) {
    par.old <- par()
    dir.create("EPS1", showWarnings = FALSE)
    setEPS()
    postscript(paste("EPS1/", format(Sys.Date(), "%Y%m%d"), "_NCFs_", paste(weighting,collapse="_"),".eps", sep=""), 
               width = 6, height = 3,
               family = "Helvetica", pointsize = 10)
    par(mfrow=c(1,1), cex=1.1, mar = c(4.1, 4.1, 1.1, 1.1))
  }
  
  plot(df$Vintage, df[, 2], main = NULL, xlab = "Vintage", 
       ylab = "Cumulative Net Cash Flow", type = "b", pch = 20, col = 2, ylim = c(-1, 9))
  abline(h = 0, col = "grey")
  
  j <- 0
  pch0 <- 20
  for(i in 3:6) {
    j <- j + 1
    if(j == 8) {
      j=0
      pch0 <- pch0 + 1
    }
    points(df$Vintage, df[, i], type="b", pch = pch0, col = i)
  }
  
  legend("topright", bty="n", legend = colnames(df[, 2:6]), col=2:6, lty=1, pch=20)
  
  if(do.eps) {
    par(par.old)
    #par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
  
}
plot.cum.net.cash.flow()


# GMM loss function ------
expontential.affine <- TRUE
driessen <- FALSE # similar to GMM when sub-fund-type conditions are used
fund.types <- ftl[["PE"]]
by <- 1
max.horizons <- seq(12, 20 * 12, by)
#max.horizons <- seq(10 * 12, 15 * 12, by)
method <- "nlminb"


horizon.NPV <- function(x, nrow.df1, npv_cf1, CompoundReturn1) {
  if(x > nrow.df1) return(NA) # NA (not 0) so that average is calc correclty
  return(npv_cf1 * CompoundReturn1[x])
}

make.weight <- function(dim) {
  x <- matrix(1, dim, dim)
  for(i in 1:dim) {
    for(j in 1:dim) {
      x[i,j] <- 1 / (1+ abs(i-j))
    }
  }
  return(x)
}

huber.loss <- function(vec, alpha) {
  ifelse(vec > alpha, abs(vec), vec^2)
} 

npv.sqr <- function(par, fund.types, preqin.basis, max.horizon){
  horizons <- seq(1, max.horizon + 1, by)
  
  weight <- c()
  for(fund.type in fund.types) {
    x <- preqin.basis$original[[fund.type]]$no.funds.total
    names(x) <- fund.type
    weight <- c(weight, x)
  }
  weight <- weight / sum(weight)

  fund.type.npvs <- list()
  for(fund.type in fund.types) {
    list.of.dfs <- preqin.basis$original[[fund.type]]$df
    
    horizon_npvs <- list()
    for(vintage.year in names(list.of.dfs)) {
      df <- list.of.dfs[[vintage.year]]
      nrow.df <- nrow(df)
      
      linear <- as.matrix(df[, names(par)]) %*% par
      
      if(expontential.affine) {
        CompoundReturn <- exp(cumsum(df$RF + linear))
      } else {
        CompoundReturn <- exp(cumsum(log(1 + df$RF+ linear)))
      }

      df$CashFlow[nrow.df] <- df$CashFlow[nrow.df] + df$NAV[nrow.df] # use final NAV as cash flow
      
      npv_cf <- sum(df$CashFlow * 1000 / CompoundReturn) # net present value of cash flow at time 0
      
      if(do.vin.weight) npv_cf <- npv_cf * preqin.basis$original[[fund.type]]$mean.fund.sizes[vintage.year] / mean(preqin.basis$original[[fund.type]]$mean.fund.sizes)

      horizon_npvs[[vintage.year]] <- sapply(horizons, horizon.NPV, nrow.df1 = nrow.df, npv_cf1 = npv_cf, CompoundReturn1 = CompoundReturn)
    }
    
    dcf <- do.call(rbind, horizon_npvs) # rows: vintage years, cols: horizons (0, 1, 2, ...)
    if(driessen) {
      dcf[is.na(dcf)] <- 0
      fund.type.npvs[[fund.type]] <- sum(dcf^2)
    }
    
    e.vector <- colMeans(dcf, na.rm=TRUE)
    e.vector <- e.vector[!(is.na(e.vector))]
    fund.type.npvs[[fund.type]] <- mean(e.vector^2) * weight[fund.type]
  }
  
  if(driessen) {
    return(mean(unlist(fund.type.npvs)))
  } else {
    return(log(sum(unlist(fund.type.npvs))))
    
    e <- unlist(fund.type.npvs) # fund.type & horizon conditions
    len.e <- length(e)
    w <- diag(len.e) # L_2 norm
    #w <- matrix(1, len.e, len.e) # all-ones weighting matrix
    #w <- make.weight(len.e) # does not yield good results
    
    out <- (as.numeric(log(t(e) %*% w %*% e) / len.e))
    return(out)
  }
}
start.par <- c("One" = 0, "Mkt.RF" = 0)
#start.par <- c("Mkt.RF" = 0)
npv.sqr(start.par, fund.types, trim.preqin.basis(RF_preqin.basis, 2005), tail(max.horizons,1))


suppressWarnings(
  optimx::optimx(par = c("Mkt.RF" = 0), fund.types = fund.types, 
                 preqin.basis = trim.preqin.basis(RF_preqin.basis, 2005),
                 max.horizon = tail(max.horizons,1), 
                 fn = npv.sqr, method = method)
)


# EL loss function ----
if(FALSE) {
  log_star <- function(x, e = 0.01) {
    if(x >= e) {
      return(log(x))
    } else {
      y <- log(e) - 1.5 + 2 * x / e - x^2 / (2*e^2)
      return(y)
    }
  }
  
  par <- start.par <- c("One" = 0.02, "Mkt.RF" = 1)
  fund.type <- "BO_Buyout_EW" # just single sub-fund-type condition
  preqin.basis <- trim.preqin.basis(RF_preqin.basis, 2005)
  max.horizon <- tail(max.horizons, 1)
  
  #emp.loli <- function(par, fund.type, preqin.basis, max.horizon){
  horizons <- seq(1, max.horizon + 1, by)
  horizons <- pmax(1, seq(0, max.horizon, 36))
  
  weight <- c()
  for(fund.type in fund.types) {
    x <- preqin.basis$original[[fund.type]]$no.funds.total
    names(x) <- fund.type
    weight <- c(weight, x)
  }
  weight <- weight / sum(weight)
  
  
  list.of.dfs <- preqin.basis$original[[fund.type]]$df
  
  horizon_npvs <- list()
  for(vintage.year in names(list.of.dfs)) {
    df <- list.of.dfs[[vintage.year]]
    nrow.df <- nrow(df)
    
    linear <- as.matrix(df[, names(par)]) %*% par
    CompoundReturn <- exp(cumsum(log(1 + df$RF+ linear)))
    
    df$CashFlow[nrow.df] <- df$CashFlow[nrow.df] + df$NAV[nrow.df] # use final NAV as cash flow
    
    npv_cf <- sum(df$CashFlow * 1000 / CompoundReturn) # net present value of cash flow at time 0
    
    if(do.vin.weight) npv_cf <- npv_cf * preqin.basis$original[[fund.type]]$mean.fund.sizes[vintage.year] / mean(preqin.basis$original[[fund.type]]$mean.fund.sizes)
    
    horizon_npvs[[vintage.year]] <- sapply(horizons, horizon.NPV, nrow.df1 = nrow.df, npv_cf1 = npv_cf, CompoundReturn1 = CompoundReturn)
  }
  
  dcf <- do.call(rbind, horizon_npvs) # rows: vintage years, cols: horizons (0, 1, 2, ...)
  dcf[is.na(dcf)] <- 0
  
  foo2min <- function(gamma) {
    y <- c()
    n <- nrow(dcf)
    for(i in 1:n) {
      y <- c(y, log_star(1 + dcf[i, ] %*% gamma))
    }
    out <- - sum(y) # - n * log(n)
    return(out)
  }
  
  p0 <- rep(1, ncol(dcf))
  p0 <- runif(ncol(dcf))
  foo2min(p0)
  df.lagrangian <- optimx::optimx(p0, foo2min, 
                                  #method = c("nlm", "nlminb")
                                  control = list(all.methods=TRUE)
  )
  df.lagrangian
  colMeans(dcf)
  #}
  
  #emp.loli(start.par, fund.type, trim.preqin.basis(RF_preqin.basis, 2005), tail(max.horizons,1))
  
}


# one step optim -----------

start.pars <- list(
  Base = c("One" = 0, "Mkt.RF" = 0),
  HML = c("One" = 0, "Mkt.RF" = 0, "HML" = 0),
  SMB = c("One" = 0, "Mkt.RF" = 0, "SMB" = 0),
  RMW = c("One" = 0, "Mkt.RF" = 0, "RMW" = 0),
  CMA = c("One" = 0, "Mkt.RF" = 0, "CMA" = 0)
  )
#start.pars <- list(Base = c("One" = 0, "Mkt.RF" = 0))

one.step.iter <- function(fund.type.list, max.vintages) {
  out.list <- list()
  for(f.type in names(fund.type.list)) {
    print(f.type)
    fund.types <- fund.type.list[[f.type]]
    
    vintage.list <- list()
    for (max.vintage in max.vintages) {
      print(max.vintage)
      
      one.step.wrap <- function(max.h, max.v, start.p) {
        tryCatch({
          res <- optimx::optimx(max.horizon = max.h, par = start.p, 
                                fund.types = fund.types, 
                                preqin.basis = trim.preqin.basis(RF_preqin.basis, max.v),
                                fn = npv.sqr, method = method)
          
          res$Horizon <- max.h
          return(res)
        }, error = function(e) {
          print(paste("Error", max.h))
          return(NULL)})
      }
      
      for(start.par.name in names(start.pars)) {
        start.par <- start.pars[[start.par.name]]
        df.out <- data.frame(do.call(rbind, lapply(max.horizons, one.step.wrap, 
                                                   max.v = max.vintage, 
                                                   start.p = start.par)))
        if(nrow(df.out) > 0) {
          df.out$Fund.Type <- f.type
          df.out$Max.Vin <- max.vintage
          df.out$Model <- start.par.name
            vintage.list[[paste0(max.vintage, "_", start.par.name)]] <- df.out
        }
      }
    }
    out.list[[f.type]] <- vintage.list
  }
  return(out.list)
}


if(expontential.affine) {
  file.path <- paste0("resi/resi_exp_ff_", paste(weighting, collapse = "_"), ".RDS")
} else {
  file.path <- paste0("resi/resi_lin_ff_", paste(weighting, collapse = "_"), ".RDS")
}

if(TRUE) {
  system.time(
    resi <- one.step.iter(ftl, 2000:2010)
  )
  saveRDS(resi, file.path)
} else {
  resi <- readRDS(file.path)
}


df.resi <- do.call(dplyr::bind_rows, unlist(resi, recursive = FALSE))
ff <- c("One", "Mkt.RF", "SMB", "HML", "RMW", "CMA")
df.resi[, ff][is.na(df.resi[, ff])] <- 0

if(expontential.affine) {
  write.csv(df.resi, paste0("resi/df_resi_exp_", paste(weighting, collapse = "_"),".csv"))
} else {
  write.csv(df.resi, paste0("resi/df_resi_lin_", paste(weighting, collapse = "_"),".csv"))
}


# squared pricing error analysis -----

df.spea <- df.resi[(df.resi$Horizon >= 120) & (df.resi$Horizon <= 180) & (df.resi$kkt1) & (df.resi$kkt2), ]
(aggregate(value ~ Fund.Type + Max.Vin, df.spea, mean))


# analyze one step optim -----
coef.by.horizon <- function(df) {
  
  if(!("One" %in% colnames(df))) {
    df$One <- 0
  }
  
  suppressWarnings( df$Horizon <- as.numeric(df$Horizon) )
  df$Horizon <- round(df$Horizon ,2)
  df <- df[complete.cases(df$Horizon), ]
  df <- df[df$Horizon > 1, ]
  df <- df[complete.cases(df$kkt1), ]
  
  plot(df$Horizon, df$Mkt.RF, type="b", ylim=c(-1.5,3), col = "blue", 
       main = paste(df$Fund.Type[1], " (Max. Vintage: ", df$Max.Vin[1], ")", sep=""), 
       ylab="Coefficient", xlab = "Maximum Horizon (in Months)")
  lines(df$Horizon, exp(12*df$One)-1, type = "b", col="red") # annualized
  abline(h=0, col="grey", lty=2)
  abline(v = c(10, 15) * 12, lty=3, col="grey")
  legend("bottomright", bty="n", legend=c("Alpha", "Beta"), col = c("red", "blue"), pch=1)
  
  # Average Coef
  df <- df[df$Horizon <= 12*15, ]
  df <- df[df$Horizon >= 12*10, ]
  
  alpha <- exp(12*df$One)-1
  beta <- df$Mkt.RF
  
  round.to <- 4
  estimates <- c(
    Type = df$Fund.Type[1],
    Vintage = df$Max.Vin[1],
    avg.alpha = round(mean(alpha), round.to),
    sd.alpha = round(sd(alpha), round.to),
    avg.beta = round(mean(beta), round.to),
    sd.beta = round(sd(beta), round.to)
  )
  return(estimates)
}

coef.by.horizon(resi$BO$`2000_Base`)

make.coef.plots <- function(resi, do.eps = FALSE) {
  
  estimates <- list()
  for(f.type in names(resi)) {
    
    if(do.eps) {
      par.old <- par()
      dir.create("EPS1", showWarnings = FALSE)
      setEPS()
      postscript(paste("EPS1/", format(Sys.Date(), "%Y%m%d"), f.type, "_", paste(weighting,collapse="_"),".eps", sep=""), 
                 width = 6, height = 4,
                 family = "Helvetica", pointsize = 7)
      par(mfrow=c(2,2), cex=1.1, mar = c(4.1, 4.1, 3.1, 1.1))
    }
    
    l1 <- resi[[f.type]]
    for(max.vin in names(l1)) {
      if(!do.eps | (max.vin %in% c("2002", "2004", "2006", "2008"))) {
        if(grepl("Base", max.vin)) { # just analyze Base models
          estimates[[paste(f.type, max.vin)]] <- coef.by.horizon(l1[[max.vin]])
        }
      }
    }
    
    if(do.eps) {
      par(par.old)
      #par(mfrow=c(1,1), cex=1)
      dev.off() 
    }
  }
  return(estimates)
}
estimates <- make.coef.plots(resi, FALSE)

make.coef.table <- function(estimates) {
  df.estimates <- data.frame(do.call(rbind, estimates))
  
  # Mean
  aggreg <- list()
  for(i in 3:6) {
    df.estimates[, i] <- as.numeric(as.character(df.estimates[, i]))
    df1 <- aggregate(df.estimates[, i], 
                     by=list(df.estimates$Type), FUN=function(x) {round(mean(x), 4)})
    colnames(df1) <- c("Type", colnames(df.estimates)[i])
    aggreg[[colnames(df.estimates)[i]]] <- df1
  }
  df.avg.estimates <- Reduce(function(x, y) merge(x, y, all=T, by= "Type"), aggreg, accumulate=F)
  df.avg.estimates$Vintage <- "Average"
  
  # SD
  aggreg <- list()
  for(i in 3:6) {
    df.estimates[, i] <- as.numeric(as.character(df.estimates[, i]))
    df1 <- aggregate(df.estimates[, i], 
                     by=list(df.estimates$Type), FUN=function(x) {round(sd(x), 4)})
    colnames(df1) <- c("Type", colnames(df.estimates)[i])
    aggreg[[colnames(df.estimates)[i]]] <- df1
  }
  df.sd.estimates <- Reduce(function(x, y) merge(x, y, all=T, by= "Type"), aggreg, accumulate=F)
  df.sd.estimates$Vintage <- "Stdv"
  
  df.estimates <- data.frame(rbind(df.estimates, df.avg.estimates, df.sd.estimates), row.names = NULL)
  for(i in 3:6) {
    df.estimates[, i] <- as.numeric(as.character(df.estimates[, i]))
  }
  df.estimates <- df.estimates[order(df.estimates$Type, df.estimates$Vintage), ]
  #rownames(df.estimates) <- df.estimates$Type
  #df.estimates$Type <- NULL
  
  invisible(df.estimates)
}
df.estimates <- make.coef.table(estimates)

print(xtable::xtable(df.estimates, digits = 4, include.rownames=FALSE))

(aggregate(Mkt.RF ~ Fund.Type + Model, df.resi[(df.resi$Horizon >= 120) & (df.resi$Horizon <= 180), ], mean))


#df.one.step <- resi$VC$`2002`
#coef.by.horizon(df.one.step)
#View(df.one.step)

# plot SDF LOG-RETURN index ----
plot.sdf.return <- function(df.resi, fund.types, 
                            max.vin = 2010, start.date = "1990-01-01", 
                            ymax=5, do.eps = FALSE) {
  # EPS init
  if(do.eps) {
    setEPS()
    postscript(paste("EPS1/", "SDF_realizations_", weighting, ".eps", sep=""), 
               width = 5.5, height = 4, family = "Helvetica", pointsize = 5)
    par(mfrow=c(1,1), cex=1.7, mar = c(4.1, 4.1, 3.1, 1.1))
  }
  
  # get df_pubin
  df_pubin <- readRDS("data_in/list_pubin.RDS")$df[, 1:7]
  df_pubin <- df_pubin[df_pubin$Date >= as.Date(start.date), ]
  df_pubin$One <- 1
  
  # plot market return
  if(weighting == "EW") {tag <- "Equal Weighted"}
  if(weighting == "VW") {tag <- "Fund-Size Weighted"}
  
  plot(df_pubin$Date, (cumsum(log(1 + df_pubin$Mkt.RF + df_pubin$RF))), 
       type = "l", lwd = 2, ylim=c(0, ymax),
       main= paste("SDF Realizations", tag), xlab = "Date", ylab="Cumulative Log Return")
  
  df.resi0 <- df.resi
  j <- 1
  for(fund.type in fund.types) {
    print(fund.type)
    j <- j + 1
    df.resi <- df.resi0
    df.resi <- df.resi[df.resi$Fund.Type == fund.type, ]
    df.resi <- df.resi[df.resi$Max.Vin == max.vin, ]
    df.resi <- df.resi[df.resi$Horizon >= 120, ]
    df.resi <- df.resi[df.resi$Horizon <= 180, ]
    df.resi <- df.resi[is.finite(df.resi$value), ]
    #df.resi <- df.resi[df.resi$kkt1, ]
    #df.resi <- df.resi[df.resi$kkt2, ]
    df.par <- df.resi[, ff]
    print(nrow(df.par))
    
    for(i in 1:nrow(df.par)) {
      par <- as.numeric(df.par[i, ])
      names(par) <- colnames(df.par)
      col <- paste("CompoundReturn", fund.type, i, max.vin, sep = "_")
      
      linear <- as.matrix(df_pubin[, names(par)]) %*% par
      
      if(expontential.affine) {
        log.return <- cumsum(df_pubin$RF + linear)
      } else {
        log.return <- cumsum(log(1 + df_pubin$RF + linear))
      }

      if(anyNA(log.return)) {
        print(col)
      }
      df_pubin[, col] <- log.return
      lines(df_pubin$Date, df_pubin[, col], col=j, lwd=1)
    }
  }
  
  legend("topleft", bty = "n", legend = c("Market", paste(fund.types, "SDFs")), 
         col = 1:(1+length(fund.types)), lty=1, lwd=2)
  

  if(do.eps) {
    par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
  
  invisible(df_pubin)
}
df.pubin <- plot.sdf.return(df.resi, c("VC", "BO", "PE"), 2010, do.eps=FALSE)

sdf.ensamble <- function(max.vins) {
  list.pubin <- list()
  for(max.vin in max.vins) {
    df.pubin <- plot.sdf.return(df.resi, c("VC", "BO"), max.vin)
    list.pubin[[paste(max.vin)]] <- df.pubin[, c(1, 9:ncol(df.pubin))]
  }
  df <- Reduce(function(x, y) merge(x, y, all=T, by=c("Date")), list.pubin, accumulate=F)
  invisible(df)
}

# df.pubin <- sdf.ensamble(2000:2010)

# calc NPV, Direct Alpha, KS05-PME ----

calc.npv <- function(r, cf) {
  discount <- exp(-r*(1:length(cf)/12))
  return(sum(cf*discount))
}

calc.direct.alpha <- function(dcfs) {
  log.alpha <- try(uniroot(calc.npv, cf=dcfs, interval = c(-10,10))$root)
  if(class(log.alpha) == "try-error") return(NA)
  direct.alpha <- exp(log.alpha) - 1 
  return(direct.alpha)
}

log.alpha <- calc.direct.alpha(c(2,42,32,0.4))

plot.npv <- function(vintages=1990:2010, metric = "NPV", do.eps=FALSE) {
  
  # EPS init
  if(do.eps) {
    setEPS()
    postscript(paste("EPS1/", "NPV_histogram_", weighting, ".eps", sep=""), 
               width = 5.5, height = 2.5, family = "Helvetica", pointsize = 5)
    par(mfrow=c(1,2), cex=1.7, mar = c(4.1, 4.1, 3.1, 1.1))
  }
  
  for(fund.type in fund.types[1:2]) {
    print(fund.type)
    out <- c()
    for(vin in vintages) {
      df <- RF_preqin.basis$original[[fund.type]]$df[[as.character(vin)]]
      df <- merge(df, df.pubin, by = "Date")
      
      cfs <- df$CashFlow
      cfs[length(cfs)] <- cfs[length(cfs)] + df$NAV[length(cfs)]
      
      rel.cols <- colnames(df.pubin)[grepl(strsplit(fund.type,"_")[[1]][1], colnames(df.pubin))]
      for(col in rel.cols) {
        f.type <- strsplit(fund.type, "_")[[1]][1]
        dcfs <- cfs / exp(df[, col]) * exp(df[1, col])
        
        if(metric == "KS-PME") kpi <- - sum(dcfs[dcfs>0]) / sum(dcfs[dcfs<0]) - 1 
        if(metric == "NPV") kpi <- sum(dcfs)
        if(metric == "Direct Alpha") kpi <- calc.direct.alpha(dcfs)
        
        out <- c(out, kpi)
      }
    }
    print(length(out))
    hist(out, 30, main = f.type, xlab = metric, xlim=c(-1,1), freq = FALSE)
    rug(out)
    abline(v=mean(out, na.rm = TRUE), col="red", lwd=2)
    abline(v=median(out, na.rm = TRUE), col="blue", lwd=2)
    
    print(summary(out))
  }
  
  if(do.eps) {
    par(mfrow=c(1,1), cex=1)
    dev.off()
  }

}
system.time(
  plot.npv(1990:2010, metric="Direct Alpha", do.eps=FALSE)
)



# fama french factors ----

df.ff <-read.csv('data_in/fama_french_factors.csv')
df.ff <- df.ff[complete.cases(df.ff), ]
df.ff[, -1] <- apply(df.ff[, -1], 2, function(x) {
  cumprod(1+x)
})

