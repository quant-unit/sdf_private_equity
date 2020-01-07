############################
## Public Market Replication
## Componentwise Boosting
############################
# Prologue ------------
if(sys.nframe() == 0L) rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(mboost)


# Preqin Basis ----
weighting <- c("EW")

#RF_preqin.basis <- readRDS("preqin_basis/RF_preqin_basis_boostrap_seed1.RDS")
RF_preqin.basis <- list()
RF_preqin.basis$VW <- readRDS("preqin_basis/RF_valueweighted_preqin_basis.RDS")[["original"]]
RF_preqin.basis$EW <- readRDS("preqin_basis/RF_equalweighted_preqin_basis.RDS")[["original"]]

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
        no.funds.total = no.funds.total + RF_preqin.basis$original[[fund.type0]]$no.funds[[vintage]]
      }
    }
    RF_preqin.basis$original[[fund.type0]]$df <- lod
    #print(paste(fund.type0, no.funds.total))
    RF_preqin.basis$original[[fund.type0]]$no.funds.total <- no.funds.total
  }
  return(RF_preqin.basis)
}


# Prepare Pseudo X ----
type <- "VC_EW"
max.vintage <- 2010
lambda <- 0.025 / 12 

prep.df.x <- function(type, max.vintage, lambda) {
  preqin.basis <- trim.preqin.basis(RF_preqin.basis, max.vintage)
  
  Vintages <- list()
  for(vintage in names(preqin.basis$original[[type]]$df)) {
    df <- preqin.basis$original[[type]]$df[[vintage]]
    final.age <- 15
    df <- df[df$Age < final.age, ]
    df$CashFlow[nrow(df)] <- df$CashFlow[nrow(df)] + df$NAV[nrow(df)]
    
    # new numeraire
    df$Numeraire.Return <- df$Mkt.RF + df$RF
    df$Numeraire.Index <- cumprod(1 + df$Numeraire.Return)
    
    ResponseFactorPredictor <- list()
    ResponseFactorPredictor[["Discounted.CF"]] <- sum(df$CashFlow / df$Numeraire.Index) # * df$Numeraire.Index[nrow(df)]
    
    factor.set <- c("Mkt.RF", "SMB", "HML", "CMA", "RMW")
    # predictor.set <- preqin.basis$original[[type]]$predictor.set
    predictor.set <- c("One", "Age")
    
    for(factor in factor.set) {
      for(predictor in predictor.set) {
        x <- sum(df[, predictor] * (df[, factor] - lambda) * df$NAV / df$Numeraire.Index)  # * df$Numeraire.Index[nrow(df)]
        ResponseFactorPredictor[[paste(factor,predictor,sep="_x_")]] <- x
      }
    }
    Vintages[[paste(vintage)]] <- data.frame(ResponseFactorPredictor)
  }
  df.x <- do.call(rbind, Vintages)
  return(df.x)
}
df.x <- prep.df.x(type, max.vintage, lambda)

# boosting -----
types <- c("VC_EW", "VC_VW")
max.vintages <- 2000:2010
lambdas <- seq(0,4,1) / 100 / 12

iterated.boosting <- function(types, max.vintages, lambdas, stable.coef.names) {
  resi <- list()
  for(type in types) {
    for(max.vintage in max.vintages) {
      for(lambda in lambdas) {
        
        df.x <- prep.df.x(type, max.vintage, lambda)
        
        if( length(stable.coef.names) > 0 ) {
          predictors <- paste(stable.coef.names, collapse= "+")
        } else {
          predictors <- "."
        }
        fmla <- as.formula(paste("Discounted.CF ~ ", predictors, "-1"))
        
        bm <- glmboost(fmla, data = df.x, 
                       center = FALSE, offset = 0, family = Gaussian(),
                       control = boost_control(risk = "inbag", mstop = 2000, stopintern = TRUE))
        
        set.seed(1)
        cv.list <- list()
        iter <- 30
        for(split in c(2)) {
          x <- c()
          for(i in 1:iter) {
            x <- c(x, mstop(cvm <- cvrisk(bm, folds = cv(model.weights(bm), type="kfold", B=split) ) ) )
          }
          cv.list[[paste0(split, 'fold')]] <- mean(x)
        }
        cv.list
        
        
        #folds <- diag(2, 25) ; folds[folds == 0] <- 1 ; folds[folds == 2] <- 0
        #mstop(cvm <- cvrisk(bm, folds = folds ) )
        plot(cvm)
        
        bm <- bm[cv.list$`2fold`] # 2-fold cv is most conservative
        
        #summary(bm)
        #plot(bm)
        #cbind(df.x$Discounted.CF, predict(bm))
        run.name <- paste(type, max.vintage, lambda)
        print(run.name)
        
        if( length(stable.coef.names) > 0 ) {
          resi[[run.name]] <- bm$coef(which = stable.coef.names)
        } else {
          resi[[run.name]] <- paste(names(bm$coef()), sign(unlist(bm$coef())))
        }
        
      }
    }
  }
  return(resi)
}

stability.booster <- function(types, max.vintages, lambdas) {
  resi <- iterated.boosting(types, max.vintages, lambdas, NULL)
  
  # select stablest
  tbl <- sort(table(unlist(resi)),T) / length(resi)
  #stable.coef.names <- unlist(lapply(names(tbl[tbl > 2/3]), function(x) strsplit(x, " ")[[1]][1]))
  stable.coef.names <- unlist(lapply(names(tbl[1:min(length(tbl), 3)]), function(x) strsplit(x, " ")[[1]][1]))
  
  resi <- iterated.boosting(types, max.vintages, lambdas, stable.coef.names)
  
  df.resi <- data.frame(do.call(rbind,lapply(resi, unlist)))
  df.resi  <- data.frame(do.call(rbind, 
                                    list(mean = colMeans(df.resi),
                                         stdv = apply(df.resi, 2, sd))))
  
  return(list(resi = resi, df.resi = df.resi, tbl = tbl))
}

system.time(
  x <- stability.booster(types, max.vintages, lambdas)
)



# construct return series -----
df <- RF_preqin.basis$original[[type]]$df$`1990`
coefs <- x$df.resi["mean", ]
r <- rep(0, nrow(df))
for(comp in names(coefs)) {
  split <- strsplit(comp, "_x_")
  r <- r + df[, split[[1]][1]] * df[, split[[1]][2]]
}
plot(df$Date, cumprod(1 + df$Numeraire.Return +r), type="l")
plot(df$Date, df$Numeraire.Return + r, type="l",col="red")

