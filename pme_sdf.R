##################################
## Private Market Equivalent - SDF
## Elastic-Net Regularization
##################################
# Prologue ------------
if(sys.nframe() == 0L) rm(list = ls())
# pme_sdf <- new.env()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Preqin Basis ----
#RF_preqin.basis <- readRDS("preqin_basis/RF_preqin_basis_boostrap_seed1.RDS")
RF_preqin.basis <- list()
RF_preqin.basis$original <- readRDS("preqin_basis/RF_valueweighted_preqin_basis.RDS")[["original"]]

names(RF_preqin.basis$original$Real$df)

# create cross-validation preqin_bases
cv.partitions <- list(
  cv0 = list(train = as.character(seq(1980,2020, by=1)), validate = as.character(seq(1980,2020, by=1))),
  cv1 = list(train = as.character(seq(1980,2020, by=2)), validate = as.character(seq(1981,2021, by=2))),
  cv2 = list(train = as.character(seq(1981,2021, by=2)), validate = as.character(seq(1980,2020, by=2))),
  cv3 = list(train = as.character(seq(1980,1997, by=1)), validate = as.character(seq(1998,2020, by=1))),
  cv4 = list(train = as.character(seq(1998,2020, by=1)), validate = as.character(seq(1980,1997, by=1))),
  cv5 = list(train = as.character(seq(1980,1992, by=1)), validate = as.character(seq(1998,2020, by=1))),
  cv6 = list(train = as.character(seq(1993,2020, by=1)), validate = as.character(seq(1980,1992, by=1))),
  cv7 = list(train = as.character(c(seq(1980,1990, by=1),seq(2001,2020, by=1))),
             validate = as.character(seq(1991,2000, by=1))),
  cv8 = list(train = as.character(seq(1991,2000, by=1)),
             validate = as.character(c(seq(1980,1990, by=1),seq(2001,2020, by=1)))),
  cv9 = list(train = as.character(c(seq(1980,2020, by=4),seq(1981,2021, by=4))),
             validate = as.character(c(seq(1982,2022, by=4),seq(1983,2023, by=4)))),
  cv10 = list(validate = as.character(c(seq(1980,2020, by=4),seq(1981,2021, by=4))),
              train = as.character(c(seq(1982,2022, by=4),seq(1983,2023, by=4)))),
  cv11 = list(train = as.character(c(seq(1980,2020, by=6),seq(1981,2021, by=6),seq(1982,2022, by=6))),
             validate = as.character(c(seq(1983,2023, by=6),seq(1984,2024, by=6),seq(1985,2025, by=6)))),
  cv12 = list(validate = as.character(c(seq(1980,2020, by=6),seq(1981,2021, by=6),seq(1982,2022, by=6))),
              train = as.character(c(seq(1983,2023, by=6),seq(1984,2024, by=6),seq(1985,2025, by=6)))),
  cv13 = list(train = as.character(c(seq(1980,2020, by=8),seq(1981,2021, by=8),
                                     seq(1982,2022, by=8), seq(1983,2023, by=8))),
              validate = as.character(c(seq(1984,2024, by=8),seq(1985,2025, by=8),
                                        seq(1986,2026, by=8),seq(1987,2027, by=8)))),
  cv14 = list(validate = as.character(c(seq(1980,2020, by=8),seq(1981,2021, by=8),
                                     seq(1982,2022, by=8), seq(1983,2023, by=8))),
              train = as.character(c(seq(1984,2024, by=8),seq(1985,2025, by=8),
                                        seq(1986,2026, by=8),seq(1987,2027, by=8))))
)

if(TRUE) {
  # leave one vintage out cross validation
  seq <- as.character(seq(1986,2010, by=1))
  cv.partitions <- list(cv0 = list(train = as.character(seq(1986,2010, by=1)), validate = as.character(seq(1986,2010, by=1))))
  for(vin in seq) {
    cv.partitions[[paste("cv",vin,sep="")]] <- list(
      train = seq[!(seq %in% vin)],
      validate = vin
    )
  }
}

fund.types <- names(RF_preqin.basis$original)

models <- list(
  m1 = c("Mkt.RF"),
  m2 = c("Mkt.RF", "SMB"),
  m3 = c("Mkt.RF", "HML"),
  m4 = c("Mkt.RF", "CMA"),
  m5 = c("Mkt.RF", "RMW"),
  m6 = c("Mkt.RF", "RMW", "SMB"),
  m7 = c("Mkt.RF", "CMA", "SMB"),
  m8 = c("Mkt.RF", "RMW", "HML"),
  m9 = c("Mkt.RF", "CMA", "HML"),
  m10 = c("Mkt.RF", "RMW", "CMA"),
  m11 = c("Mkt.RF", "SMB", "HML"),
  m12 = c("Mkt.RF", "SMB", "HML", "CMA"),
  m13 = c("Mkt.RF", "SMB", "HML", "RMW"),
  m14 = c("Mkt.RF", "SMB", "RMW", "CMA"),
  m15 = c("Mkt.RF", "HML", "RMW", "CMA"),
  m16 = c("Mkt.RF", "HML", "SMB", "RMW", "CMA")
)

all.combinations <- function(vector) {
  # https://spartanideas.msu.edu/2019/02/21/binary-integer-conversion-in-r/
  n <- length(vector)
  no.combinations <- 2^n-1
  fromID <- function(id) { as.integer(head(intToBits(id), n)) }
  moli <- apply(sapply(1:no.combinations, fromID) == 1, 2, function(x) {vector[x] })
  names(moli) <- paste("m", 1:length(moli),sep="")
  return(moli)
}
models <- all.combinations(models[["m16"]])

for(name in names(models)) {
  models[[paste(name, "alpha", sep = ".")]] <- c("One", models[[name]])
  models[[name]] <- NULL
}

methods <- c("nlminb", "ucminf", "nlm", "L-BFGS-B") # in descending order


if(FALSE) {
  macro.vars <- c("TB3MS","T10Y3M","TEDRATE","USSLIND","VXOCLS","CFNAIDIFF","BAA10Y")
  lead.diff <- function(preqin.basis) {
    
    for(vintage in names(preqin.basis$df)) {
      df <- preqin.basis$df[[vintage]]
      for(col in macro.vars) {
        y <- df[, col] - df[,paste(col, "lag1",sep=".")]
        y[1] <- 0
        df[,paste(col, "diff1",sep=".")] <- y
      }
      preqin.basis$df[[vintage]] <- df
    }
    return(preqin.basis)
  }
  preqin.basis <- lead.diff(preqin.basis)
}


# GMM like Driessen et al (2012) ----
method <- "nlminb"
fund.type <- "BO"
do.exp <- TRUE
model <- names(models)[31]
prep <- "original"
max.vintage <- 2010
max.horizon <- 12
lambda.ridge <- 0.01
lambda.lasso <- 0.01
cv.part <- "cv0"

fun.once <- function(method, fund.type, do.exp, model, prep, max.vintage, max.horizon, 
                     lambda.ridge, lambda.lasso, cv.part) {
  
  preqin.basis <- RF_preqin.basis[[prep]][[fund.type]]
  
  # cross validation data split
  list.of.dfs.training <- list()
  list.of.dfs.validate <- list()
  for(vin in names(preqin.basis$df)) {
    if(as.numeric(vin) <= max.vintage) {
      if (vin %in% cv.partitions[[cv.part]]$train) {
        list.of.dfs.training[[vin]] <- preqin.basis$df[[vin]]
      }
      if (vin %in% cv.partitions[[cv.part]]$validate) {
        list.of.dfs.validate[[vin]] <- preqin.basis$df[[vin]]
      }
    }
  }
  
  deg.free.train <- length(names(list.of.dfs.training))
  deg.free.valid <- length(names(list.of.dfs.validate))
  
  if(deg.free.train == 0 | deg.free.valid == 0) {
    return(NULL)
  }

  vars <- models[[model]]
  len <- length(vars)
  pars <- rep(0, len)
  
  npv.sqr <- function(par, list.of.dfs){
    
    discounted.cashflow <- list()
    for(vintage.year in names(list.of.dfs)) {
      df <- list.of.dfs[[vintage.year]]
      #df <- df[1:(15*12), ] # cut after 15 years
      
      if(do.exp) {
        linear <- log(1 + as.matrix(df[, vars])) %*% par # log-returns
        df$CompoundReturn <- exp(cumsum(log(1 + df$RF) + linear))
      } else {
        linear <- as.matrix(df[, vars]) %*% par
        df$CompoundReturn <- cumprod(1 + df$RF + linear)
      }
      
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
    out <- out + lambda.ridge * sum(par[2:len]^2) + lambda.lasso * sum(abs(par[2:len]))
    
    #if(FALSE) {
    #  vacf <- apply(dcf, 2, function(x){
    #    sum(acf(x, na.action = na.exclude, plot=FALSE)$acf^2)
    #  })
    #} else {
    #  vacf <- rep(1, max.horizon+1) # no acf weighting
    #}
    # obs <- sum(!is.na(dcf))
    # dcf[is.na(dcf)] <- 0
    # out <- log(sum((dcf %*% diag(vacf))^2) / obs)
    
    return(out)
  }
  
  res <- optimx::optimx(par = pars, list.of.dfs = list.of.dfs.training, fn = npv.sqr, method = method)
  
  colnames(res)[1:len] <- vars
  beta <- as.numeric(res[method, vars])
  
  loss.valid <- npv.sqr(beta, list.of.dfs.validate) / deg.free.valid
  loss.train <- npv.sqr(beta, list.of.dfs.training) / deg.free.train
  
  SE <- function(res){
    
    if(len > 1) {
      coef <- coef(res)[method, ]
    } else {
      coef <- res[method, vars]
    }
    
    result <- data.frame(Coef = coef)
    
    if (TRUE) {
      # just valid for MLE (with negative hessian)
      hessian <- attr(res, "details")[, "nhatend"]
      fisher_info <- solve(hessian[[1]]) # sign issue (maximization vs. minimization)
      SE.MLE <- sqrt(diag(fisher_info))
      result$SE.MLE <- SE.MLE
      result$t_value.MLE <- result$Coef / result$SE.MLE
      #result$t_value.MLE <- pnorm(-abs(result$t_value.MLE)) * 2
      result$p_value.MLE <- pt(-abs(result$t_value.MLE), df = deg.free.train) * 2
    }

    
    if(len > 1) {
      rownames(result) <- colnames(coef(res))
    } else {
      rownames(result) <- vars
    }
    result$factor <- rownames(result)
    
    # run details
    variables <- c("model", "do.exp", "fund.type", "method", "prep", 
                   "max.vintage", "max.horizon", "lambda.ridge", "lambda.lasso", "cv.part")
    for(variable in variables) {
      result[, variable] <- get(variable)
    }
    
    for(col in c("kkt1", "kkt2", "xtimes")) {
      result[, col] <- res[method, col]
    }
    
    result$loss.train <- loss.train
    result$loss.valid <- loss.valid
    
    result$df.train <- deg.free.train
    result$df.valid <- deg.free.valid
    
    return(result)
  }
  return(SE(res))
}
system.time(
  test <- fun.once("nlminb", fund.type, do.exp, model, prep, max.vintage, max.horizon, 
                   lambda.ridge, lambda.lasso, cv.part)
)
test


dcf <- matrix(1:30,10,3)
dcf[10,3] <- NA
vacf <- apply(dcf, 2, function(x){
  sum(acf(x, na.action = na.exclude)$acf[2]^2)
})
dcf[is.na(dcf)] <- 0
sum((dcf %*% diag(vacf))^2)



# multiple runs ----

mult.run <- function(methods, fund.types, do.exps, models, preps, max.vintages, max.horizons,lambdas, cv.parts) {
  out <- list()
  
  for(method in methods) {
    for(fund.type in fund.types) {
      print(fund.type)
      for(do.exp in do.exps) {
          for(model in models) {
            print(model)
            for(prep in preps) {
              print(prep)
              for(max.vintage in max.vintages) {
                print(max.vintage)
                for(max.horizon in max.horizons) {
                  print(max.horizon)
                  for(lambda in lambdas) {
                    for(cv.part in cv.parts) {
                      df.se <- fun.once(method = method, 
                                        fund.type = fund.type,
                                        do.exp = do.exp, 
                                        model = model,
                                        prep = prep,
                                        max.vintage = max.vintage,
                                        max.horizon = max.horizon,
                                        lambda.ridge = lambda,
                                        lambda.lasso = lambda,
                                        cv.part = cv.part) 
                      run.name <- paste(method, fund.type, do.exp, model, prep, 
                                        max.vintage, max.horizon, lambda, cv.part)
                      out[[run.name]] <- df.se 
                    }
                  }
                }
              }
            }
          }
      }
    }
  }
  df.se <- data.frame(do.call(rbind, out))
  rownames(df.se) <- NULL
  
  return(df.se)
}

system.time(
if(FALSE) {
    df.se <- mult.run(methods = "nlminb", 
                      fund.types = fund.types,
                      do.exps = c(TRUE),
                      models = names(models)[31], 
                      preps = names(RF_preqin.basis),
                      max.vintages = c(2010),
                      max.horizons = c(12),
                      lambdas = seq(0, 0.3, 0.01),
                      cv.parts = names(cv.partitions))
  saveRDS(df.se, "data_out/20190707_df.se_valwei_newcon_elasticnet_lovo.RDS")
} else {
  df.se <- readRDS("data_out/20190707_df.se_valwei_newcon_elasticnet_block.RDS")
  #df.se <- readRDS("data_out/20190707_df.se_valwei_newcon_elasticnet_lovo.RDS")
}
)

# Cross validation error ------

df.cross.val <- aggregate(cbind(mean.loss.valid = loss.valid, mean.loss.train = loss.train) ~ lambda.ridge + fund.type, df.se[df.se$cv.part != "cv0", ], mean)
df.cross.val

df.se <- merge(df.se, df.cross.val, by = c("lambda.ridge", "fund.type"))

# extract original ----
min.error <- function(df = df.se, 
                      do.exp = c(TRUE), 
                      max.vin = c(2010), 
                      max.hor = c(12)) {
  df <- df[df$do.exp %in% do.exp, ]
  df <- df[df$max.vintage %in% max.vin, ]
  df <- df[df$max.horizon %in% max.hor, ]
  df <- df[df$cv.part == "cv0", ]
  #df <- df[df$lambda.ridge == 0.3, ]

  df.min <- aggregate(list(min.value = df$mean.loss.valid), list(fund.type = df$fund.type), min)
  df <- merge(df, df.min, by ="fund.type", all.x = TRUE)
  df$is.min <- ifelse(df$mean.loss.valid == df$min.value, TRUE, FALSE)
  df$min.value <- NULL
  return(df[df$is.min, ])
}
min.error() [, c("fund.type", "factor", "Coef", "model", "lambda.ridge")]

# calc SDF returns ----

#df.se <- df.se[df.se$model %in% names(models[1:5]), ]

plot.sdf.return <- function(fund.type = "BO", start.date = "1990-01-01", 
                            max.vin = c(2010), max.hor = c(12), tag = "elasticnet_block",
                            ymax=30, do.eps = FALSE, lambda=seq(0,0.3,0.01)) {
  
  df_pubin <- readRDS("data_in/list_pubin.RDS")$df[, 1:7]
  df_pubin <- df_pubin[df_pubin$Date >= as.Date(start.date), ]
  df_pubin$One <- 1
  
  df.ss <- df.se[df.se$fund.type == fund.type, ]
  df.ss <- df.ss[df.ss$do.exp == TRUE, ]
  df.ss <- df.ss[df.ss$max.vintage %in% max.vin, ]
  df.ss <- df.ss[df.ss$max.horizon %in% max.hor, ]
  
  df.ss$model.id <- paste(df.ss$model, df.ss$max.vintage, df.ss$max.horizon, 
                          df.ss$lambda.ridge, df.ss$lambda.lasso, sep = "_")
  df.ss$model.id <- as.factor(df.ss$model.id)
  best.cv.model.id <- df.ss$model.id[min(df.ss$mean.loss.valid) == df.ss$mean.loss.valid]
  best.cv.model.id <- as.character(unique(best.cv.model.id))
  best.lambda <- strsplit(best.cv.model.id,"_")[[1]][5]
  print(best.cv.model.id)
  df.ss <- df.ss[df.ss$cv.part == "cv0" & df.ss$lambda.ridge %in% lambda, ]
  
  average.alpha <- mean(df.ss$Coef[df.ss$factor == "One"]) # average alpha
  best.alpha <- df.ss$Coef[df.ss$factor == "One" & df.ss$model.id == best.cv.model.id]

  
  for(model.id in levels(df.ss$model.id)) {
    df_pubin[, model.id] <- log(1 + df_pubin$RF) # log risk-free rate
    df.ss1 <- df.ss[df.ss$model.id == model.id, ]
    for(i in 1:nrow(df.ss1)) {
      factor <- df.ss1$factor[i]
      coef <- df.ss1$Coef[i]
      df_pubin[, model.id] <- df_pubin[, model.id] + log(1 + df_pubin[, factor]) * coef
    }
  }
  
  # Model averaging
  df.weight <- df.ss[df.ss$fund.type == fund.type, c("model.id", "mean.loss.valid")]
  df.weight <- df.weight[!duplicated(df.weight$model.id), ]
  df.weight$weight <- mean(df.weight$mean.loss.valid) / df.weight$mean.loss.valid
  df.weight$weight <- df.weight$weight / sum(df.weight$weight)
  rownames(df.weight) <- df.weight$model.id
  w <- df.weight[levels(df.ss$model.id), "weight"]
  m <- as.matrix(df_pubin[, levels(df.ss$model.id)])
  average.return <- m %*% w
  
  average.return <- rowMeans(df_pubin[, levels(df.ss$model.id)])

  
  if(is.na(ymax)) {
    ymax <- max(apply(df_pubin[, levels(df.ss$model.id)], 2, function(x) {max(exp(cumsum(x)))}))
  }
  
  
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
  for(model.id in levels(df.ss$model.id)) {
    lines(df_pubin$Date, exp(cumsum(df_pubin[, model.id])), col="grey")
  }
  
  #lines(df_pubin$Date, exp(cumsum( average.return )), col="red", lwd=2)
  #print(colnames(df_pubin))
  lines(df_pubin$Date, exp(cumsum( df_pubin[, best.cv.model.id] )), col="red", lwd=2)
  
  legend("topleft", bty = "n", 
         legend = c("Selected SDF", "Fama-French Mkt", paste("Max. Vintage:", paste(max.vin, collapse = ", ")), 
                    paste("Max. Horizon:", paste(max.hor, collapse = ", ")),
                    paste("Monthly alpha: ", eval(round(best.alpha*100,3)), "%", sep=""),
                    paste("Ridge-Lasso Coef:", best.lambda,sep = "")),
         lwd=c(2,2,NA,NA,NA,NA), lty=c(1,1,NA,NA,NA,NA), col=c("red", "black", NA,NA,NA,NA))
  
  
  if(do.eps) {
    par(mfrow=c(1,1), cex=1)
    dev.off() 
  }
  
  return(average.alpha)
}

plot.sdf.return("BO")
plot.sdf.return("VC")
plot.sdf.return("Debt")
plot.sdf.return("Real")

#sapply(c(1,2,5,10,12,15,20,25), function(x) plot.sdf.return("BO", max.vin=c(2005, 2000, 2015), max.hor = x, do.eps=FALSE))

# extract bootstrap ----

boostrap.levels = list(Model = df.se$model, Alpha = df.se$do.alpa, Exp = df.se$do.exp, 
                       fund.type = df.se$fund.type, Factor = df.se$factor)
df.mean <- aggregate(list(Coef = df.se$Coef, value = df.se$value), by = boostrap.levels, mean)
df.sd <- aggregate(list(Coef = df.se$Coef), by = boostrap.levels, sd)
df.ms <- merge(df.mean, df.sd, by = c("Model", "Alpha", "Exp", "fund.type", "Factor"), suffixes = c(".Mean", ".SD"))
df.ms$t_value <- df.ms$Coef.Mean / df.ms$Coef.SD


# Dummy Moment Condition Matrix ----

dummy.moment.condition.matrix <- function() {
  rowz <- seq(2005,2017)
  colz <- seq(0,6)
  df.price <- matrix(round(rnorm(length(rowz)*length(colz),0,5),2), nrow = length(rowz), ncol=length(colz))
  colnames(df.price) <- colz
  rownames(df.price) <- rowz
  
  for(c in colnames(df.price)) {
    for(r in rownames(df.price)) {
      if(2017 - as.numeric(r) < as.numeric(c)) {
        df.price[r, c] <- NA
      }
    }
  }
  xtable::xtable(df.price)
}

dummy.moment.condition.matrix()


