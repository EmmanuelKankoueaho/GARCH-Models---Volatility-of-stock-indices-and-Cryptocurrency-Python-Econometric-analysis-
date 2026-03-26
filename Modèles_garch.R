# Installation si nécessaire
install.packages("rugarch")

# Chargement
library(rugarch)

setwd("C:/Users/emman/ADVANCED ECONOMETRICS")

install.packages("openxlsx")

library(openxlsx)

# Exemple : charger tes données (CSV, Excel possible aussi)
data <- read.xlsx("Bitcoin.xlsx")

# Supposons que la variable de prix s'appelle "close"
data$close <- as.numeric(gsub(",", "", data$close))

# Log-returns
#data$log_return <- diff(log(data$close))

install.packages("tidyverse")
library(tidyverse)
data <- data %>%
  mutate(log_return = log(close) - lag(log(close)))

# Suppression des NA
returns <- na.omit(data$log_return)

estimate_model <- function(model_name, p=1, q=1, ar=1) {
  
  spec <- ugarchspec(
    variance.model = list(
      model = model_name,
      garchOrder = c(p, q)
    ),
    mean.model = list(
      armaOrder = c(ar, 0),   # AR(ar)
      include.mean = TRUE
    ),
    distribution.model = "norm"  # ou "std"
  )
  
  fit <- ugarchfit(spec = spec, data = returns, solver = "hybrid")
  return(fit)
}

fit_garch <- estimate_model("sGARCH")

fit_egarch <- estimate_model("eGARCH")

fit_tgarch <- estimate_model("gjrGARCH")

fit_aparch <- estimate_model("apARCH")

fit_cgarch <- estimate_model("csGARCH")



show(fit_garch)
show(fit_egarch)
show(fit_tgarch)
show(fit_aparch)
show(fit_cgarch)

infocriteria(fit_garch)
infocriteria(fit_egarch)
infocriteria(fit_tgarch)
infocriteria(fit_aparch)
infocriteria(fit_cgarch)

#archlm(residuals(fit_garch, standardize = TRUE))

models <- list(
  GARCH  = fit_garch,
  EGARCH = fit_egarch,
  TGARCH = fit_tgarch,
  APARCH = fit_aparch,
  CGARCH = fit_cgarch
)

results <- data.frame(
  Model = names(models),
  AIC = sapply(models, function(x) infocriteria(x)[1]),
  BIC = sapply(models, function(x) infocriteria(x)[2])
)

print(results)


c
install.packages("FinTS")
library(FinTS)
library(tseries)

garch_diagnostics <- function(fit, arch_lag = 5, q_lag = 10) {
  
  # Résidus standardisés
  z <- residuals(fit, standardize = TRUE)
  
  # ARCH LM test
  arch_test <- ArchTest(z, lags = arch_lag)
  
  # Ljung-Box sur les résidus au carré
  q2_test <- Box.test(z^2, lag = q_lag, type = "Ljung-Box")
  
  # Jarque-Bera
  jb_test <- jarque.bera.test(z)
  
  return(list(
    ARCH_stat = as.numeric(arch_test$statistic),
    ARCH_pval = arch_test$p.value,
    Q2_stat   = as.numeric(q2_test$statistic),
    Q2_pval   = q2_test$p.value,
    JB_stat   = as.numeric(jb_test$statistic),
    JB_pval   = jb_test$p.value
  ))
}

diag_results <- lapply(models, garch_diagnostics)

diag_table <- do.call(rbind, lapply(names(diag_results), function(name) {
  cbind(Model = name, as.data.frame(diag_results[[name]]))
}))

print(diag_table)



# Installation si nécessaire
install.packages("rugarch")

# Chargement
library(rugarch)

setwd("C:/Users/emman/ADVANCED ECONOMETRICS")

install.packages("openxlsx")

library(openxlsx)

# Exemple : charger tes données (CSV, Excel possible aussi)
data <- read.csv("S&P 500 Data.csv")

# Supposons que la variable de prix s'appelle "close"
data$Price <- as.numeric(gsub(",", "", data$Price))

# Log-returns
#data$log_return <- diff(log(data$Price))

install.packages("tidyverse")
library(tidyverse)
data <- data %>%
  mutate(log_return = log(Price) - lag(log(Price)))

# Suppression des NA
returns <- na.omit(data$log_return)

estimate_model <- function(model_name, p=1, q=1, ar=1) {
  
  spec <- ugarchspec(
    variance.model = list(
      model = model_name,
      garchOrder = c(p, q)
    ),
    mean.model = list(
      armaOrder = c(ar, 0),   # AR(ar)
      include.mean = TRUE
    ),
    distribution.model = "norm"  # ou "std"
  )
  
  fit <- ugarchfit(spec = spec, data = returns, solver = "hybrid")
  return(fit)
}

fit_garch <- estimate_model("sGARCH")

fit_egarch <- estimate_model("eGARCH")

fit_tgarch <- estimate_model("gjrGARCH")

fit_aparch <- estimate_model("apARCH")

fit_cgarch <- estimate_model("csGARCH")



show(fit_garch)
show(fit_egarch)
show(fit_tgarch)
show(fit_aparch)
show(fit_cgarch)

infocriteria(fit_garch)
infocriteria(fit_egarch)
infocriteria(fit_tgarch)
infocriteria(fit_aparch)
infocriteria(fit_cgarch)

#archlm(residuals(fit_garch, standardize = TRUE))

models <- list(
  GARCH  = fit_garch,
  EGARCH = fit_egarch,
  TGARCH = fit_tgarch,
  APARCH = fit_aparch,
  CGARCH = fit_cgarch
)

results <- data.frame(
  Model = names(models),
  AIC = sapply(models, function(x) infocriteria(x)[1]),
  BIC = sapply(models, function(x) infocriteria(x)[2])
)

print(results)


install.packages("FinTS")
library(FinTS)
library(tseries)

garch_diagnostics <- function(fit, arch_lag = 5, q_lag = 10) {
  
  # Résidus standardisés
  z <- residuals(fit, standardize = TRUE)
  
  # ARCH LM test
  arch_test <- ArchTest(z, lags = arch_lag)
  
  # Ljung-Box sur les résidus au carré
  q2_test <- Box.test(z^2, lag = q_lag, type = "Ljung-Box")
  
  # Jarque-Bera
  jb_test <- jarque.bera.test(z)
  
  return(list(
    ARCH_stat = as.numeric(arch_test$statistic),
    ARCH_pval = arch_test$p.value,
    Q2_stat   = as.numeric(q2_test$statistic),
    Q2_pval   = q2_test$p.value,
    JB_stat   = as.numeric(jb_test$statistic),
    JB_pval   = jb_test$p.value
  ))
}

diag_results <- lapply(models, garch_diagnostics)

diag_table <- do.call(rbind, lapply(names(diag_results), function(name) {
  cbind(Model = name, as.data.frame(diag_results[[name]]))
}))

print(diag_table)


