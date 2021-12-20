library(xts)
library(fBasics) # e.g. basicStats()
library(tseries)# e.g. jarque.bera.test()
library(car) # e.g. durbinWatsonTest()
library(FinTS) # e.g. ArchTest()
library(urca)
#library(MSBVAR)
library(xts)
library(dplyr)
library(openxlsx)
library(tidyverse)

source('functions.R')

#setwd("C:\\Users\\DELL\\OneDrive\\Pulpit\\UW\\Zaawansowana Ekonometria II")
dane<-read.xlsx("Dane do pierwszego modelu.xlsx")
Dane_do_pierwszego_modelu <- dane
Dane_do_pierwszego_modelu$data<-seq(as.Date("1990/1/1"), as.Date("2021/7/1"), by = "quarter")
str(Dane_do_pierwszego_modelu$data)
#Podzielenie krajow na oddzielne datasety
kraje <- c('pl', 'fr', 'ger', 'uk', 'nl', 'dmk')
kraje1 <- c('Polska', 'Francja', 'Niemcy', 'Wielka Brytania', 'Holandia', 'Dania')
data <- list()
for (x in kraje) {
  data[[x]] <- Dane_do_pierwszego_modelu %>% dplyr::select(paste0(c('oil_price_', 'ree_'), x)) %>% 
    rename_all(function(x) gsub('_','',substr(x, 1, nchar(x)-3))) %>%
    mutate(oilprice_r = diff.xts(log(oilprice))) %>% mutate(ree_r=diff.xts(log(ree))) %>% 
    mutate(date = seq(as.Date("1990/1/1"), as.Date("2021/7/1"), by = "quarter")) %>%
    mutate(D08=as.numeric(date<='2008/12/31' & date>='2007/10/1')) %>%
    mutate(D20=as.numeric(date<='2020/9/30' & date>='2020/1/1'))
    
}
par(mfrow = c(3,2))
par(mar=c(5, 4, 4, 6) + 0.1)
for (x in kraje) {
  plot(data[[x]]$date, log(data[[x]]$ree), type='l', col='red')
  par(new = TRUE)
  plot(data[[x]]$date,log(data[[x]]$oilprice), type = "l",col="green", axes = FALSE, bty = "n", xlab = "", ylab = "")
  axis(side=4, at = pretty(range(log(data[[x]]$oilprice))))  
  mtext("oil prize", side=4, line=3)
}


#testy na obecność pierwiastka jednostkowego
Wyniki <- matrix(data=NA, nrow = 12, ncol = 2)
colnames(Wyniki)<- c("Bez Trendu  ", "Z trendem")

for (i in 1:6) {
  cat(str_to_upper(names(data)[i])) 
  uur(data[[i]]) %>% print
  cat('\n')
}

# test ADF od Dzik

library(lmtest)
dev.off()
testdf(diff(log(data[['pl']]$oilprice)), 2)

# testy na kointegrację
library(dynlm)
for (x in names(data)) {
  obj <-dynlm(data[[x]]$oilprice~data[[x]]$ree)
  cat(str_to_upper(x))
  ur.df(resid(obj), lags = 6, type = "none", selectlags = "AIC") %>% print
}

# VAR models -> Granger nonlinear casuality test
library(vars)
VARselect(data[[1]][-1,3:4], lag.max=8,
          type="const")[["selection"]]
var1 <- VAR(data[[2]][-1,3:4], p=1, type="const")
summary(var1)
test <- serial.test(var1, lags.pt=2, type="PT.asymptotic")
test <- serial.test(var1, lags.bg=2, type="BG")

resid(var1)

library(SDD)


# 7 summary statistics
library(e1071)
calcutalate_stats <- function(x) {
  returns <-  diff(log(x))
  stat <- ur.df(returns)@teststat[1]
  pval <- pnorm(stat) %>% round(.,5)
  x <- diff(log(x))*100
  return(c(mean(x), median(x), max(x), min(x), sd(x), skewness(x), kurtosis(x), pval, length(x)))
}
sum_stats <- NULL
for (x in c('ree', 'oilprice')) {
  for (y in names(data)) {
    ret <- calcutalate_stats(data[[y]][x] %>% unlist)
    sum_stats <- sum_stats %>% cbind(ret) %>% as.data.frame %>% 
      rename_with(.cols=last_col(), .fn=function(name) paste(x,y,sep='_'))
  }
}
rownames(sum_stats) <- c('Mean', 'Median', 'Maximum', 'Minimum', 'Std. Dev', 'Skewness', 'Kurtosis', 'Probability', 'n')

# return ploty
dev.off()
par(mfrow = c(3,2))
par(mar=c(4, 4, 1, 2) + 0.1)
for (x in kraje) {
  plot(data[[x]]$date[-1],data[[x]]$oilprice_r[-1], type = "l",col="green",xlab = "", ylab = "")
  lines(data[[x]]$date[-1],data[[x]]$ree_r[-1], type = "l",col="red")
  #plot(data[[x]]$date[-1], data[[x]]$ree_r[-1], type='l', col='red')
  #par(new = TRUE)
  #plot(data[[x]]$date[-1],data[[x]]$oilprice_r[-1], type = "l",col="green", axes = FALSE, bty = "n", xlab = "", ylab = "")
  #axis(side=4, at = pretty(range(data[[x]]$oilprice_r[-1])))  
  mtext('returns', side=2, line=2.5)
  mtext(kraje1[which(kraje==x)], side=1, line=2.5)
}

# 8 OLS and quantile regression models
# olsy
lm(ree_r~oilprice_r+D08+D20,data[[1]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100)) %>% summary
library(quantreg)
rq(ree_r~oilprice_r+D08+D20, tau = 0.5, data=data[[1]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100)) %>% summary
obj <-rq(ree_r~oilprice_r+D08+D20, tau = 0.5, data=data[[1]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100))
summary(obj, se = "boot")
obj <-rq(ree_r~oilprice_r+D08+D20, tau = 0.5, data=data[[1]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100)) %>% summary
obj$terms
