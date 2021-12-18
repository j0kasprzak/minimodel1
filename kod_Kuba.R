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

#setwd("C:\\Users\\DELL\\OneDrive\\Pulpit\\UW\\Zaawansowana Ekonometria II")
dane<-read.xlsx("Dane do pierwszego modelu.xlsx")
Dane_do_pierwszego_modelu <- dane
Dane_do_pierwszego_modelu$data<-seq(as.Date("1990/1/1"), as.Date("2021/7/1"), by = "quarter")
str(Dane_do_pierwszego_modelu$data)
#Podzielenie krajow na oddzielne datasety
kraje <- c('pl', 'fr', 'ger', 'uk', 'nl', 'dmk')
data <- list()
for (x in kraje) {
  data[[x]] <- Dane_do_pierwszego_modelu %>% select(paste0(c('oil_price_', 'ree_'), x)) %>% 
    rename_all(function(x) gsub('_','',substr(x, 1, nchar(x)-3))) %>%
    mutate(oilprice_r = diff.xts(log(oilprice))) %>% mutate(ree_l=diff.xts(log(ree))) %>% 
    mutate(date = seq(as.Date("1990/1/1"), as.Date("2021/7/1"), by = "quarter"))
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

df <- polska
uur<-function(df){
  Wyniki <- matrix(data=NA, nrow = 2, ncol = 6)
  colnames(Wyniki)<- c("Bez Trendu  ", "Z trendem", "F.diff", "PP bez trendu  ", "PP z trendem  " ," F.diff PP")
  oil_p<-names(df)[1]
  ree<-names(df)[2]
  rownames(Wyniki)<-c(oil_p, ree)

  for (i in names(df)[1:2]){
        Wyniki[i,1] = summary(ur.df(log(df[,i]), type = "none", selectlags = c("BIC")))@teststat[1]
        Wyniki[i,2] = summary(ur.df(log(df[,i]), type = "trend", selectlags = c("BIC")))@teststat[1]
        Wyniki[i,3] = summary(ur.df(diff(log(df[,i])), type = "none", selectlags = c("BIC")))@teststat[1]
        Wyniki[i,4] = summary(ur.pp(diff(log(df[,i])), model="constant"))@teststat[1]
        Wyniki[i,5] = summary(ur.pp(diff(log(df[,i])), model="trend"))@teststat[1]
        Wyniki[i,6] = summary(ur.pp(diff(log(df[,i])), model="constant"))@teststat[1]
        summary(ur.df(log(df[,1])))@teststat
      }
  return(Wyniki)
}

for (i in 1:6) {
  cat(str_to_upper(names(data)[i])) 
  uur(data[[i]]) %>% print
  cat('\n')
}

# test ADF od Dzik
testdf <- function(variable, adf_order) {
  results_adf <- data.frame(order = -1,
                            adf = 0,
                            p_adf = "",
                            bgodfrey = 0, p_bg = 0)
  variable <- variable[!is.na(variable)]
  
  for (order in 0:adf_order) {
    df.test_ <- ur.df(variable, type = c("drift"), lags = order)
    df_ <- df.test_@teststat[1]
    df_crit <- df.test_@cval[1, ]
    df_crit <- (df_ < df_crit) * 1
    p_adf <- ifelse(sum(df_crit) == 0,
                    ">10pct",
                    paste("<",
                          names(df_crit)[min(which(df_crit == 1))],
                          sep = "")
    )
    
    resids_ <- df.test_@testreg$residuals
    bgtest_ <- bgtest(resids_ ~ 1, order = 1)
    bgodfrey <- bgtest_$statistic
    names(bgodfrey) <- NULL
    p_bg <- bgtest_$p.value
    
    results_adf <- rbind(results_adf,
                         data.frame(order = order,
                                    adf = df_,
                                    p_adf = p_adf,
                                    bgodfrey = bgodfrey,
                                    p_bg = p_bg)
    )
  }
  
  results_adf <- results_adf[results_adf$order >= 0, ]
  
  plot(variable,
       type = "l",
       col = "blue",
       lwd = 2,
       main = "Plot of the examined variable")
  
  return(results_adf)
}
library(lmtest)
dev.off()
testdf(diff(log(polska$oil_price_pl)), 2)

# testy na kointegrację
library(dynlm)
for (x in names(data)) {
  obj <-dynlm(data[[x]]$oilprice~data[[x]]$ree)
  cat(str_to_upper(x))
  ur.df(resid(obj), lags = 6, type = "none", selectlags = "AIC") %>% print
}

#

