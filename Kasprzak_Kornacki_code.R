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

dane <- read.csv2('Dane_do_modelu.csv')
colnames(dane) <- gsub('russ', 'rus', colnames(dane))
Dane_do_pierwszego_modelu <- dane
Dane_do_pierwszego_modelu$data<-seq(as.Date("1990/1/1"), as.Date("2021/7/1"), by = "quarter")
str(Dane_do_pierwszego_modelu$data)
#Podzielenie krajow na oddzielne datasety
kraje <- c('pl', 'ger', 'rus', 'nor', 'hu', 'uk')
kraje1 <- c('Polska', 'Niemcy',  'Rosja', 'Norwegia', 'Węgry', 'Wielka Brytania')
data <- list()

r_asym <- function(x, type='plus') {
  y <- x
  if (type=='plus') {
    y[x<0] <- 0
  } else {
    y[x>0] <- 0
  }
  y
}

for (x in kraje) {
  data[[x]] <- Dane_do_pierwszego_modelu %>% dplyr::select(paste0(c('oil_price_', 'ree_'), x)) %>% 
    rename_all(function(x) gsub('_','',substr(x, 1, nchar(x)-3))) %>%
    mutate(oilprice_r = diff.xts(log(oilprice))) %>% mutate(ree_r=diff.xts(log(ree))) %>% 
    mutate(date = seq(as.Date("1990/1/1"), as.Date("2021/7/1"), by = "quarter")) %>%
    mutate(D08=as.numeric(date<='2008/12/31' & date>='2007/10/1')) %>%
    mutate(D20=as.numeric(date<='2020/9/30' & date>='2020/1/1')) %>%
    mutate(oilprice_r_p = r_asym(oilprice_r, 'plus')*100) %>%
    mutate(oilprice_r_n = r_asym(oilprice_r, 'minus')*100) %>% 
    filter(!is.na(oilprice))
}

dev.off()
png(file="log_time_series_plot.png", height = 700, width = 700)
par(mfrow = c(3,2))
par(mar=c(3, 4, 3, 4) + 0.1)
i=1
for (x in kraje) {
  plot(data[[x]]$date, log(data[[x]]$ree), type='l', col='red', xlab='', ylab='')
  par(new = TRUE)
  plot(data[[x]]$date,log(data[[x]]$oilprice), type = "l",col="green", axes = FALSE, bty = "n", xlab = "", ylab = "")
  axis(side=4, at = pretty(range(log(data[[x]]$oilprice[!is.na(data[[x]]$oilprice)]))))  
  mtext("cena ropy", side=4, line=2.5)
  mtext("kurs walutowy", side=2, line=2.5)
  
  mtext(kraje1[i], side=3, line=1)
  i <- i+1
}
dev.off()



#testy na obecność pierwiastka jednostkowego
Wyniki <- matrix(data=NA, nrow = 12, ncol = 2)
colnames(Wyniki)<- c("Bez Trendu  ", "Z trendem")
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
  uur(data[[i]]) %>% print  %>% cat('\n')
}
# testy na kointegrację
library(dynlm)
for (x in names(data)) {
  obj <-dynlm(data[[x]]$ree~data[[x]]$oilprice)
  cat(str_to_upper(x))
  summary(ur.df(resid(obj), lags = 6, type = "none", selectlags = "AIC"))@teststat %>% print
}

#BDS test for structural breaks
library(strucchange)
library(sarbcurrent)
library(tidyverse)
library(lubridate)

#Optimal number of breakpoints - zmienne
for (x in names(data)) {
  obj <-(log(data[[x]]$oilprice)~log(lag(data[[x]]$oilprice)))
  obj_1 <-(log(data[[x]]$ree)~log(lag(data[[x]]$ree)))
  cat(str_to_upper(x))
  print(paste(x, sep=" "))
  print(summary(breakpoints(obj))$RSS[2,])
  print(paste("Dla kraju ", x,"i zmiennej oil price, najlepsza liczba zmian strukturalnych to: ",
              names(which(summary(breakpoints(obj))$RSS[2,]==min(summary(breakpoints(obj))$RSS[2,]))), sep=" "))
  if(as.numeric( names(which(summary(breakpoints(obj))$RSS[2,]==min(summary(breakpoints(obj))$RSS[2,]))))>0){
    obserwacje<-as.numeric(na.omit(summary(breakpoints(obj))$breakpoints[as.numeric( names(which(summary(breakpoints(obj))$RSS[2,]==min(summary(breakpoints(obj))$RSS[2,])))),]))
    
    print(paste("Kwartały w ktorych wystapiły załamania strukturalne", Dane_do_pierwszego_modelu$Date[obserwacje], sep="  "))
  }
  print(summary(breakpoints(obj_1))$RSS[2,])
  print(paste("Dla kraju ", x,"i zmiennej ree, najlepsza liczba zmian strukturalnych to: ",
              names(which(summary(breakpoints(obj_1))$RSS[2,]==min(summary(breakpoints(obj_1))$RSS[2,])))), sep=" ")
  
  if(as.numeric( names(which(summary(breakpoints(obj_1))$RSS[2,]==min(summary(breakpoints(obj_1))$RSS[2,]))))>0){
    obserwacje<-as.numeric(na.omit(summary(breakpoints(obj_1))$breakpoints[as.numeric( names(which(summary(breakpoints(obj_1))$RSS[2,]==min(summary(breakpoints(obj_1))$RSS[2,])))),]))
    print(paste("Kwartały w ktorych wystapiły załamania strukturalne", Dane_do_pierwszego_modelu$Date[obserwacje], sep="  "))
  }
  
  
}

#Optimal number of breakpoints - zmwroty
for (x in names(data)) {
  obj <-(log(diff(data[[x]]$oilprice))~log(diff(data[[x]]$oilprice)))
  obj_1 <-(log(diff(data[[x]]$ree))~log(diff(data[[x]]$ree)))
  cat(str_to_upper(x))
  print(paste(x, sep=" "))
  print(summary(breakpoints(obj))$RSS[2,])
  print(paste("Dla kraju ", x,"i zmiennej oil price, najlepsza liczba zmian strukturalnych to: ",
              names(which(summary(breakpoints(obj))$RSS[2,]==min(summary(breakpoints(obj))$RSS[2,]))), sep=" "))
  if(as.numeric( names(which(summary(breakpoints(obj))$RSS[2,]==min(summary(breakpoints(obj))$RSS[2,]))))>0){
    obserwacje<-as.numeric(na.omit(summary(breakpoints(obj))$breakpoints[as.numeric( names(which(summary(breakpoints(obj))$RSS[2,]==min(summary(breakpoints(obj))$RSS[2,])))),]))
    
    print(paste("Kwartały w ktorych wystapiły załamania strukturalne", Dane_do_pierwszego_modelu$Date[obserwacje], sep="  "))
  }
  print(summary(breakpoints(obj_1))$RSS[2,])
  print(paste("Dla kraju ", x,"i zmiennej ree, najlepsza liczba zmian strukturalnych to: ",
              names(which(summary(breakpoints(obj_1))$RSS[2,]==min(summary(breakpoints(obj_1))$RSS[2,])))), sep=" ")
  
  if(as.numeric( names(which(summary(breakpoints(obj_1))$RSS[2,]==min(summary(breakpoints(obj_1))$RSS[2,]))))>0){
    obserwacje<-as.numeric(na.omit(summary(breakpoints(obj_1))$breakpoints[as.numeric( names(which(summary(breakpoints(obj_1))$RSS[2,]==min(summary(breakpoints(obj_1))$RSS[2,])))),]))
    print(paste("Kwartały w ktorych wystapiły załamania strukturalne", Dane_do_pierwszego_modelu$Date[obserwacje], sep="  "))
  }
  
  
}

#Granger linear non-causlatiy test
wybor_opoznienia_z_modelu_var = function(df){
  results = matrix(NA, 1,6)
  colnames(results)<- names(df)
  
  for(i in names(df)){
    results[i] = VARselect(na.omit(data[[i]][,3:4]), lag.max = 8)$selection[1]
  }
  results = results[-c(1:6)]
  return(results)
}
wektor_opoznien<-wybor_opoznienia_z_modelu_var(data)

granger_tescik = function(df){
  granger_wyniki = matrix(NA, 2,6)
  colnames(granger_wyniki)<- names(df)
  rownames(granger_wyniki)<-c("ree ~ oilprice", "oil price ~ ree")
  
  for(i in names(df)){
    granger_wyniki[1,i] = grangertest(diff(ree_r)~diff(oilprice_r), order = wektor_opoznien[i], data = na.omit(df[[i]]))$`Pr(>F)`[2]
    granger_wyniki[2,i] = grangertest(diff(oilprice_r)~diff(ree_r), order = wektor_opoznien[i], data = na.omit(df[[i]]))$`Pr(>F)`[2]
  }
  return(granger_wyniki)
}
granger_tescik(data)

#Test BTS sprawdzający, czy dany ts jest i.i.d
BTS = function(df){
  df = na.omit(df)
  row.names(df)<-NULL
  Wyniki <- matrix(data=NA, nrow = 2, ncol = 8)
  colnames(Wyniki)<- c("m=2", "m=3", "m=4", "m=5 ", "m=2", "m=3", "m=4", "m=5")
  oil_p<-names(df)[3]
  ree<-names(df)[4]
  rownames(Wyniki)<-c(oil_p, ree)
  
  for(i in names(df)[3:4]){
    Wyniki[i,1] = bds.test(df[,i], m=5)$p.value[1]
    Wyniki[i,2] = bds.test(df[,i], m=5)$p.value[2]
    Wyniki[i,3] = bds.test(df[,i], m=5)$p.value[3]
    Wyniki[i,4] = bds.test(df[,i], m=5)$p.value[4]
    Wyniki[i,5] = bds.test(residuals(VAR(df[,3:4]), lag.max = 8)[,i], m=5)$p.value[1]
    Wyniki[i,6] = bds.test(residuals(VAR(df[,3:4]), lag.max = 8)[,i], m=5)$p.value[2]
    Wyniki[i,7] = bds.test(residuals(VAR(df[,3:4]), lag.max = 8)[,i], m=5)$p.value[3]
    Wyniki[i,8] = bds.test(residuals(VAR(df[,3:4]), lag.max = 8)[,i], m=5)$p.value[4]
    
  }
  
  return(Wyniki)
}

for (i in 1:6) {
  cat(str_to_upper(names(data)[i]))
  BTS(data[[i]]) %>% print
  cat('\n')
}


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
png(file="returns_plot.png", height = 700, width = 700)
par(mfrow = c(3,2))
par(mar=c(3, 4, 2.5, 2) + 0.1)
for (x in kraje) {
  plot(data[[x]]$date[-1],data[[x]]$oilprice_r[-1]*100, type = "l",col="green",xlab = "", ylab = "")
  lines(data[[x]]$date[-1],data[[x]]$ree_r[-1]*100, type = "l",col="red")
  mtext('returns', side=2, line=2.5)
  mtext(kraje1[which(kraje==x)], side=3, line=0.5)
}
dev.off()
# 8 OLS and quantile regression models
library(quantreg)

df_ols_qr <- data.frame(Country=rep(kraje1, each=4)) %>%
  mutate(Variable = rep(c('Constant', 'Pot', 'D08', 'D20'), length(kraje))) %>%
  cbind(matrix(0, nrow = 24, ncol = 10))
colnames(df_ols_qr)[3:12] <- c('OLS', paste0('Q_0.',1:9))

get_coefs <- function(summary, significance=T) {
  coefs <- round(summary$coefficients[,1],3)
  if (significance==T) {
    for (x in c(0.1, 0.05, 0.1)) {
      coefs[summary$coefficients[,4]<x] <- paste0(coefs[summary$coefficients[,4]<x], '*') 
    } 
  }
  coefs
}
i=1
for (x in kraje) {
  kraj <- kraje1[i]
  ols <- lm(ree_r~oilprice_r+D08+D20,data[[x]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100)) %>% summary
  df_ols_qr$OLS[df_ols_qr$Country==kraj] <- get_coefs(ols, T)
  
  for (q in seq(0.1, 0.9, 0.1)) {
    q_reg <- rq(ree_r~oilprice_r+D08+D20, tau = q, data=data[[x]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100))
    q_reg <- summary(q_reg, se='boot')
    df_ols_qr[[paste0('Q_',q)]][df_ols_qr$Country==kraj] <- get_coefs(q_reg, T)
  }
  i <- i+1
}

#ploty
calculate_CI <- function(mean, sd, q=0.95) {
  ci <- qnorm((1-q)/2+q)
  return(c(mean,mean-sd*ci, mean+sd*ci))
}
plot_stats <- list()
i=1
for (x in kraje) {
  kraj <- kraje1[i]
  plot_stats[[x]] <- matrix(0, nrow = 9, ncol = 7)
  plot_stats[[x]][,1] <- 1:9/10
  ols <- lm(ree_r~oilprice_r+D08+D20,data[[x]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100)) %>% summary
  for (q in seq(0.1, 0.9, 0.1)) {
    q_reg <- rq(ree_r~oilprice_r+D08+D20, tau = q, data=data[[x]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100))
    q_reg <- summary(q_reg, se='boot')
    plot_stats[[x]][10*q,2:4] <- calculate_CI(ols$coefficients[2,1], ols$coefficients[2,2], 0.95)
    plot_stats[[x]][10*q,5:7] <- calculate_CI(q_reg$coefficients[2,1], q_reg$coefficients[2,2], 0.95)
  }
  plot_stats[[x]] <- as.data.frame(plot_stats[[x]])
  colnames(plot_stats[[x]]) <- c('tau', 'ols_coef', 'ols_down', 'ols_up', 'qr_coef', 'qr_down', 'qr_up')
}
plot_stats[[6]]  
dev.off()
png(file="8_estymacja.png", height = 700, width = 700)
par(mfrow = c(3,2))
par(mar=c(3, 4, 2.5, 2) + 0.1)

# plot
for (x in names(plot_stats)) {
  df <- plot_stats[[x]]
  max <- max(as.matrix(df[,-1]))
  min <- min(as.matrix(df[,-1]))
  
  plot(qr_coef ~ tau, data = df, type = 'n', ylim=c(min-0.01, max+(max-min)/3.5), ylab='coef', xlab='')
  # add fill
  legend('topleft', inset=0.01, c('Estymator regresji kwantylowej', 'Estymator MNK'), lty = c(1,1), col = c('black', 'blue'), box.lty=0, y.intersp = 0.85, cex = 0.95)
  polygon(c(rev(df$tau), df$tau), c(rev(df$qr_up), df$qr_down), col = 'grey90', border = NA)
  lines(qr_coef ~ tau, data = df, type = 'l', lwd=1)
  lines(qr_coef ~ tau, data = df, type = 'b', lwd=1, pch = 18)
  lines(ols_coef ~ tau, data = df, type = 'l', lwd=1, col='blue')
  lines(ols_down ~ tau, data = df, lty='dashed', col='blue')
  lines(ols_up ~ tau, data = df, lty='dashed', col='blue')
  mtext(kraje1[which(kraje==x)], side=3, line=0.5)
  mtext('tau', side=1, line=2, cex=0.8)
}
dev.off()

# 9 Quantile slope equality F test
df_qr_test <- data.frame(Country=rep(kraje1, each=3)) %>%
  mutate(Variable = rep(c('Pot', 'D08', 'D20'), length(kraje))) %>%
  cbind(matrix(0, nrow = 18, ncol = 10))
colnames(df_qr_test)[3:12] <- c(paste0('Q_0.',1:8,'=Q_0.',2:9),'Q_0.1=Q_0.5','Q_0.5=Q_0.9')
set.seed(1)

#kod radzący sobie z brakiem unikalnych rozwiązań
i<-1
for (x in kraje) {
  kraj <- kraje1[i]
  for (q in seq(0.1, 1, 0.1)) {
    obj <- NULL
    if (q<=0.8) { q1=q; q2=q+0.1 
    } else if (q==0.9) { q1=0.1; q2=0.5
    } else if (q==1)  { q1=0.5; q2=0.9 
    }
    q_reg <- rq(ree_r~oilprice_r+D08+D20, tau = c(q1,q2), data=data[[x]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100))
    try({
      obj <-anova(q_reg, test = "Wald", joint=F)
    }, silent = T)
    iter=0
    while(is.null(obj) & iter<200) {
      try({
        q_reg <- rq(ree_r~oilprice_r+D08+D20, tau = c(q1+runif(1, -0.02, 0.02),q2+runif(1, -0.035, 0.035)), data=data[[x]][-1,] %>% mutate(oilprice_r=oilprice_r*100, ree_r=ree_r*100))
        obj <-anova(q_reg, test = "Wald", joint=F)
      }, silent = T)
      iter <- iter+1
    }
    if (!is.null(obj)) {
      df_qr_test[df_qr_test$Country==kraj,10*q+2] <- round(obj$table$pvalue,4)
    } else {
      df_qr_test[df_qr_test$Country==kraj,10*q+2] <- '---'
    }
    obj <- NULL
  }
  i <- i+1
}

#10 Asymetric models
df_ols_qr_asm <- data.frame(Country=rep(kraje1, each=5)) %>%
  mutate(Variable = rep(c('Constant', 'Pot_p', 'Pot_m', 'D08', 'D20'), length(kraje))) %>%
  cbind(matrix(0, nrow = 30, ncol = 10))
colnames(df_ols_qr_asm)[3:12] <- c('OLS', paste0('Q_0.',1:9))
i=1
for (x in kraje) {
  kraj <- kraje1[i]
  ols <- lm(ree_r~oilprice_r_p+oilprice_r_n+D08+D20,data[[x]][-1,] %>% mutate(ree_r=ree_r*100)) %>% summary
  df_ols_qr_asm$OLS[df_ols_qr_asm$Country==kraj] <- get_coefs(ols, T)
  
  for (q in seq(0.1, 0.9, 0.1)) {
    q_reg <- rq(ree_r~oilprice_r_p+oilprice_r_n+D08+D20, tau = q, data=data[[x]][-1,] %>% mutate(ree_r=ree_r*100))
    q_reg <- summary(q_reg, se='boot')
    df_ols_qr_asm[[paste0('Q_',q)]][df_ols_qr_asm$Country==kraj] <- get_coefs(q_reg, T)
  }
  i <- i+1
}
#ploty dla modelu asymetrycznego
plot_stats_asm <- list()
i=1
for (x in kraje) {
  kraj <- kraje1[i]
  plot_stats_asm[[x]] <- matrix(0, nrow = 9, ncol = 13)
  plot_stats_asm[[x]][,1] <- 1:9/10
  ols <- lm(ree_r~oilprice_r_p+oilprice_r_n+D08+D20,data[[x]][-1,] %>% mutate(ree_r=ree_r*100)) %>% summary
  for (q in seq(0.1, 0.9, 0.1)) {
    q_reg <- rq(ree_r~oilprice_r_p+oilprice_r_n+D08+D20, tau = q, data=data[[x]][-1,] %>% mutate(ree_r=ree_r*100))
    q_reg <- summary(q_reg, se='boot')
    plot_stats_asm[[x]][10*q,2:4] <- calculate_CI(ols$coefficients[2,1], ols$coefficients[2,2], 0.95)
    plot_stats_asm[[x]][10*q,5:7] <- calculate_CI(q_reg$coefficients[2,1], q_reg$coefficients[2,2], 0.95)
    plot_stats_asm[[x]][10*q,8:10] <- calculate_CI(ols$coefficients[3,1], ols$coefficients[3,2], 0.95)
    plot_stats_asm[[x]][10*q,11:13] <- calculate_CI(q_reg$coefficients[3,1], q_reg$coefficients[3,2], 0.95)
    
  }
  plot_stats_asm[[x]] <- as.data.frame(plot_stats_asm[[x]])
  colnames(plot_stats_asm[[x]]) <- c('tau', 'ols_coef_p', 'ols_down_p', 'ols_up_p', 'qr_coef_p', 'qr_down_p', 'qr_up_p',
                                     'ols_coef_n', 'ols_down_n', 'ols_up_n', 'qr_coef_n', 'qr_down_n', 'qr_up_n')
}
plot_stats_asm[[6]]  

dev.off()
png(file="10_asymetryczny_p.png", height = 700, width = 700)
par(mfrow = c(3,2))
par(mar=c(3, 4, 2.5, 2) + 0.1)
# plot
for (x in names(plot_stats_asm)) {
  df <- plot_stats_asm[[x]]
  max <- max(as.matrix(df[,2:7]))
  min <- min(as.matrix(df[,2:7]))
  
  plot(qr_coef_p ~ tau, data = df, type = 'n', ylim=c(min-0.01, max+(max-min)/3.5), ylab='coef', xlab='')
  # add fill
  legend('topleft', inset=0.01, c('Estymator regresji kwantylowej', 'Estymator MNK'), lty = c(1,1), col = c('black', 'blue'), box.lty=0, y.intersp = 0.85, cex = 0.85)
  polygon(c(rev(df$tau), df$tau), c(rev(df$qr_up_p), df$qr_down_p), col = 'grey90', border = NA)
  lines(qr_coef_p ~ tau, data = df, type = 'l', lwd=1)
  lines(qr_coef_p ~ tau, data = df, type = 'b', lwd=1, pch = 18)
  lines(ols_coef_p ~ tau, data = df, type = 'l', lwd=1, col='blue')
  lines(ols_down_p ~ tau, data = df, lty='dashed', col='blue')
  lines(ols_up_p ~ tau, data = df, lty='dashed', col='blue')
  mtext(kraje1[which(kraje==x)], side=3, line=0.5)
  mtext('POS_RPO', side=3, line=0.3, cex = 0.5, at=0.2)
  mtext('tau', side=1, line=2, cex=0.8)
}
dev.off()

png(file="10_asymetryczny_n.png", height = 700, width = 700)
par(mfrow = c(3,2))
par(mar=c(3, 4, 2.5, 2) + 0.1)
for (x in names(plot_stats_asm)) {
  df <- plot_stats_asm[[x]]
  max <- max(as.matrix(df[,8:13]))
  min <- min(as.matrix(df[,8:13]))
  
  plot(qr_coef_n ~ tau, data = df, type = 'n', ylim=c(min-0.01, max+(max-min)/3.5), ylab='coef', xlab='')
  # add fill
  legend('topleft', inset=0.01, c('Estymator regresji kwantylowej', 'Estymator MNK'), lty = c(1,1), col = c('black', 'blue'), box.lty=0, y.intersp = 0.85, cex = 0.85)
  polygon(c(rev(df$tau), df$tau), c(rev(df$qr_up_n), df$qr_down_n), col = 'grey90', border = NA)
  lines(qr_coef_n ~ tau, data = df, type = 'l', lwd=1)
  lines(qr_coef_n ~ tau, data = df, type = 'b', lwd=1, pch = 18)
  lines(ols_coef_n ~ tau, data = df, type = 'l', lwd=1, col='blue')
  lines(ols_down_n ~ tau, data = df, lty='dashed', col='blue')
  lines(ols_up_n ~ tau, data = df, lty='dashed', col='blue')
  mtext(kraje1[which(kraje==x)], side=3, line=0.5)
  mtext('NEG_RPO', side=3, line=0.3, cex = 0.5, at=0.2)
  mtext('tau', side=1, line=2, cex=0.8)
}
dev.off()

#11 quanlite slope test
df_qr_test_asm <- data.frame(Country=rep(kraje1, each=4)) %>%
  mutate(Variable = rep(c('Pot_p','Pot_n', 'D08', 'D20'), length(kraje))) %>%
  cbind(matrix(0, nrow = 24, ncol = 10))
colnames(df_qr_test_asm)[3:12] <- c(paste0('Q_0.',1:8,'=Q_0.',2:9),'Q_0.1=Q_0.5','Q_0.5=Q_0.9')
set.seed(1)
i<-1
for (x in kraje) {
  kraj <- kraje1[i]
  for (q in seq(0.1, 1, 0.1)) {
    obj <- NULL
    if (q<=0.8) { q1=q; q2=q+0.1 
    } else if (q==0.9) { q1=0.1; q2=0.5
    } else if (q==1)  { q1=0.5; q2=0.9 
    }
    q_reg <- rq(ree_r~oilprice_r_p+oilprice_r_n+D08+D20, tau = c(q1,q2), data=data[[x]][-1,] %>% mutate(ree_r=ree_r*100))
    try({
      obj <-anova(q_reg, test = "Wald", joint=F)
    }, silent = T)
    iter=0
    while(is.null(obj) & iter<200) {
      try({
        q_reg <- rq(ree_r~oilprice_r_p+oilprice_r_n+D08+D20, tau = c(q1+runif(1, -0.04, 0.04),q2+runif(1, -0.035, 0.035)), data=data[[x]][-1,] %>% mutate(ree_r=ree_r*100))
        obj <-anova(q_reg, test = "Wald", joint=F)
      }, silent = T)
      iter <- iter+1
    }
    if (!is.null(obj)) {
      df_qr_test_asm[df_qr_test_asm$Country==kraj,10*q+2] <- round(obj$table$pvalue,4)
    } else {
      df_qr_test_asm[df_qr_test_asm$Country==kraj,10*q+2] <- '---'
    }
    obj <- NULL
  }
  i <- i+1
}


wb <- createWorkbook()
addWorksheet(wb, '7_summary_stats')
writeData(wb, '7_summary_stats', sum_stats, rowNames = T)
addWorksheet(wb, '8_Estimation')
writeData(wb, '8_Estimation', df_ols_qr)
addWorksheet(wb, '9_Slope_test')
writeData(wb, '9_Slope_test', df_qr_test)
addWorksheet(wb, '10_Asimetric_model')
writeData(wb, '10_Asimetric_model', df_ols_qr_asm)
addWorksheet(wb, '11_Asimetric_slope_test')
writeData(wb, '11_Asimetric_slope_test', df_qr_test_asm)
saveWorkbook(wb, 'analysis_7_11_final.xlsx', overwrite = T)

