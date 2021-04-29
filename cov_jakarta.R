library(httr)
#tes
resp <- GET ("https://data.covid19.go.id/public/api/update.json")
status_code (resp)
identical(resp$status_code, status_code(resp))
resp$status_code

headers(resp)
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE) 
cov_id_raw_data = cov_id_raw$data

#get jakarta covid data from API
resp_jakarta = GET("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json")
cov_jakarta_raw <- content(resp_jakarta, as = "parsed", simplifyVector = TRUE)
names(cov_jakarta_raw)

cov_jakarta = cov_jakarta_raw$list_perkembangan
str(cov_jakarta)

#data cleaning
library(dplyr)
new_cov_jakarta <-
  cov_jakarta %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )

str(new_cov_jakarta)
head(new_cov_jakarta)
tail(new_cov_jakarta)
write.csv(new_cov_jakarta, "covid_jakarta_daily(1903).csv", row.names = F)

#### analisis ####
data = read.csv("covid_jakarta_daily(1903).csv")
attach(data)
date = seq(as.Date("2020-03-01"), as.Date("2021-03-14"), by = "day")
ts_covid = ts(kasus_baru, start = c(2020, as.numeric(format(date[1],"%j"))), frequency = 365)

library(forecast)
library(tseries)
library(nnfor)
library(TSstudio)
library(zoo)

#split data into train and test set
newcase_train = head(kasus_baru, 367)
newcase_test = tail(kasus_baru, length(kasus_baru)-367)
dead_train = head(meninggal, 367)
dead_test = tail(meninggal, length(meninggal)-367)
sembuh_train = head(sembuh, 367)
sembuh_test = tail(sembuh, length(sembuh)-367)
# pacf newcase 7, pacf meninggal 2, 5 atau 7, pacf sembuh 1, 2, atau 7

ts.kcv = function(Trainset, Testset, size = NULL, p = NULL) {
  hasil = list()
  for (j in 1:size) {
    ntrain = NROW(Trainset)
    ntest = NROW(Testset)
    frc = c()
    rmse = c()
    mae = c()
    for (i in 1:ntest) {
      if (i == 1) {
        fit <- nnetar(Trainset, size = j, p = p)
        frcast = forecast(fit, h = 1)
        frc[i] = frcast$mean
        rmse[i] = accuracy(frcast)[2]
        mae[i] = accuracy(frcast)[3]
      }
      if (i > 1) {
        Trainset[ntrain+i-1] = Testset[i-1]
        fit <- nnetar(Trainset, size = j, p = p)
        frcast = forecast(fit, h = 1)
        frc[i] = frcast$mean
        rmse[i] = accuracy(frcast)[2]
        mae[i] = accuracy(frcast)[3]
      }
    }
    e = frc - Testset
    rmse_train = mean(rmse)
    mae_train = mean(mae)
    mape_test = mean(abs(e/Testset))*100
    hasil[[j]] = list(frc=frc, e=e, rmse_train=rmse_train, mae_train=mae_train, mape_test=mape_test)
  }
  return(hasil)
}

# sebelum mulai rolling cv
# default nnetar buat meninggal NNAR(11,6), MSE 21.69, RMSE 4.6572, MAE 3.2304 (TRAINING)
# default nnetar buat sembuh NNAR(14,8), MSE 15620, RMSE 124.9789, MAE 79.72 (TRAINING)

rolling_p7 = ts.kcv(sembuh_train, sembuh_test, size = 7, p = 7)
rolling_p1 = ts.kcv(sembuh_train, sembuh_test, size = 7, p = 1)
rolling_p2 = ts.kcv(sembuh_train, sembuh_test, size = 7, p = 2)
rolling_p3 = ts.kcv(sembuh_train, sembuh_test, size = 7, p = 3)
rolling_p4 = ts.kcv(sembuh_train, sembuh_test, size = 7, p = 4)
rolling_p5 = ts.kcv(sembuh_train, sembuh_test, size = 7, p = 5)
rolling_p6 = ts.kcv(sembuh_train, sembuh_test, size = 7, p = 6)

rolling6 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 6)
rolling7 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 7)
rolling8 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 8)
rolling9 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 9)
rolling10 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 10)
rolling11 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 11)
rolling12 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 12)
rolling13 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 13)
rolling14 = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 14)

ts.kcv = function(Trainset, Testset, size = NULL, p = NULL) {
  hasil = list()
  for (j in 1:size) {
    ntrain = NROW(Trainset)
    ntest = NROW(Testset)
    frc = c()
    true_frc = c()
    mape = c()
    acc = c()
    for (i in 1:(ntest+7)) {
      if (i == 1) {
        fit <- nnetar(Trainset, size = j, p = p)
        frcast = forecast(fit, h = 1)
        frc[i] = frcast$mean
        mape[i] = accuracy(frcast)[5]
      }
      if (i > 1 & i <= ntest) {
        Trainset[ntrain+i-1] = Testset[i-1]
        fit <- nnetar(Trainset, size = j, p = p)
        frcast = forecast(fit, h = 1)
        frc[i] = frcast$mean
        mape[i] = accuracy(frcast)[5]
      }
      if (i == (ntest + 1)) {
        Trainset[ntrain+i-1] = Testset[i-1]
        fit <- nnetar(Trainset, size = j, p = p)
        frcast = forecast(fit, h = 1)
        true_frc[i-ntest] = frcast$mean
        acc[i-ntest] = accuracy(frcast)[5]
      }
      if (i > (ntest + 1)) {
        Trainset[ntrain+i-1] = true_frc[i-1-ntest]
        fit <- nnetar(Trainset, size = j, p = p)
        frcast = forecast(fit, h = 1)
        true_frc[i-ntest] = frcast$mean
        acc[i-ntest] = accuracy(frcast)[5]
      }
    }
    e = frc - Testset
    mape_train = mean(mape)
    acc_fitnn = mean(acc)
    mape_test = mean(abs(e/Testset))*100
    hasil[[j]] = list(frc=frc, e=e, mape_train=mape_train, mape_test=mape_test,
                      true_frc=true_frc, acc_fitnn=acc_fitnn)
  }
  return(hasil)
}

rolling7 = ts.kcv(Trainset, Testset, size = 14, p = 7)
rolling7[[14]]$acc_fitnn
rolling7[[14]]$true_frc
tail(Testset)
accuracy(fit)
fit$p
fit$model
plot(fit)
plot(frc)
autoplot(frc) + autolayer(fit$fitted)
?nnetar
fit = nnetar(ts_covid, p = 7, size = 14)
fit$fitted
accuracy(fit)
ts.kcv = function(Trainset, Testset, size = NULL, p = NULL) {
  hasil = list()
  for (j in 1:size) {
    ntrain = NROW(Trainset)
    ntest = NROW(Testset)
    frc = c()
    Point = c()
    rmse = c()
    mae = c()
    acc_rmse = c()
    acc_mae = c()
    for (i in 1:(ntest+7)) {
      if (i == 1) {
        fit <- nnetar(Trainset, size = j, p = p, P = 1)
        frcast = forecast(fit, h = 1)
        frc[i] = frcast$mean
        rmse[i] = accuracy(frcast)[2]
        mae[i] = accuracy(frcast)[3]
      }
      if (i > 1 & i <= ntest) {
        Trainset[ntrain+i-1] = Testset[i-1]
        fit <- nnetar(Trainset, size = j, p = p, P = 1)
        frcast = forecast(fit, h = 1)
        frc[i] = frcast$mean
        rmse[i] = accuracy(frcast)[2]
        mae[i] = accuracy(frcast)[3]
      }
      if (i == (ntest + 1)) {
        Trainset[ntrain+i-1] = Testset[i-1]
        fit <- nnetar(Trainset, size = j, p = p, P = 1)
        frcast = forecast(fit, h = 1)
        Point[i-ntest] = frcast$mean
        acc_rmse[i-ntest] = accuracy(frcast)[2]
        acc_mae[i-ntest] = accuracy(frcast)[3]
      }
      if (i > (ntest + 1)) {
        Trainset[ntrain+i-1] = Point[i-1-ntest]
        fit <- nnetar(Trainset, size = j, p = p, P = 1)
        frcast = forecast(fit, h = 1)
        Point[i-ntest] = frcast$mean
        acc_rmse[i-ntest] = accuracy(frcast)[2]
        acc_mae[i-ntest] = accuracy(frcast)[3]
      }
    }
    e = frc - Testset
    rmse_train = mean(rmse)
    mae_train = mean(mae)
    accfit_rmse = mean(acc_rmse)
    accfit_mae = mean(acc_mae)
    mape_test = mean(abs(e/Testset))*100
    hasil[[j]] = list(frc=frc, e=e, rmse_train=rmse_train, mae_train=mae_train, mape_test=mape_test,
                      true_frc=Point, accfit_rmse=accfit_rmse, accfit_mae=accfit_mae)
  }
  return(hasil)
}
fit_newcase = ts.kcv(newcase_train, newcase_test, size = 14, p = 7)
fit_newcase[[14]]$acc_fitnn
fit_newcase[[14]]$true_frc
tail(Testset)
fit_newcase[[14]]$mape_test

time.points1 <- seq.Date(as.Date("2020-03-01"), by = 1, length.out = 383)
time.points2 <- seq.Date(as.Date("2021-03-19"), by = 1, length.out = 7)
frc = zoo(fit_newcase[[14]]$true_frc, time.points2)

fit_died = ts.kcv(dead_train, dead_test, size = 14, p = 14)
fit_recovered = ts.kcv(sembuh_train, sembuh_test, size = 14, p = 14)
data = data.frame(tanggal = time.points2, newcase = fit_newcase[[14]]$true_frc, 
                  dead = fit_died[[14]]$true_frc, recovered = fit_recovered[[14]]$true_frc)
data[,-1] = round(data[,-1])
write.csv(data, "hasil.csv", row.names = F)


##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
#----------------------------- CASUAL IMPACT ---------------------------------#

suppressMessages(library(CausalImpact))
suppressMessages(library(dplyr))

df = read.csv("covid_jakarta_daily(1903).csv")

#Vaccination
pre.period <- as.Date(c('2020-03-01','2021-01-12'))
post.period <- as.Date(c('2021-01-13','2021-03-18'))

time.points <- as.Date(df$tanggal)
data <- zoo(df$sembuh, time.points)

ci <- CausalImpact(data = data,
                   pre.period = pre.period,
                   post.period = post.period)
summary(ci)
plot(ci)

#PPKM Jakarta
pre.period <- as.Date(c('2020-03-01','2021-02-08'))
post.period <- as.Date(c('2021-02-09','2021-03-18'))

time.points <- as.Date(df$tanggal)
data <- zoo(df$sembuh, time.points)

ci <- CausalImpact(data = data,
                   pre.period = pre.period,
                   post.period = post.period)
summary(ci)
plot(ci)
