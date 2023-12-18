

library(tseries)
library(forecast)
library(lmtest)
library(lubridate)
library(TSA)

#################################MEMANGGIL DATA CSV###########################

setwd("D:/KULIAH RAGIL/SEMESTER 5/Analisis Deret Waktu/final project")
data<-read.csv("curah_hujan_bandung.csv", sep=',',header=TRUE)
data.ts <- ts(data, start = decimal_date(ymd("2017-01-01")), frequency = 12)

#################################DATA YANG DIGUNAKAN UNTUK DI ANALISIS###########################

data.Curah_hujan=ts(data$Curah_hujan, start = decimal_date(ymd("2017-01-01")), frequency = 12)


#################################PLOTING DATA ASLI##################################################

#memploting data curah hujan
plot.ts(data.Curah_hujan)

#################################UJI STASIONER DATA ASLI############################################

#uji ADF untuk menguji kestasioneran
adf.test(data.Curah_hujan) #stasioner
acf0 =acf(data.Curah_hujan, lag.max=15)  
pacf0= pacf(data.Curah_hujan, lag.max=15)  

#menentukan orde p,0,q
eacf(data.Curah_hujan)
#diperoleh model non musiman ARIMA(0,0,1)

########################################## PEMBENTUKAN MUSIMAN DARI DATA ASLI ########################################
diff_mus12 <- diff(data.Curah_hujan,lag=12)
plot.ts(diff_mus12)
par(mfrow=c(1,2))
acf(diff_mus12, lag.max=24)
pacf(diff_mus12, lag.max=24)

#menentukan orde P,0,Q
eacf(diff_mus12)

adf.test(diff_mus12)#stasioner
#jadi tidak perlu di di differencing
##jadi kesimpulan musiman adalah modelnya SARIMA(0,0,1)12
##kesimpulan keseluruhan adalah ARIMA(0,0,1)x(0,0,1)12 

########################################### METODE GRID SEARCH ########################################
# Tentukan rentang nilai parameter
p_values <- c(0, 1, 2,3,4,5)
d_values <- c(0, 1)
q_values <- c(0, 1, 2,3,4,5)
P_values <- c(0, 1, 2,3,4,5)
D_values <- c(0, 1)
Q_values <- c(0, 1, 2,3,4,5)

# Buat grid kombinasi parameter
parameter_grid <- expand.grid(p = p_values, d = d_values, q = q_values,
                              P = P_values, D = D_values, Q = Q_values)

# Inisialisasi variabel untuk menyimpan hasil evaluasi
best_aic <- Inf
best_model <- NULL

# Iterasi melalui kombinasi parameter
for (i in 1:nrow(parameter_grid)) {
  current_param <- parameter_grid[i, ]
  
  # Membangun model SARIMA
  current_model <- Arima(data.Curah_hujan, order = c(current_param$p, current_param$d, current_param$q),
                         seasonal = list(order = c(current_param$P, current_param$D, current_param$Q)))
  
  # Evaluasi model menggunakan AIC
  current_aic <- AIC(current_model)
  
  # Membandingkan dengan model terbaik sejauh ini
  if (current_aic < best_aic) {
    best_aic <- current_aic
    best_model <- current_model
  }
}

# Menampilkan model terbaik
print(best_model)

########################################### ESTIMASI PARAMETER DAN PEMILIHAN MODEL TERBAIK ########################################

data.Curah_hujan1 <- arima(data.Curah_hujan, order=c(0,0,1), seasonal = list(order = c(0,0,1), period = 12))
data.Curah_hujan1

data.Curah_hujan2 <- arima(data.Curah_hujan, order=c(0,0,1), seasonal = list(order = c(1,0,1), period = 12))
data.Curah_hujan2

data.Curah_hujan3 <- arima(data.Curah_hujan, order=c(0,0,1), seasonal = list(order = c(2,0,1), period = 12))
data.Curah_hujan3

data.Curah_hujan4 <- arima(data.Curah_hujan, order=c(1,0,1), seasonal = list(order = c(0,0,1), period = 12))
data.Curah_hujan4

data.Curah_hujan5 <- arima(data.Curah_hujan, order=c(1,0,1), seasonal = list(order = c(1,0,1), period = 12))
data.Curah_hujan5

data.Curah_hujan6 <- arima(data.Curah_hujan, order=c(1,0,1), seasonal = list(order = c(2,0,1), period = 12))
data.Curah_hujan6

data.Curah_hujan7<- arima(data.Curah_hujan, order=c(2,0,1), seasonal = list(order = c(0,0,1), period = 12))
data.Curah_hujan7

data.Curah_hujan8 <- arima(data.Curah_hujan, order=c(2,0,1), seasonal = list(order = c(1,0,1), period = 12))
data.Curah_hujan8

data.Curah_hujan9<- arima(data.Curah_hujan, order=c(2,0,1), seasonal = list(order = c(2,0,1), period = 12))
data.Curah_hujan9

data.Curah_hujan10<- arima(data.Curah_hujan, order=c(4,1,4), seasonal = list(order = c(1,0,0),period = 12))
data.Curah_hujan10


######################################################DIAGNOSTIK RESIDUAL#######################################
plot(rstandard(data.Curah_hujan5))

#KENORMALAN RESIDUAL
qqnorm(residuals(data.Curah_hujan5))
qqline(residuals(data.Curah_hujan5))

shapiro.test(data.Curah_hujan5$residuals)

#Uji Kesignifikanan parameter: Uji t
coeftest(data.Curah_hujan5)

#AUTOKORELASI RESIDUAL
residuals <- residuals(data.Curah_hujan5)
ljung_box_test <- Box.test(residuals, type = "Ljung-Box")
print(ljung_box_test)

######################################Forecasting untuk 12 bulan kedepan####################################
forecasting <- forecast(data.Curah_hujan, model=data.Curah_hujan5, h=12)
plot(forecasting, main="Plot Hasil Peramalan")


od <- options(digits = 3) 
fit <- arima(data.Curah_hujan, order=c(1,0,1), seasonal = list(order = c(1,0,1), period=12))
predict(fit, n.ahead = 12)
options(od)
