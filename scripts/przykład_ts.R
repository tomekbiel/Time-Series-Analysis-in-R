# Po wczytaniu danych
us100_ts <- convert_to_ts(us100d1)

# Analiza podstawowa
plot(us100_ts, main = "US.100 D1")
acf(us100_ts, main = "Autokorelacja")

# Prognozowanie
library(forecast)
fit <- auto.arima(us100_ts)
fcast <- forecast(fit, h = 1)
plot(fcast, main = "30-dniowa prognoza US.100")
print(fcast)

convert_to_ts_minute <- function(dt) {
  if (nrow(dt) == 0) return(NULL)
  
  # Ustaw stałą częstotliwość dla danych minutowych
  freq <- 1440  # 60 minut * 24 godziny = 1440 (sezonowość dzienna)
  
  # Oblicz początek szeregu w formacie c(rok, dzień)
  start_time <- min(dt$time)
  start_decimal <- c(
    lubridate::hour(start_time) * 60 + lubridate::minute(start_time) + 1,
    1
  )
  
  ts(dt$close, frequency = freq, start = start_decimal)
}
# W przykładzie użycia na końcu skryptu:

if(exists("us100m1")) {
  # 1. Próbkowanie danych do wyższego interwału (np. 5-minutowe)
  us100m5 <- us100m1[, .(time = time[seq(1, .N, by = 5)], 
                         close = close[seq(1, .N, by = 5)])]
  
  # 2. Konwersja do TS z poprawną sezonowością
  us100_ts <- convert_to_ts_minute(us100m5)
  
  # 3. Użyj różnicowania sezonowego
  returns <- diff(log(us100_ts), lag = 1440)  # Różnicowanie dzienne
  
  # 4. Ogranicz zakres testowanych modeli
  fit <- auto.arima(
    returns,
    seasonal = TRUE,
    stepwise = TRUE,       # Przyspiesza obliczenia
    approximation = TRUE,  # Użyj przybliżeń
    max.p = 5,
    max.q = 5,
    max.P = 2,
    max.Q = 2,
    max.d = 2,
    max.D = 1
  )
  
  # 5. Prognoza tylko na najbliższe 10 interwałów (50 minut)
  fcast <- forecast(fit, h = 10)
  
  # 6. Efektywna wizualizacja
  plot(fcast, main = "Prognoza ARIMA dla US.100 (5-minutowe)")
}


# Model TBATS obsługujący wielokrotną sezonowość
fit_tbats <- tbats(log(us100_ts), 
                   use.arma.errors = TRUE,
                   use.parallel = TRUE)

# Prognoza
fcast_tbats <- forecast(fit_tbats, h = 12)  # 1 godzina

# Wizualizacja
plot(fcast_tbats, main = "Prognoza TBATS dla US.100 (5-minutowe)")