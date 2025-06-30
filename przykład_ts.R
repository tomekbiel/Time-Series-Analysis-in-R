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


# W przykładzie użycia na końcu skryptu:

# Konwersja do ts dla danych minutowych
if(exists("us100m1")) {
  us100m1_ts <- convert_to_ts(us100m1)
  cat("\nSzereg czasowy dla us100m1 (pierwsze 5 wartości):\n")
  print(head(us100m1_ts, 5))
  
  # Analiza ARIMA
  cat("\nAnaliza ARIMA dla danych minutowych:\n")
  library(forecast)
  
  # Stacjonaryzacja danych (zwroty logarytmiczne)
  returns <- diff(log(us100m1_ts))
  returns <- na.omit(returns)
  
  # Automatyczne dopasowanie modelu ARIMA
  fit <- auto.arima(returns, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  
  # Podsumowanie modelu
  cat("\nPodsumowanie modelu ARIMA:\n")
  print(summary(fit))
  
  # Prognoza na następne 30 minut
  fcast <- forecast(fit, h = 30)
  
  # Wizualizacja
  plot(fcast, main = "Prognoza ARIMA na 30 minut dla US.100 M1")
  abline(h = 0, col = "gray", lty = 2)
}