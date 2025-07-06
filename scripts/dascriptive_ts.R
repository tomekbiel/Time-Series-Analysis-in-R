
if (sys.nframe() == 0) {
  # Inicjalizacja konfiguracji
  config <- setup_config()
  
  # Funkcja do ładowania danych z cache
  load_market_data_cached <- function() {
    if (file.exists(config$cache_file)) {
      cat("Wczytuję zapisane dane z cache...\n")
      return(readRDS(config$cache_file))
    } else {
      cat("Brak cache - przetwarzam dane...\n")
      data <- load_market_data(
        symbols = config$symbols,
        timeframes = config$timeframes,
        config = config
      )
      saveRDS(data, config$cache_file)
      return(data)
    }
  }
  
  # Wczytaj WSZYSTKIE dane
  tic("Wczytywanie WSZYSTKICH danych")
  market_data <- load_market_data_cached()
  toc()
  
  # DODAJEMY OBLICZENIE CHANGE (logarytmiczna stopa zwrotu między kolejnymi close)
  market_data <- lapply(market_data, function(dt) {
    returns <- c(0, diff(log(dt$close)))  # Pierwsza wartość 0
    dt[, Change := round(returns, 4)]
    return(dt)
  })
  
  # Przypisz dane do nazwanych obiektów w środowisku globalnym
  list2env(market_data, envir = .GlobalEnv)
  
  # --- KONFIGURACJA ANALIZY ---
  # Tutaj możesz zmieniać symbol i timeframe dla analizy
  selected_symbol <- "US.100"   # Można zmienić na np. "OIL.WTI"
  selected_timeframe <- "D1"    # Można zmienić na np. "H4"
  # ----------------------------
  
  # Generuj nazwę zbioru danych na podstawie wyboru
  data_name <- tolower(gsub("[^[:alnum:]]", "", paste0(selected_symbol, selected_timeframe)))
  
  # Sprawdź czy dane istnieją i przygotuj do analizy
  if (exists(data_name)) {
    dt <- get(data_name)
    analysis_data <- data.table(
      Date = dt$time,
      Close = dt$close,
      Change = dt$Change
    )
    
    # Dynamiczna nazwa dla wykresów
    asset_name <- paste(selected_symbol, selected_timeframe)
    
    # WYKRESY
    par(mfcol = c(2, 1), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
    plot(analysis_data$Date, analysis_data$Close, 
         type = "l", 
         main = paste("Close Price -", asset_name),
         xlab = "Date", ylab = "Price")
    
    plot(analysis_data$Date, analysis_data$Change, 
         type = "l", 
         main = paste("Rate of return (close-to-close) -", asset_name),
         xlab = "Date", ylab = "Log Return")
    
    # PRZYGOTOWANIE DANYCH DO ANALIZY OPISOWEJ
    returns <- analysis_data$Change
    
    cat("\nAnaliza dla:", asset_name, "\n")
    cat("Okres od", format(min(analysis_data$Date)), "do", format(max(analysis_data$Date)), "\n")
    cat("Liczba obserwacji:", nrow(analysis_data), "\n\n")
        

        
        # Przykład:
        returns_s <- sort(returns)
        plot(returns_s, type = 'l', main = paste("Cumulative distribution -", asset_name))
        hist(returns, breaks = 100, main = paste("Return Distribution -", asset_name))
        
        # ... (reszta kodu analitycznego z zamienioną zmienną)
        
  } else {
    warning("Brak danych dla: ", data_name)
  }
}


# --- DALSZA CZĘŚĆ ANALIZY OPISOWEJ ---
# Przygotowanie danych
returns <- analysis_data$Change
returns_s <- sort(returns)

# Podstawowe statystyki
cat("\nPodstawowe statystyki dla stóp zwrotu (", asset_name, "):\n")
print(summary(returns))
cat("Odchylenie standardowe:", sd(returns), "\n")

# Wykresy rozkładu
par(mfcol = c(2, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
plot(analysis_data$Date, analysis_data$Close, 
     type = "l", 
     main = paste("Cena zamknięcia -", asset_name),
     xlab = "Data", ylab = "Cena")

plot(analysis_data$Date, returns, 
     type = "l", 
     main = paste("Stopy zwrotu (close-to-close) -", asset_name),
     xlab = "Data", ylab = "Log Stopa Zwrotu")

plot(returns_s, type = 'l', 
     main = paste("Dystrybuanta empiryczna -", asset_name),
     xlab = "Kolejność", ylab = "Stopa zwrotu")

hist(returns, breaks = 100, 
     main = paste("Rozkład stóp zwrotu -", asset_name),
     xlab = "Stopa zwrotu", ylab = "Częstość")

# Analiza kwantyli
q <- as.numeric(quantile(returns, c(0.25, 0.5, 0.75, 0.90)))
par(mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
plot(ecdf(returns), main = paste("Dystrybuanta empiryczna -", asset_name))
arrows(q, c(0.25, 0.5, 0.75, 0.90), q, c(0, 0, 0, 0), length = 0.1, angle = 15,
       col = c("violetred3", "YellowGreen", "SteelBlue", "wheat4"))
arrows(q, c(0.25, 0.5, 0.75, 0.90), rep(min(returns)-0.1, 4), c(0.25, 0.5, 0.75, 0.9), angle = 0,
       col = c("violetred3", "YellowGreen", "SteelBlue", "wheat4"))
points(q, c(0.25, 0.5, 0.75, 0.9), bg = "white", pch = 21,
       col = c("violetred3", "YellowGreen", "SteelBlue", "wheat4"))
legend("right", paste(c("Q25 = ", "Q50 = ", "Q75 = ", "Q90 = "), round(q, 4)), 
       bty = "n", text.col = c("violetred3", "YellowGreen", "SteelBlue", "wheat4"))

# Asymetria (skośność)
cat("\nAsymetria rozkładu:\n")
S1 <- e1071::skewness(returns, type = 1)
S2 <- e1071::skewness(returns, type = 2)
S3 <- e1071::skewness(returns, type = 3)
cat("Skośność (typ 1):", round(S1, 4), "\n")
cat("Skośność (typ 2):", round(S2, 4), "\n")
cat("Skośność (typ 3):", round(S3, 4), "\n")

# Test skośności Agostino
cat("\nTest skośności Agostino:\n")
print(moments::agostino.test(returns))

# Kurtoza
cat("\nKurtoza rozkładu:\n")
K1 <- e1071::kurtosis(returns, type = 1)
K2 <- e1071::kurtosis(returns, type = 2)
K3 <- e1071::kurtosis(returns, type = 3)
cat("Kurtoza (typ 1):", round(K1, 4), "\n")
cat("Kurtoza (typ 2):", round(K2, 4), "\n")
cat("Kurtoza (typ 3):", round(K3, 4), "\n")

# Test kurtozy Anscombe
cat("\nTest kurtozy Anscombe:\n")
print(moments::anscombe.test(returns))

# --- DOPASOWYWANIE ROZKŁADÓW ---

# 1. Rozkład normalny
cat("\nDopasowanie rozkładu normalnego:\n")
m <- mean(returns)
sd_ret <- sd(returns)

# Funkcja wiarygodności
f_norm <- function(theta, returns) {
  sum(-dnorm(returns, mean = theta[1], sd = theta[2], log = TRUE))
}
fit_norm <- nlminb(c(m, sd_ret), f_norm, returns = returns, 
                   lower = c(-Inf, 0), upper = c(Inf, Inf))
cat("Parametry dopasowania (średnia, sd):", 
    round(fit_norm$par, 4), "\n")
cat("Wartość funkcji wiarygodności:", fit_norm$objective, "\n")

# Test Andersona-Darlinga
cat("\nTest Andersona-Darlinga dla rozkładu normalnego:\n")
norm_test <- ADGofTest::ad.test(returns, pnorm, m, sd_ret)
print(norm_test)

# Wykres porównawczy
par(mfcol = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)

# Oblicz maksymalną gęstość dla ustawienia ylim
max_dens <- max(density(returns)$y, dnorm(m, m, sd_ret))

plot(density(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Gęstość -', asset_name),
     ylim = c(0, max_dens))
curve(dnorm(x, m, sd_ret), 
      add = TRUE, 
      col = 'violetred3', 
      lwd = 3)
legend("topright", 
       bg = 'white', 
       bty = "n", 
       cex = 1, 
       lty = 1, 
       lwd = c(4, 3),
       c('empiryczny', 'teoretyczny'), 
       col = c('SteelBlue', 'violetred3'))

plot(ecdf(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Dystrybuanta -', asset_name))
curve(pnorm(x, m, sd_ret), 
      add = TRUE, 
      col = 'violetred3', 
      lwd = 3)
legend("topleft", 
       bg = 'white', 
       bty = "n", 
       lty = 1, 
       lwd = c(4, 3),
       c('empiryczny', 'teoretyczny'), 
       col = c('SteelBlue', 'violetred3'))

     # 2. Rozkład Cauchy'ego
     cat("\nDopasowanie rozkładu Cauchy'ego:\n")
     md_ret <- median(returns)
     I_ret <- IQR(returns)/2
     
     f_cauchy <- function(theta, returns) {
       sum(-dcauchy(returns, location = theta[1], scale = theta[2], log = TRUE))
     }
     fit_cauchy <- nlminb(c(md_ret, I_ret), f_cauchy, returns = returns, 
                          lower = c(-Inf, 0), upper = c(Inf, Inf))
     cat("Parametry dopasowania (lokalizacja, skala):", 
         round(fit_cauchy$par, 4), "\n")
     
     # Test Andersona-Darlinga
     cat("\nTest Andersona-Darlinga dla rozkładu Cauchy'ego:\n")
     cauchy_test <- ADGofTest::ad.test(returns, pcauchy, fit_cauchy$par[1], fit_cauchy$par[2])
     print(cauchy_test)
     
     # Wykres porównawczy
     # Poprawiony wykres gęstości
     par(mfcol = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
     
     # Oblicz maksymalną gęstość
     max_dens_cauchy <- max(density(returns)$y, dcauchy(fit_cauchy$par[1], fit_cauchy$par[1], fit_cauchy$par[2]), na.rm = TRUE)
     
     plot(density(returns), 
          col = 'SteelBlue', 
          lwd = 4, 
          main = paste('Gęstość -', asset_name),
          ylim = c(0, max_dens_cauchy))
     curve(dcauchy(x, fit_cauchy$par[1], fit_cauchy$par[2]), 
           add = TRUE, 
           col = 'violetred3', 
           lwd = 3)
     legend("topright", 
            bg = 'white', 
            bty = "n", 
            cex = 1, 
            lty = 1, 
            lwd = c(4, 3),
            c('empiryczny', 'teoretyczny'), 
            col = c('SteelBlue', 'violetred3'))
     
     
     # 3. Rozkład Laplace'a
     cat("\nDopasowanie rozkładu Laplace'a:\n")
     ps_ret <- mean(abs(returns - md_ret))
     
     f_laplace <- function(theta, returns) {
       sum(-VGAM::dlaplace(returns, location = theta[1], scale = theta[2], log = TRUE))
     }
     fit_laplace <- nlminb(c(md_ret, ps_ret), f_laplace, returns = returns, 
                           lower = c(-Inf, 0), upper = c(Inf, Inf))
     
     # Dopasowanie alternatywną metodą
     fit_laplace_vglm <- VGAM::vglm(returns ~ 1, VGAM::laplace, 
                                    data.frame(returns), trace = FALSE, crit = "l")
     
     # Test Andersona-Darlinga
     cat("\nTest Andersona-Darlinga dla rozkładu Laplace'a:\n")
     laplace_test <- ADGofTest::ad.test(returns, VGAM::plaplace, 
                                        fit_laplace$par[1], fit_laplace$par[2])
     
     # POPRAWIONE WYKRESY - DWA W JEDNYM OKNIE
     par(mfrow = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
     
     # Oblicz maksymalną gęstość dla ustawienia ylim
     max_dens_laplace <- max(
       density(returns)$y, 
       VGAM::dlaplace(md_ret, fit_laplace$par[1], fit_laplace$par[2]), 
       na.rm = TRUE
     )
     
     # Wykres gęstości
     plot(density(returns), 
          col = 'SteelBlue', 
          lwd = 4, 
          main = paste('Gęstość -', asset_name),
          ylim = c(0, max_dens_laplace))
     curve(VGAM::dlaplace(x, fit_laplace$par[1], fit_laplace$par[2]), 
           add = TRUE, 
           col = 'violetred3', 
           lwd = 3)
     legend("topright", 
            bg = 'white', 
            bty = "n", 
            cex = 1, 
            lty = 1, 
            lwd = c(4, 3),
            c('empiryczny', 'teoretyczny'), 
            col = c('SteelBlue', 'violetred3'))
     
     # Wykres dystrybuanty
     plot(ecdf(returns), 
          col = 'SteelBlue', 
          lwd = 4, 
          main = paste('Dystrybuanta -', asset_name))
     curve(VGAM::plaplace(x, fit_laplace$par[1], fit_laplace$par[2]), 
           add = TRUE, 
           col = 'violetred3', 
           lwd = 3)
     legend("topleft", 
            bg = 'white', 
            bty = "n", 
            lty = 1, 
            lwd = c(4, 3),
            c('empiryczny', 'teoretyczny'), 
            col = c('SteelBlue', 'violetred3'))
     

     
     # 4. Rozkład stabilny
     cat("\nDopasowanie rozkładu stabilnego:\n")
     
     stable_fit <- fBasics::stableFit(returns, type = "mle", doplot = FALSE)
     # Pobierz parametry dopasowania
     stable_params <- stable_fit@fit$estimate
     cat("Parametry rozkładu stabilnego (alpha, beta, gamma, delta):\n")
     print(stable_params)
     
     # Test Andersona-Darlinga
     cat("\nTest Andersona-Darlinga dla rozkładu stabilnego:\n")
     stable_test <- ADGofTest::ad.test(returns, stabledist::pstable, 
                                       stable_params[1], stable_params[2],
                                       stable_params[3], stable_params[4])
     print(stable_test)
     
     # POPRAWIONE WYKRESY - DWA W JEDNYM OKNIE
     par(mfrow = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
     
     # Oblicz maksymalną gęstość dla ustawienia ylim
     max_dens_stable <- max(
       density(returns)$y, 
       stabledist::dstable(median(returns), 
                           stable_fit$estimate[1], stable_fit$estimate[2],
                           stable_fit$estimate[3], stable_fit$estimate[4]),
       na.rm = TRUE
     )
     
     # Wykres gęstości
     plot(density(returns), 
          col = 'SteelBlue', 
          lwd = 4, 
          main = paste('Gęstość -', asset_name),
          ylim = c(0, max_dens_stable),
          xlab = "Stopa zwrotu", ylab = "Gęstość")
     curve(stabledist::dstable(x, 
                               stable_fit$estimate[1], stable_fit$estimate[2],
                               stable_fit$estimate[3], stable_fit$estimate[4]),
           add = TRUE, 
           col = 'violetred3', 
           lwd = 3)
     legend("topright", 
            bg = 'white', 
            bty = "n", 
            cex = 1, 
            lty = 1, 
            lwd = c(4, 3),
            c('empiryczny', 'teoretyczny'), 
            col = c('SteelBlue', 'violetred3'))
     
     # Wykres dystrybuanty
     plot(ecdf(returns), 
          col = 'SteelBlue', 
          lwd = 4, 
          main = paste('Dystrybuanta -', asset_name),
          xlab = "Stopa zwrotu", ylab = "Prawdopodobieństwo")
     curve(stabledist::pstable(x, 
                               stable_fit$estimate[1], stable_fit$estimate[2],
                               stable_fit$estimate[3], stable_fit$estimate[4]),
           add = TRUE, 
           col = 'violetred3', 
           lwd = 3)
     legend("topleft", 
            bg = 'white', 
            bty = "n", 
            lty = 1, 
            lwd = c(4, 3),
            c('empiryczny', 'teoretyczny'), 
            col = c('SteelBlue', 'violetred3'))
     
     # Obliczenia prawdopodobieństw
     cat("\nPrawdopodobieństwa dla rozkładu stabilnego:\n")
     
     # Prawdopodobieństwo, że stopa zwrotu jest między -0.475% a 0.84%
     prob1 <- stabledist::pstable(0.0084, 
                                  stable_fit$estimate[1], stable_fit$estimate[2],
                                  stable_fit$estimate[3], stable_fit$estimate[4]) -
       stabledist::pstable(-0.00475, 
                           stable_fit$estimate[1], stable_fit$estimate[2],
                           stable_fit$estimate[3], stable_fit$estimate[4])
     cat("P( -0.475% < R < 0.84% ) =", round(prob1, 4), "\n")
     
     # Wykres prawdopodobieństwa
     par(mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
     curve(stabledist::dstable(x, 
                               stable_fit$estimate[1], stable_fit$estimate[2],
                               stable_fit$estimate[3], stable_fit$estimate[4]),
           -0.1, 0.1, 
           col = 'white', 
           main = paste('P( -0.475% < R < 0.84% ) -', asset_name),
           xlab = "Stopa zwrotu", ylab = "Gęstość")
     x <- seq(-0.00475, 0.0084, length = 300)
     y <- stabledist::dstable(x, 
                              stable_fit$estimate[1], stable_fit$estimate[2],
                              stable_fit$estimate[3], stable_fit$estimate[4])
     polygon(c(-0.00475, x, 0.0084), c(0, y, 0),
             col = 'Violet', border = "Dark Magenta")
     curve(stabledist::dstable(x, 
                               stable_fit$estimate[1], stable_fit$estimate[2],
                               stable_fit$estimate[3], stable_fit$estimate[4]),
           -0.1, 0.1, 
           lwd = 4, 
           col = 'Dark Magenta', 
           add = TRUE)
     
     # Prawdopodobieństwo, że stopa zwrotu jest między 0% a 5%
     prob2 <- stabledist::pstable(0.05, 
                                  stable_fit$estimate[1], stable_fit$estimate[2],
                                  stable_fit$estimate[3], stable_fit$estimate[4]) -
       stabledist::pstable(0.00, 
                           stable_fit$estimate[1], stable_fit$estimate[2],
                           stable_fit$estimate[3], stable_fit$estimate[4])
     cat("P( 0% < R < 5% ) =", round(prob2, 4), "\n")
     
     # Wykres prawdopodobieństwa
     par(mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
     curve(stabledist::dstable(x, 
                               stable_fit$estimate[1], stable_fit$estimate[2],
                               stable_fit$estimate[3], stable_fit$estimate[4]),
           -0.1, 0.1, 
           col = 'white', 
           main = paste('P( 0% < R < 5% ) -', asset_name),
           xlab = "Stopa zwrotu", ylab = "Gęstość")
     x <- seq(0.00, 0.05, length = 300)
     y <- stabledist::dstable(x, 
                              stable_fit$estimate[1], stable_fit$estimate[2],
                              stable_fit$estimate[3], stable_fit$estimate[4])
     polygon(c(0.00, x, 0.05), c(0, y, 0),
             col = 'Violet', border = "Dark Magenta")
     curve(stabledist::dstable(x, 
                               stable_fit$estimate[1], stable_fit$estimate[2],
                               stable_fit$estimate[3], stable_fit$estimate[4]),
           -0.1, 0.1, 
           lwd = 4, 
           col = 'Dark Magenta', 
           add = TRUE)