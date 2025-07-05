cat("\nDopasowanie rozkładu stabilnego (alphastable):\n")

# Ładowanie pakietów
library(fBasics)
library(alphastable)
library(stabledist)
library(ADGofTest)

# Szacowanie parametrów startowych metodą kwantylową (szybsza niż MLE)
start_fit <- stableFit(returns, type = "qn", doplot = FALSE)
start_params <- start_fit@fit$estimate

# Przypisanie nazwanych parametrów w konwencji alphastable
alpha0 <- start_params["alpha"]
beta0  <- start_params["beta"]
sigma0 <- start_params["gamma"]  # gamma = sigma
mu0    <- start_params["delta"]  # delta = mu

cat("Parametry startowe z stableFit (przekształcone do alphastable):\n")
cat("alpha0 =", alpha0, "beta0 =", beta0, "sigma0 =", sigma0, "mu0 =", mu0, "\n")

# Dopasowanie rozkładu stabilnego przy użyciu alphastable
fit_alpha <- ufitstab.skew(returns,
                           alpha0 = alpha0,
                           beta0 = beta0,
                           sigma0 = sigma0,
                           mu0 = mu0,
                           param  = 1)

# Parametry dopasowania
stable_params <- fit_alpha$estimate
names(stable_params) <- c("alpha", "beta", "gamma", "delta")

cat("\nParametry dopasowania (alphastable):\n")
print(stable_params)

# Test Andersona-Darlinga dla dopasowania rozkładu stabilnego
cat("\nTest Andersona-Darlinga dla dopasowania alphastable:\n")
ad_test_result <- ad.test(returns, stabledist::pstable,
                          stable_params[1], stable_params[2],
                          stable_params[3], stable_params[4])
print(ad_test_result)

# WYKRESY
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)

# Gęstość
max_dens_stable <- max(
  density(returns)$y,
  dstable(median(returns),
          stable_params[1], stable_params[2],
          stable_params[3], stable_params[4]),
  na.rm = TRUE
)

plot(density(returns),
     col = 'SteelBlue',
     lwd = 4,
     main = paste('Gęstość -', asset_name),
     ylim = c(0, max_dens_stable),
     xlab = "Stopa zwrotu", ylab = "Gęstość")
curve(dstable(x,
              stable_params[1], stable_params[2],
              stable_params[3], stable_params[4]),
      add = TRUE,
      col = 'violetred3',
      lwd = 3)
legend("topright",
       bg = 'white', bty = "n",
       cex = 1, lty = 1,
       lwd = c(4, 3),
       c('empiryczny', 'teoretyczny'),
       col = c('SteelBlue', 'violetred3'))

# Dystrybuanta
plot(ecdf(returns),
     col = 'SteelBlue',
     lwd = 4,
     main = paste('Dystrybuanta -', asset_name),
     xlab = "Stopa zwrotu", ylab = "Prawdopodobieństwo")
curve(pstable(x,
              stable_params[1], stable_params[2],
              stable_params[3], stable_params[4]),
      add = TRUE,
      col = 'violetred3',
      lwd = 3)
legend("topleft",
       bg = 'white', bty = "n",
       lty = 1,
       lwd = c(4, 3),
       c('empiryczny', 'teoretyczny'),
       col = c('SteelBlue', 'violetred3'))

# Prawdopodobieństwa i wykresy
cat("\nPrawdopodobieństwa dla rozkładu stabilnego:\n")

## 1. P(-0.475% < R < 0.84%)
prob1 <- pstable(0.0084, stable_params[1], stable_params[2],
                 stable_params[3], stable_params[4]) -
  pstable(-0.00475, stable_params[1], stable_params[2],
          stable_params[3], stable_params[4])
cat("P( -0.475% < R < 0.84% ) =", round(prob1, 4), "\n")

# Wykres dla przedziału (-0.475%, 0.84%)
par(mar = c(4, 4, 1, 1) + 0.1)
curve(dstable(x, stable_params[1], stable_params[2],
              stable_params[3], stable_params[4]),
      -0.1, 0.1,
      col = 'white',
      main = paste('P( -0.475% < R < 0.84% ) -', asset_name),
      xlab = "Stopa zwrotu", ylab = "Gęstość")
x <- seq(-0.00475, 0.0084, length = 300)
y <- dstable(x, stable_params[1], stable_params[2],
             stable_params[3], stable_params[4])
polygon(c(-0.00475, x, 0.0084), c(0, y, 0),
        col = 'Violet', border = "Dark Magenta")
curve(dstable(x, stable_params[1], stable_params[2],
              stable_params[3], stable_params[4]),
      -0.1, 0.1,
      add = TRUE, col = 'Dark Magenta', lwd = 4)

## 2. P(0% < R < 5%)
prob2 <- pstable(0.05, stable_params[1], stable_params[2],
                 stable_params[3], stable_params[4]) -
  pstable(0.00, stable_params[1], stable_params[2],
          stable_params[3], stable_params[4])
cat("P( 0% < R < 5% ) =", round(prob2, 4), "\n")

# Wykres dla przedziału (0%, 5%)
par(mar = c(4, 4, 1, 1) + 0.1)
curve(dstable(x, stable_params[1], stable_params[2],
              stable_params[3], stable_params[4]),
      -0.1, 0.1,
      col = 'white',
      main = paste('P( 0% < R < 5% ) -', asset_name),
      xlab = "Stopa zwrotu", ylab = "Gęstość")
x <- seq(0.00, 0.05, length = 300)
y <- dstable(x, stable_params[1], stable_params[2],
             stable_params[3], stable_params[4])
polygon(c(0.00, x, 0.05), c(0, y, 0),
        col = 'Violet', border = "Dark Magenta")
curve(dstable(x, stable_params[1], stable_params[2],
              stable_params[3], stable_params[4]),
      -0.1, 0.1,
      add = TRUE, col = 'Dark Magenta', lwd = 4)
