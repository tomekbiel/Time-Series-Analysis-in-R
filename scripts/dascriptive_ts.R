
if (sys.nframe() == 0) {
  # Initialization of configuration
  config <- setup_config()
  
  # Function for loading market data with caching
  load_market_data_cached <- function() {
    if (file.exists(config$cache_file)) {
      cat("Loading cached market data...\n")
      return(readRDS(config$cache_file))
    } else {
      cat("Cache not found - processing data...\n")
      data <- load_market_data(
        symbols = config$symbols,
        timeframes = config$timeframes,
        config = config
      )
      saveRDS(data, config$cache_file)
      return(data)
    }
  }
  
  # Load ALL market data
  tic("Loading ALL market data")
  market_data <- load_market_data_cached()
  toc()
  
  # Compute logarithmic returns (log differences between consecutive close prices)
  market_data <- lapply(market_data, function(dt) {
    returns <- c(0, diff(log(dt$close)))  # First return is set to 0
    dt[, Change := round(returns, 4)]
    return(dt)
  })
  
  # Assign named data tables to the global environment
  list2env(market_data, envir = .GlobalEnv)
  
  # --- ANALYSIS CONFIGURATION ---
  # You can modify the symbol and timeframe here
  selected_symbol <- "US.100"   # Change to e.g., "OIL.WTI"
  selected_timeframe <- "D1"    # Change to e.g., "H4"
  # -------------------------------
  
  # Generate dataset name from the selected symbol and timeframe
  data_name <- tolower(gsub("[^[:alnum:]]", "", paste0(selected_symbol, selected_timeframe)))
  
  # Check if the selected dataset exists and prepare it for analysis
  if (exists(data_name)) {
    dt <- get(data_name)
    analysis_data <- data.table(
      Date = dt$time,
      Close = dt$close,
      Change = dt$Change
    )
    
    # Dynamic label for plots
    asset_name <- paste(selected_symbol, selected_timeframe)
    
    # PLOTS
    par(mfcol = c(2, 1), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
    plot(analysis_data$Date, analysis_data$Close, 
         type = "l", 
         main = paste("Close Price -", asset_name),
         xlab = "Date", ylab = "Price")
    
    plot(analysis_data$Date, analysis_data$Change, 
         type = "l", 
         main = paste("Rate of return (close-to-close) -", asset_name),
         xlab = "Date", ylab = "Log Return")
    
    # PREPARE DATA FOR DESCRIPTIVE STATISTICAL ANALYSIS
    returns <- analysis_data$Change
    
    cat("\nAnalysis for:", asset_name, "\n")
    cat("Time range:", format(min(analysis_data$Date)), "to", format(max(analysis_data$Date)), "\n")
    cat("Number of observations:", nrow(analysis_data), "\n\n")
    
    # Cumulative distribution and histogram
    returns_s <- sort(returns)
    plot(returns_s, type = 'l', main = paste("Cumulative distribution -", asset_name))
    hist(returns, breaks = 100, main = paste("Return Distribution -", asset_name))
    
  } else {
    warning("Data not found for: ", data_name)
  }
}

# --- CONTINUATION OF DESCRIPTIVE STATISTICAL ANALYSIS ---
returns <- analysis_data$Change
returns_s <- sort(returns)

# Basic descriptive statistics
cat("\nBasic statistics for returns (", asset_name, "):\n")
print(summary(returns))  # Min, 1st Qu., Median, Mean, 3rd Qu., Max
cat("Standard deviation:", sd(returns), "\n")  # Measure of volatility

# Distribution plots
par(mfcol = c(2, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)

# Time series of closing prices
plot(analysis_data$Date, analysis_data$Close, 
     type = "l", 
     main = paste("Closing Price -", asset_name),
     xlab = "Date", ylab = "Price")

# Time series of returns
plot(analysis_data$Date, returns, 
     type = "l", 
     main = paste("Log Returns (close-to-close) -", asset_name),
     xlab = "Date", ylab = "Log Return")

# Empirical cumulative distribution function (ECDF)
plot(returns_s, type = 'l', 
     main = paste("Empirical CDF -", asset_name),
     xlab = "Index", ylab = "Return")

# Histogram of returns
hist(returns, breaks = 100, 
     main = paste("Return Distribution -", asset_name),
     xlab = "Return", ylab = "Frequency")

# Quantile analysis (e.g., Q25, Q50, Q75, Q90)
q <- as.numeric(quantile(returns, c(0.25, 0.5, 0.75, 0.90)))
par(mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
plot(ecdf(returns), main = paste("Empirical CDF -", asset_name))
arrows(q, c(0.25, 0.5, 0.75, 0.90), q, c(0, 0, 0, 0), length = 0.1, angle = 15,
       col = c("violetred3", "YellowGreen", "SteelBlue", "wheat4"))
arrows(q, c(0.25, 0.5, 0.75, 0.90), rep(min(returns)-0.1, 4), c(0.25, 0.5, 0.75, 0.9), angle = 0,
       col = c("violetred3", "YellowGreen", "SteelBlue", "wheat4"))
points(q, c(0.25, 0.5, 0.75, 0.9), bg = "white", pch = 21,
       col = c("violetred3", "YellowGreen", "SteelBlue", "wheat4"))
legend("right", paste(c("Q25 = ", "Q50 = ", "Q75 = ", "Q90 = "), round(q, 4)), 
       bty = "n", text.col = c("violetred3", "YellowGreen", "SteelBlue", "wheat4"))

# Skewness analysis (asymmetry of distribution)
# Measures how symmetric the distribution is
cat("\nSkewness:\n")
S1 <- e1071::skewness(returns, type = 1)  # Type 1: Classical moment-based
S2 <- e1071::skewness(returns, type = 2)  # Type 2: Adjusted Fisher-Pearson standardized moment
S3 <- e1071::skewness(returns, type = 3)  # Type 3: Based on third central moment
cat("Skewness (type 1):", round(S1, 4), "\n")
cat("Skewness (type 2):", round(S2, 4), "\n")
cat("Skewness (type 3):", round(S3, 4), "\n")

# D'Agostino skewness test (null: normality)
cat("\nD'Agostino test for skewness:\n")
print(moments::agostino.test(returns))

# Kurtosis analysis (tailedness or peakedness of distribution)
cat("\nKurtosis:\n")
K1 <- e1071::kurtosis(returns, type = 1)  # Type 1: Classical moment-based
K2 <- e1071::kurtosis(returns, type = 2)  # Type 2: Adjusted (excess kurtosis = 0 for normal)
K3 <- e1071::kurtosis(returns, type = 3)  # Type 3: Based on fourth central moment
cat("Kurtosis (type 1):", round(K1, 4), "\n")
cat("Kurtosis (type 2):", round(K2, 4), "\n")
cat("Kurtosis (type 3):", round(K3, 4), "\n")

# Anscombe-Glynn kurtosis test (null: kurtosis equals 3 as in normal distribution)
cat("\nAnscombe-Glynn test for kurtosis:\n")
print(moments::anscombe.test(returns))



# --- Fit Normal Distribution using MLE ---

cat("\nFitting Normal Distribution:\n")

# Estimate sample mean and standard deviation
m <- mean(returns)
sd_ret <- sd(returns)

# Log-likelihood function for normal distribution
f_norm <- function(theta, returns) {
  sum(-dnorm(returns, mean = theta[1], sd = theta[2], log = TRUE))
}

# Numerical optimization (MLE)
fit_norm <- nlminb(c(m, sd_ret), f_norm, returns = returns, 
                   lower = c(-Inf, 0), upper = c(Inf, Inf))

# Display fitted parameters
cat("Estimated parameters (mean, sd):", round(fit_norm$par, 4), "\n")
cat("Log-likelihood value:", fit_norm$objective, "\n")

# --- Goodness-of-fit test: Anderson-Darling for Normality ---

cat("\nAnderson-Darling test for normal distribution:\n")

# Perform test using ADGofTest package
norm_test <- ADGofTest::ad.test(returns, pnorm, m, sd_ret)
print(norm_test)

# --- Plot: Empirical vs Fitted Normal Distribution ---

# Set plotting layout: 1 row, 2 columns
par(mfcol = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)

# Calculate upper limit for y-axis from maximum density
max_dens <- max(density(returns)$y, dnorm(m, m, sd_ret))

# Plot density with fitted normal curve
plot(density(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Density -', asset_name),
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
       legend = c('Empirical', 'Theoretical'), 
       col = c('SteelBlue', 'violetred3'))

# Plot empirical CDF with normal CDF
plot(ecdf(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Empirical CDF -', asset_name))

curve(pnorm(x, m, sd_ret), 
      add = TRUE, 
      col = 'violetred3', 
      lwd = 3)

legend("topleft", 
       bg = 'white', 
       bty = "n", 
       lty = 1, 
       lwd = c(4, 3),
       legend = c('Empirical', 'Theoretical'), 
       col = c('SteelBlue', 'violetred3'))


# --- Fit Cauchy Distribution using MLE ---

cat("\nFitting Cauchy Distribution:\n")

# Initial estimates: location = median, scale ≈ IQR / 2
md_ret <- median(returns)
I_ret <- IQR(returns) / 2

# Log-likelihood function for Cauchy
f_cauchy <- function(theta, returns) {
  sum(-dcauchy(returns, location = theta[1], scale = theta[2], log = TRUE))
}

# MLE via optimization
fit_cauchy <- nlminb(c(md_ret, I_ret), f_cauchy, returns = returns,
                     lower = c(-Inf, 0), upper = c(Inf, Inf))

# Display fitted parameters
cat("Estimated parameters (location, scale):", round(fit_cauchy$par, 4), "\n")


# --- Goodness-of-fit test: Anderson-Darling for Cauchy ---

cat("\nAnderson-Darling test for Cauchy distribution:\n")

# Perform test
cauchy_test <- ADGofTest::ad.test(returns, pcauchy, 
                                  fit_cauchy$par[1], fit_cauchy$par[2])
print(cauchy_test)

# --- Plot: Empirical vs Fitted Cauchy Distribution ---

# Set plotting layout: 1 row, 2 columns
par(mfcol = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)

# Max value for y-axis in density plot
max_dens_cauchy <- max(density(returns)$y, 
                       dcauchy(fit_cauchy$par[1], fit_cauchy$par[1], fit_cauchy$par[2]),
                       na.rm = TRUE)

# Plot density
plot(density(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Density -', asset_name),
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
       legend = c('Empirical', 'Theoretical'), 
       col = c('SteelBlue', 'violetred3'))

# CDF plot
plot(ecdf(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Empirical CDF -', asset_name))

curve(pcauchy(x, fit_cauchy$par[1], fit_cauchy$par[2]), 
      add = TRUE, 
      col = 'violetred3', 
      lwd = 3)

legend("topleft", 
       bg = 'white', 
       bty = "n", 
       lty = 1, 
       lwd = c(4, 3),
       legend = c('Empirical', 'Theoretical'), 
       col = c('SteelBlue', 'violetred3'))

     

# --- Fit Laplace Distribution using MLE ---

cat("\nFitting Laplace Distribution:\n")

# Initial estimates: location = median, scale = mean absolute deviation
ps_ret <- mean(abs(returns - md_ret))

# Log-likelihood function for Laplace (manual MLE)
f_laplace <- function(theta, returns) {
  sum(-VGAM::dlaplace(returns, location = theta[1], scale = theta[2], log = TRUE))
}

# MLE optimization (starting from median and MAD)
fit_laplace <- nlminb(c(md_ret, ps_ret), f_laplace, returns = returns, 
                      lower = c(-Inf, 0), upper = c(Inf, Inf))

# Optional: Alternative estimation using VGAM's vglm
fit_laplace_vglm <- VGAM::vglm(returns ~ 1, VGAM::laplace, 
                               data.frame(returns), trace = FALSE, crit = "l")

# Display fitted parameters
cat("Estimated parameters (location, scale):", round(fit_laplace$par, 4), "\n")

# --- Goodness-of-fit test: Anderson-Darling for Laplace ---

cat("\nAnderson-Darling test for Laplace distribution:\n")

# Perform test using estimated parameters
laplace_test <- ADGofTest::ad.test(returns, VGAM::plaplace, 
                                   fit_laplace$par[1], fit_laplace$par[2])
print(laplace_test)

# --- Plot: Empirical vs Fitted Laplace Distribution ---

# Set plotting layout: 1 row, 2 columns
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)

# Calculate maximum y-value for density plot
max_dens_laplace <- max(
  density(returns)$y, 
  VGAM::dlaplace(md_ret, fit_laplace$par[1], fit_laplace$par[2]), 
  na.rm = TRUE
)

# Plot empirical density and fitted Laplace PDF
plot(density(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Density -', asset_name),
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
       legend = c('Empirical', 'Theoretical'), 
       col = c('SteelBlue', 'violetred3'))

# Plot empirical CDF and fitted Laplace CDF
plot(ecdf(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Empirical CDF -', asset_name))

curve(VGAM::plaplace(x, fit_laplace$par[1], fit_laplace$par[2]), 
      add = TRUE, 
      col = 'violetred3', 
      lwd = 3)

legend("topleft", 
       bg = 'white', 
       bty = "n", 
       lty = 1, 
       lwd = c(4, 3),
       legend = c('Empirical', 'Theoretical'), 
       col = c('SteelBlue', 'violetred3'))


     
# --- Fit Stable Distribution using MLE via fBasics ---

cat("\nFitting Stable Distribution:\n")

# Fit α-stable distribution using maximum likelihood estimation (MLE)
stable_fit <- fBasics::stableFit(returns, type = "mle", doplot = FALSE)

# Extract parameters: (alpha, beta, gamma, delta)
stable_params <- stable_fit@fit$estimate

cat("Estimated stable distribution parameters (alpha, beta, gamma, delta):\n")
print(round(stable_params, 4))

# --- Goodness-of-fit test: Anderson-Darling for Stable distribution ---

cat("\nAnderson-Darling test for stable distribution:\n")

# Apply the test using the CDF from stabledist with fitted parameters
stable_test <- ADGofTest::ad.test(
  returns, 
  stabledist::pstable, 
  alpha = stable_params[1], 
  beta  = stable_params[2], 
  gamma = stable_params[3], 
  delta = stable_params[4]
)

print(stable_test)

# --- Plot: Empirical vs Fitted Stable Distribution ---

# Set plotting layout: 1 row, 2 columns
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)

# Compute maximum y-value for density plot scaling
max_dens_stable <- max(
  density(returns)$y,
  stabledist::dstable(median(returns), 
                      alpha = stable_params[1], 
                      beta  = stable_params[2], 
                      gamma = stable_params[3], 
                      delta = stable_params[4]),
  na.rm = TRUE
)

# Plot empirical density and fitted α-stable PDF
plot(density(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Density -', asset_name),
     ylim = c(0, max_dens_stable))

curve(stabledist::dstable(x, 
                          alpha = stable_params[1], 
                          beta  = stable_params[2], 
                          gamma = stable_params[3], 
                          delta = stable_params[4]), 
      add = TRUE, 
      col = 'violetred3', 
      lwd = 3)

legend("topright", 
       bg = 'white', 
       bty = "n", 
       cex = 1, 
       lty = 1, 
       lwd = c(4, 3),
       legend = c('Empirical', 'Theoretical'), 
       col = c('SteelBlue', 'violetred3'))

# Plot empirical CDF and fitted α-stable CDF
plot(ecdf(returns), 
     col = 'SteelBlue', 
     lwd = 4, 
     main = paste('Empirical CDF -', asset_name))

curve(stabledist::pstable(x, 
                          alpha = stable_params[1], 
                          beta  = stable_params[2], 
                          gamma = stable_params[3], 
                          delta = stable_params[4]), 
      add = TRUE, 
      col = 'violetred3', 
      lwd = 3)

legend("topleft", 
       bg = 'white', 
       bty = "n", 
       lty = 1, 
       lwd = c(4, 3),
       legend = c('Empirical', 'Theoretical'), 
       col = c('SteelBlue', 'violetred3'))




# Probability calculations
cat("\nProbabilities for the stable distribution:\n")

# Probability that the return is between -0.475% and 0.84%
# Calculate cumulative probability at upper bound minus cumulative probability at lower bound
prob1 <- stabledist::pstable(0.0084, 
                             stable_params[1], stable_params[2],
                             stable_params[3], stable_params[4]) -
  stabledist::pstable(-0.00475, 
                      stable_params[1], stable_params[2],
                      stable_params[3], stable_params[4])
cat("P( -0.475% < R < 0.84% ) =", round(prob1, 4), "\n")

# Plot the probability density function highlighting the interval [-0.475%, 0.84%]
par(mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
curve(stabledist::dstable(x, 
                          stable_params[1], stable_params[2],
                          stable_params[3], stable_params[4]),
      -0.1, 0.1, 
      col = 'white',  # Initialize empty plot with white curve to set axes and limits
      main = paste('P( -0.475% < R < 0.84% ) -', asset_name),
      xlab = "Return", ylab = "Density")

# Generate sequence over the target interval for shading
x <- seq(-0.00475, 0.0084, length = 300)
# Compute density values for the stable distribution on this interval
y <- stabledist::dstable(x, 
                         stable_params[1], stable_params[2],
                         stable_params[3], stable_params[4])

# Shade the area under the density curve between the bounds to visualize the probability
polygon(c(-0.00475, x, 0.0084), c(0, y, 0),
        col = 'Violet', border = "Dark Magenta")

# Overlay the stable distribution density curve for the full plotting range
curve(stabledist::dstable(x, 
                          stable_params[1], stable_params[2],
                          stable_params[3], stable_params[4]),
      -0.1, 0.1, 
      lwd = 4, 
      col = 'Dark Magenta', 
      add = TRUE)

# Probability that the return is between 0% and 5%
# Same approach: difference of cumulative distribution function values at bounds
prob2 <- stabledist::pstable(0.05, 
                             stable_params[1], stable_params[2],
                             stable_params[3], stable_params[4]) -
  stabledist::pstable(0.00, 
                      stable_params[1], stable_params[2],
                      stable_params[3], stable_params[4])
cat("P( 0% < R < 5% ) =", round(prob2, 4), "\n")

# Plot the probability density function highlighting the interval [0%, 5%]
par(mar = c(4, 4, 1, 1) + 0.1, mgp = c(3, 0.6, 0), las = 1)
curve(stabledist::dstable(x, 
                          stable_params[1], stable_params[2],
                          stable_params[3], stable_params[4]),
      -0.1, 0.1, 
      col = 'white',  # Blank curve to initialize plot
      main = paste('P( 0% < R < 5% ) -', asset_name),
      xlab = "Return", ylab = "Density")

# Sequence over the second target interval for shading
x <- seq(0.00, 0.05, length = 300)
# Density values on this interval
y <- stabledist::dstable(x, 
                         stable_params[1], stable_params[2],
                         stable_params[3], stable_params[4])

# Shade area under curve for this interval
polygon(c(0.00, x, 0.05), c(0, y, 0),
        col = 'Violet', border = "Dark Magenta")

# Overlay stable distribution density curve again
curve(stabledist::dstable(x, 
                          stable_params[1], stable_params[2],
                          stable_params[3], stable_params[4]),
      -0.1, 0.1, 
      lwd = 4, 
      col = 'Dark Magenta', 
      add = TRUE)
