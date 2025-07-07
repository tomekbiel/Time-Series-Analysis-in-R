# load_market_data.R
# Script for loading and processing financial market data

# 1. Required Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table,    # Fast data loading and processing
  lubridate,     # Date-time manipulation
  tictoc,        # Execution time measurement
  future.apply,  # Parallel processing
  stable,        # Stable distributions
  fBasics,       # Basic financial analytics
  alphastable,   # Advanced stable distribution tools
  stabledist,    # Stable distribution functions
  ADGofTest      # Goodness-of-fit tests
)

# 2. Configuration Setup ----
setup_config <- function() {
  config <- list(
    # Paths
    project_dir = normalizePath("C:/R/Time-Series-Analysis-in-R"),
    data_dir = "data/historical",
    
    # Instrument symbols
    symbols = c("US.100", "OIL.WTI", "VIX", "EURUSD", "GOLDs", "SILVERs", "USDJPY", "US.500"),
    timeframes = c("M1", "M5", "M15", "H1", "H4", "D1"),
    
    # Processing settings
    use_parallel = TRUE,
    n_workers = max(1, parallel::detectCores() - 1),
    
    # Datetime format
    date_format = "%Y.%m.%d %H:%M",
    timezone = "Europe/Warsaw",
    
    # Caching settings
    cache_file = "data/processed/full_market_data.rds"
  )
  
  # Set project directory
  setwd(config$project_dir)
  
  # Create required directories
  sapply(c("data/processed", "logs"), function(dir) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  })
  
  return(config)
}

# 3. Data Loading Functions ----

#' Load single CSV file
#' @param symbol Instrument name (e.g. "US.100")
#' @param timeframe Time interval (e.g. "H1")
#' @param config Configuration object
#' @return data.table with time and close columns
load_single_file <- function(symbol, timeframe, config) {
  # Add "+" to symbol in file path
  symbol_plus <- paste0(symbol, "+")
  file_path <- file.path(
    config$project_dir,
    config$data_dir,
    symbol_plus,
    timeframe,
    paste0(symbol_plus, "_", timeframe, ".csv")
  )
  
  if (!file.exists(file_path)) {
    warning("File missing: ", file_path)
    return(NULL)
  }
  
  tryCatch({
    # Load only time and close columns
    dt <- fread(
      file = file_path,
      select = c("time", "close"),
      colClasses = c(time = "character", close = "numeric"),
      showProgress = FALSE,
      fill = TRUE  # Handle missing columns
    )
    
    # Check if data loaded
    if (nrow(dt) == 0) {
      warning("Empty file: ", file_path)
      return(NULL)
    }
    
    # Convert time format
    dt[, time := as.POSIXct(time, format = config$date_format, tz = config$timezone)]
    
    # Remove time duplicates and sort
    dt <- unique(dt, by = "time")[order(time)]
    
    return(dt)
    
  }, error = function(e) {
    warning("Error loading file ", basename(file_path), ": ", e$message)
    return(NULL)
  })
}

#' Load data for multiple symbols and timeframes
#' @param symbols Vector of symbol names
#' @param timeframes Vector of time intervals
#' @param config Configuration object
#' @return List of data.tables with time and close data
load_market_data <- function(symbols, timeframes, config) {
  # Generate all symbol-timeframe combinations
  combinations <- expand.grid(
    symbol = symbols,
    timeframe = timeframes,
    stringsAsFactors = FALSE
  )
  
  # Single file processing function
  process_file <- function(i) {
    load_single_file(combinations$symbol[i], combinations$timeframe[i], config)
  }
  
  # Parallel or sequential loading
  if (config$use_parallel && config$n_workers > 1) {
    plan(multisession, workers = config$n_workers)
    all_data <- future_lapply(
      seq_len(nrow(combinations)), 
      process_file,
      future.seed = TRUE
    )
  } else {
    all_data <- lapply(seq_len(nrow(combinations)), process_file)
  }
  
  # Generate clean names: us100d1 format
  clean_names <- apply(combinations, 1, function(row) {
    tolower(gsub("[^[:alnum:]]", "", paste0(row["symbol"], row["timeframe"])))
  })
  names(all_data) <- clean_names
  
  # Remove empty elements
  all_data <- all_data[!sapply(all_data, is.null)]
  
  return(all_data)
}

# 4. Helper Functions ----

#' Retrieve data for specific symbol/timeframe
#' @param data_list Data list object
#' @param symbol Instrument name
#' @param timeframe Time interval
#' @return data.table or NULL if not found
get_symbol_data <- function(data_list, symbol, timeframe) {
  clean_name <- tolower(gsub("[^[:alnum:]]", "", paste0(symbol, timeframe)))
  data_list[[clean_name]]
}

#' Display data preview
#' @param data_list Data list object
#' @param n Number of rows to display
print_data_preview <- function(data_list, n = 5) {
  lapply(names(data_list), function(name) {
    cat("\n====", name, "====\n")
    print(head(data_list[[name]], n))
  })
  invisible(data_list)
}

# 5. Usage Example ----
if (sys.nframe() == 0) {
  # Initialize configuration
  config <- setup_config()
  
  # Cache loading function
  load_market_data_cached <- function() {
    if (file.exists(config$cache_file)) {
      cat("Loading cached data...\n")
      return(readRDS(config$cache_file))
    } else {
      cat("No cache - processing data...\n")
      data <- load_market_data(
        symbols = config$symbols,
        timeframes = config$timeframes,
        config = config
      )
      saveRDS(data, config$cache_file)
      return(data)
    }
  }
  
  # Load ALL data
  tic("Loading ALL data")
  market_data <- load_market_data_cached()
  toc()
  
  # Assign to global environment
  list2env(market_data, envir = .GlobalEnv)
  
  # Display data structure
  cat("\nData structure (first 2 list elements):\n")
  str(market_data[1:min(2, length(market_data))])
  
  # Example usage
  cat("\nSample data for us100d1:\n")
  if(exists("us100d1")) print(head(us100d1, 3))
  
  # Convert to time series function
  convert_to_ts <- function(dt) {
    if (nrow(dt) == 0) return(NULL)
    
    # Calculate median time difference in minutes
    time_diff <- as.numeric(median(diff(dt$time)), units = "mins")
    
    # Automatic frequency detection
    if (time_diff <= 1) freq <- 1440       # M1
    else if (time_diff <= 5) freq <- 288   # M5
    else if (time_diff <= 15) freq <- 96   # M15
    else if (time_diff <= 60) freq <- 24   # H1
    else if (time_diff <= 240) freq <- 6   # H4
    else freq <- 1                         # D1
    
    # Full time start with date/hour
    start_date <- min(dt$time)
    start_decimal <- c(
      year(start_date) + (yday(start_date) - 1) / 365,
      hour(start_date) / 24 + minute(start_date) / (24 * 60)
    )
    
    ts_data <- ts(
      dt$close,
      frequency = freq,
      start = start_decimal
    )
    
    return(ts_data)
  }  
  
  # Time series conversion example
  if(exists("us100d1")) {
    us100_ts <- convert_to_ts(us100d1)
    cat("\nTime series for us100d1 (first 5 values):\n")
    print(head(us100_ts, 5))
  }
}