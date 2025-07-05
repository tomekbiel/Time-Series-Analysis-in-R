# load_market_data.R
# Skrypt do wczytywania i przetwarzania danych rynkowych

# 1. Wymagane pakiety ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table,    # Szybkie wczytywanie i przetwarzanie danych
  lubridate,     # Praca z datami
  tictoc,        # Pomiar czasu wykonywania
  future.apply,   # Równoległe przetwarzanie
  stable,
  fBasics
)

# 2. Konfiguracja ----
setup_config <- function() {
  config <- list(
    # Ścieżki
    project_dir = normalizePath("C:/R/Time-Series-Analysis-in-R"),
    data_dir = "data/historical",
    
    # Lista symboli
    symbols = c("US.100", "OIL.WTI", "VIX", "EURUSD", "GOLDs", "SILVERs", "USDJPY", "US.500"),
    timeframes = c("M1", "M5", "M15", "H1", "H4", "D1"),
    
    # Ustawienia przetwarzania
    use_parallel = TRUE,
    n_workers = max(1, parallel::detectCores() - 1),
    
    # Ustawienia daty i czasu
    date_format = "%Y.%m.%d %H:%M",
    timezone = "Europe/Warsaw",
    
    # Ustawienia wczytywania
    cache_file = "data/processed/full_market_data.rds"
  )
  
  # Ustaw katalog projektu
  setwd(config$project_dir)
  
  # Utwórz katalogi
  sapply(c("data/processed", "logs"), function(dir) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  })
  
  return(config)
}

# 3. Funkcje do wczytywania danych ----

#' Wczytaj pojedynczy plik CSV
#' @param symbol Nazwa symbolu (np. "US.100")
#' @param timeframe Interwał czasowy (np. "H1")
#' @param config Konfiguracja
#' @return data.table z kolumnami time i close
load_single_file <- function(symbol, timeframe, config) {
  # Dodaj "+" do symbolu w nazwie pliku
  symbol_plus <- paste0(symbol, "+")
  file_path <- file.path(
    config$project_dir,
    config$data_dir,
    symbol_plus,
    timeframe,
    paste0(symbol_plus, "_", timeframe, ".csv")
  )
  
  if (!file.exists(file_path)) {
    warning("Brak pliku: ", file_path)
    return(NULL)
  }
  
  tryCatch({
    # Wczytaj tylko kolumny time i close, ignoruj błędy struktury pliku
    dt <- fread(
      file = file_path,
      select = c("time", "close"),
      colClasses = c(time = "character", close = "numeric"),
      showProgress = FALSE,
      fill = TRUE  # Ignoruj brakujące kolumny
    )
    
    # Sprawdź czy wczytano dane
    if (nrow(dt) == 0) {
      warning("Plik jest pusty: ", file_path)
      return(NULL)
    }
    
    # Konwersja czasu
    dt[, time := as.POSIXct(time, format = config$date_format, tz = config$timezone)]
    
    # Usuń duplikaty czasowe i posortuj
    dt <- unique(dt, by = "time")[order(time)]
    
    return(dt)
    
  }, error = function(e) {
    warning("Błąd wczytywania pliku ", basename(file_path), ": ", e$message)
    return(NULL)
  })
}

#' Wczytaj dane dla wielu symboli i timeframe'ów
#' @param symbols Wektor nazw symboli
#' @param timeframes Wektor interwałów czasowych
#' @param config Konfiguracja
#' @return Lista data.table z danymi (tylko time i close)
load_market_data <- function(symbols, timeframes, config) {
  # Przygotuj wszystkie kombinacje symboli i timeframe'ów
  combinations <- expand.grid(
    symbol = symbols,
    timeframe = timeframes,
    stringsAsFactors = FALSE
  )
  
  # Funkcja do przetwarzania pojedynczego pliku
  process_file <- function(i) {
    load_single_file(combinations$symbol[i], combinations$timeframe[i], config)
  }
  
  # Wczytaj dane równolegle lub sekwencyjnie
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
  
  # Generuj nazwy w formacie: us100d1
  clean_names <- apply(combinations, 1, function(row) {
    tolower(gsub("[^[:alnum:]]", "", paste0(row["symbol"], row["timeframe"])))
  })
  names(all_data) <- clean_names
  
  # Usuń puste elementy
  all_data <- all_data[!sapply(all_data, is.null)]
  
  return(all_data)
}

# 4. Funkcje pomocnicze ----

#' Pobierz dane dla konkretnego symbolu i timeframe'u
#' @param data_list Lista z danymi
#' @param symbol Nazwa symbolu
#' @param timeframe Interwał czasowy
#' @return data.table z danymi lub NULL jeśli brak
get_symbol_data <- function(data_list, symbol, timeframe) {
  clean_name <- tolower(gsub("[^[:alnum:]]", "", paste0(symbol, timeframe)))
  data_list[[clean_name]]
}

#' Wyświetl podgląd danych
#' @param data_list Lista z danymi
#' @param n Liczba wierszy do wyświetlenia (domyślnie 5)
print_data_preview <- function(data_list, n = 5) {
  lapply(names(data_list), function(name) {
    cat("\n====", name, "====\n")
    print(head(data_list[[name]], n))
  })
  invisible(data_list)
}

# 5. Przykład użycia ----
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
  
  # Przypisz dane do nazwanych obiektów w środowisku globalnym
  list2env(market_data, envir = .GlobalEnv)
  
  # Wyświetl podgląd struktury danych
  cat("\nStruktura danych (pierwsze 2 elementy listy):\n")
  str(market_data[1:min(2, length(market_data))])
  
  # Przykładowe użycie
  cat("\nPrzykładowe dane dla us100d1:\n")
  if(exists("us100d1")) print(head(us100d1, 3))
  
  # Funkcja pomocnicza do konwersji na ts
  convert_to_ts <- function(dt) {
    if (nrow(dt) == 0) return(NULL)
    
    # Oblicz średni odstęp czasowy w minutach
    time_diff <- as.numeric(median(diff(dt$time)), units = "mins")
    
    # Automatyczne określanie częstotliwości
    if (time_diff <= 1) freq <- 1440       # M1
    else if (time_diff <= 5) freq <- 288   # M5
    else if (time_diff <= 15) freq <- 96   # M15
    else if (time_diff <= 60) freq <- 24   # H1
    else if (time_diff <= 240) freq <- 6   # H4
    else freq <- 1                         # D1
    
    # Pełny start czasu z uwzględnieniem daty i godziny
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
  # Przykład konwersji do szeregu czasowego
  if(exists("us100d1")) {
    us100_ts <- convert_to_ts(us100d1)
    cat("\nSzereg czasowy dla us100d1 (pierwsze 5 wartości):\n")
    print(head(us100_ts, 5))
  }
}
