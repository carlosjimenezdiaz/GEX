getting_option_chain <- function(db_Ratios, environment_r){
  
  # Setting the years for the Option Chain
  sYear  <- lubridate::year(Sys.Date())
  eYear  <- lubridate::year(Sys.Date()) + 1
  
  # Getting the Option Chain from Yahoo Finance
  option_chain_yahoo <- NULL
  
  for(i in db_Ratios$Symbol){ # i <- "SPY"
    db_option <- getOptionChain(i, src = "yahoo", Exp = str_glue("{sYear}/{eYear}")) %>%
      unlist(recursive = FALSE) %>%
      enframe() %>%
      unnest(cols = c(value)) %>%
      dplyr::mutate(Symbol = i)
    
    option_chain_yahoo <- bind_rows(option_chain_yahoo, db_option)
  }
  
  # Removing NAs
  option_chain_yahoo <- option_chain_yahoo %>% replace(is.na(.), 0)
  
  # Fixing and Enhancing the database
  option_chain_yahoo_enhanced <- option_chain_yahoo %>%
    left_join(db_Ratios, by = "Symbol") %>%
    dplyr::mutate(Symbol = case_when(Symbol == "^SPX" ~ "SPX", TRUE ~ Symbol))
  
  # Addaping the structure of the option chain to US or EU machines
  if(environment_r == "US"){
    # Fixing some formats
    db_option_chain_final <- option_chain_yahoo_enhanced %>%
      dplyr::mutate(data_month = substr(name, 1, 3),  # Extracting just the portion related to the month
                    data_day   = substr(name, 5, 6),  # Extracting just the portion related to the day
                    data_year  = substr(name, 8, 11), # Extracting just the portion related to the year
                    type       = substr(name, 13, length(name)),
                    data_month = case_when(data_month == "Jan" ~ "01",
                                           data_month == "Feb" ~ "02",
                                           data_month == "Mar" ~ "03",
                                           data_month == "Apr" ~ "04",
                                           data_month == "May" ~ "05",
                                           data_month == "Jun" ~ "06",
                                           data_month == "Jul" ~ "07",
                                           data_month == "Aug" ~ "08",
                                           data_month == "Sep" ~ "09",
                                           data_month == "Oct" ~ "10",
                                           data_month == "Nov" ~ "11",
                                           data_month == "Dec" ~ "12",
                                           TRUE ~ data_month),
                    final_date   = as.Date(str_glue("{data_year}-{data_month}-{data_day}"), format = "%Y-%m-%d"),
                    Time_Process = Sys.time())
  }else{
    # Fixing some formats
    db_option_chain_final <- option_chain_yahoo_enhanced %>%
      dplyr::mutate(data_month = substr(name, 1, 3),  # Extracting just the portion related to the month
                    data_day   = substr(name, 6, 7),  # Extracting just the portion related to the day
                    data_year  = substr(name, 9, 12), # Extracting just the portion related to the year
                    type       = substr(name, 14, length(name)),
                    data_month = case_when(data_month == "ene" ~ "01",
                                           data_month == "feb" ~ "02",
                                           data_month == "mar" ~ "03",
                                           data_month == "abr" ~ "04",
                                           data_month == "may" ~ "05",
                                           data_month == "jun" ~ "06",
                                           data_month == "jul" ~ "07",
                                           data_month == "ago" ~ "08",
                                           data_month == "sep" ~ "09",
                                           data_month == "oct" ~ "10",
                                           data_month == "nov" ~ "11",
                                           data_month == "dic" ~ "12",
                                           TRUE ~ data_month),
                    final_date   = as.Date(str_glue("{data_year}-{data_month}-{data_day}"), format = "%Y-%m-%d"),
                    Time_Process = Sys.time())
  }
  
  # Adding the Current Prices and Time to Expiration (in Years)
  db_option_chain_final <- db_option_chain_final %>%
    dplyr::select(final_date, type, Strike, Bid, Ask, OI, IV, Symbol, ITM, Time_Process, Price_Ratios, Multiplier, div_yield, risk_free_rate, Spot_Price) %>% 
    replace(is.na(.), 0) %>%
    dplyr::mutate(time_expiration_years = (((final_date %>% as.Date()) - Sys.Date()) %>% as.numeric())/365,
                  time_expiration_years = case_when(time_expiration_years < 0 ~ 0, TRUE ~ time_expiration_years))

  # Returning the result
  return(db_option_chain_final)
}

# Folder Creation ----
if(dir.exists("00_scripts/")){
  dump(list = c("getting_option_chain"), file = "00_scripts/getting_option_chain.R", append = FALSE,
       control = "all", envir = parent.frame(), evaluate = TRUE)
}else{
  fs::dir_create("00_scripts/") 
  dump(list = c("getting_option_chain"), file = "00_scripts/getting_option_chain.R", append = FALSE)
}
