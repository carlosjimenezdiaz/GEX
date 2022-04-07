Current_GEX <-
function(file_name, Ticker_Info_DF, DIY){
  
  # TEST #
  # file_name      <- "spx_quotedata.csv"
  # Ticker_Info_DF <- db_Tickers
  # DIY <- 365
  
  # Extracting the Ticker from the CBOE file
  Ticker_Symbol <- file_name %>% strsplit(split = "_") %>% pluck(1) %>% first() %>% toupper()
  
  # Looking the characteristics of the Ticker in our Global Dataframe (Ticker Info)
  Ticker_Info_Filtered <- Ticker_Info_DF %>% dplyr::filter(Ticker == Ticker_Symbol)
  
  # Reading the data from the CBOE File (General Information)
  CBOE_General_Info <- read.csv(str_glue("cboe_file/{file_name}"), sep = '\t', header = FALSE) %>%
    dplyr::slice(1:2) %>% 
    splitstackshape::cSplit("V1",",")
  
  Spot_Price <- CBOE_General_Info$V1_2[1] %>% str_remove_all("Last: ") %>% as.numeric()
  
  # Extracting the Option Chain
  db_Option_Chain <- read.csv(str_glue("cboe_file/{file_name}"), sep = '\t', header = FALSE) %>%
    dplyr::slice(3:n()) %>% 
    splitstackshape::cSplit("V1",",") %>%
    janitor::row_to_names(1)
  
  # Extracting data from the Calls
  db_Option_Chain_Calls <- db_Option_Chain %>%
    dplyr::select(1, 5, 6, 8, 9, 10, 11, 12) %>%
    dplyr::mutate(Expiration_Month  = case_when(substr(`Expiration Date`, 5, 7) == "Jan" ~ "01",
                                                substr(`Expiration Date`, 5, 7) == "Feb" ~ "02",
                                                substr(`Expiration Date`, 5, 7) == "Mar" ~ "03",
                                                substr(`Expiration Date`, 5, 7) == "Apr" ~ "04",
                                                substr(`Expiration Date`, 5, 7) == "May" ~ "05",
                                                substr(`Expiration Date`, 5, 7) == "Jun" ~ "06",
                                                substr(`Expiration Date`, 5, 7) == "Jul" ~ "07",
                                                substr(`Expiration Date`, 5, 7) == "Aug" ~ "08",
                                                substr(`Expiration Date`, 5, 7) == "Sep" ~ "09",
                                                substr(`Expiration Date`, 5, 7) == "Oct" ~ "10",
                                                substr(`Expiration Date`, 5, 7) == "Nov" ~ "11",
                                                substr(`Expiration Date`, 5, 7) == "Dec" ~ "12",
                                                TRUE ~ substr(`Expiration Date`, 5, 7)),
                  Expiration_Day    = substr(`Expiration Date`, 9, 10),
                  Expiration_Year   = substr(`Expiration Date`, 11, 15),
                  Fixed_Expiration_Date = str_glue("{Expiration_Day}/{Expiration_Month}/{Expiration_Year}") %>% as.Date(format = "%d/%m/%Y")) %>%
    janitor::clean_names() %>%
    dplyr::select(fixed_expiration_date, bid, ask, iv, delta, gamma, open_interest, strike) %>%
    dplyr::mutate(time_expiration_years = (((fixed_expiration_date %>% as.Date()) - Sys.Date()) %>% as.numeric())/DIY,
                  time_expiration_years = case_when(time_expiration_years == 0 ~ 1/DIY, TRUE ~ time_expiration_years)) %>%
    dplyr::mutate(Scenarios  = case_when(lubridate::week(fixed_expiration_date) == lubridate::week(Sys.Date()) ~ "W/out this week",
                                         lubridate::month(fixed_expiration_date) == lubridate::month(Sys.Date()) ~ "W/out this month",
                                         TRUE ~ "The Rest"),
                  Spot_Price = Spot_Price,
                  type       = "Calls")
  
  # Extracting data from the Puts
  db_Option_Chain_Puts <- db_Option_Chain %>%
    dplyr::select(1, 16, 17, 19, 20, 21, 22, 12) %>%
    dplyr::mutate(Expiration_Month  = case_when(substr(`Expiration Date`, 5, 7) == "Jan" ~ "01",
                                                substr(`Expiration Date`, 5, 7) == "Feb" ~ "02",
                                                substr(`Expiration Date`, 5, 7) == "Mar" ~ "03",
                                                substr(`Expiration Date`, 5, 7) == "Apr" ~ "04",
                                                substr(`Expiration Date`, 5, 7) == "May" ~ "05",
                                                substr(`Expiration Date`, 5, 7) == "Jun" ~ "06",
                                                substr(`Expiration Date`, 5, 7) == "Jul" ~ "07",
                                                substr(`Expiration Date`, 5, 7) == "Aug" ~ "08",
                                                substr(`Expiration Date`, 5, 7) == "Sep" ~ "09",
                                                substr(`Expiration Date`, 5, 7) == "Oct" ~ "10",
                                                substr(`Expiration Date`, 5, 7) == "Nov" ~ "11",
                                                substr(`Expiration Date`, 5, 7) == "Dec" ~ "12",
                                                TRUE ~ substr(`Expiration Date`, 5, 7)),
                  Expiration_Day    = substr(`Expiration Date`, 9, 10),
                  Expiration_Year   = substr(`Expiration Date`, 11, 15),
                  Fixed_Expiration_Date = str_glue("{Expiration_Day}/{Expiration_Month}/{Expiration_Year}") %>% as.Date(format = "%d/%m/%Y")) %>%
    janitor::clean_names() %>%
    dplyr::select(fixed_expiration_date, bid, ask, iv, delta, gamma, open_interest, strike) %>%
    dplyr::mutate(time_expiration_years = (((fixed_expiration_date %>% as.Date()) - Sys.Date()) %>% as.numeric())/DIY,
                  time_expiration_years = case_when(time_expiration_years == 0 ~ 1/DIY, TRUE ~ time_expiration_years)) %>% # For 0 DTE options, I'm setting DTE = 1 day, otherwise they get excluded
    dplyr::mutate(Scenarios  = case_when(lubridate::week(fixed_expiration_date) == lubridate::week(Sys.Date()) ~ "W/out this week",
                                         lubridate::month(fixed_expiration_date) == lubridate::month(Sys.Date()) ~ "W/out this month",
                                         TRUE ~ "The Rest"),
                  Spot_Price = Spot_Price,
                  type       = "Puts")
  
  # Combining both DF
  db_Option_Chain_Combined <- db_Option_Chain_Calls %>% rbind(db_Option_Chain_Puts)
  
  # GEX Calculation
    # CALLS
    db_Calls <- db_Option_Chain_Combined %>% dplyr::filter(type == "Calls")
    
    GEX_Calls <- greeks(bscall(s  = db_Calls$Spot_Price,
                               k  = db_Calls$strike %>% as.numeric(),
                               v  = db_Calls$iv %>% as.numeric(),
                               r  = Ticker_Info_Filtered$risk_free_rate/100,
                               tt = db_Calls$time_expiration_years,
                               d  = Ticker_Info_Filtered$div_yield/100), complete = TRUE) %>%
      dplyr::select(k, s, Gamma) %>%
      dplyr::mutate(Date = db_Calls$fixed_expiration_date,
                    OI   = db_Calls$open_interest %>% as.numeric(),
                    type = "Calls",
                    GEX  = +1 * Gamma * Ticker_Info_Filtered$Multiplier * OI * s^2 * 0.01)
    
    # PUTS
    db_Puts <- db_Option_Chain_Combined %>% dplyr::filter(type == "Puts")
    
    GEX_Puts <- greeks(bsput(s  = db_Puts$Spot_Price,
                             k  = db_Puts$strike %>% as.numeric(),
                             v  = db_Puts$iv %>% as.numeric(),
                             r  = Ticker_Info_Filtered$risk_free_rate/100,
                             tt = db_Puts$time_expiration_years,
                             d  = Ticker_Info_Filtered$div_yield/100), complete = TRUE) %>%
      dplyr::select(k, s, Gamma) %>%
      dplyr::mutate(Date = db_Puts$fixed_expiration_date,
                    OI   = db_Puts$open_interest %>% as.numeric(),
                    type = "Puts",
                    GEX  = -1 * Gamma * Ticker_Info_Filtered$Multiplier * OI * s^2 * 0.01)
    
    # Merging both GEX DB
    db_GEX <- GEX_Calls %>% 
      rbind(GEX_Puts) %>%
      dplyr::mutate(k = k * Ticker_Info_Filtered$Price_Ratio,
                    s = s * Ticker_Info_Filtered$Price_Ratio)

    # Returning data
    return(db_GEX)
}
