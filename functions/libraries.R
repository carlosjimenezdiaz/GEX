libraries <- function(){
  suppressPackageStartupMessages({
    if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
    if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
    if (!require("derivmkts")) install.packages("derivmkts"); library(derivmkts)
    if (!require("scales")) install.packages("scales"); library(scales)
    if (!require("broom")) install.packages("broom"); library(broom)
    if (!require("tictoc")) install.packages("tictoc"); library(tictoc)
    if (!require("viridis")) install.packages("viridis"); library(viridis)
    if (!require("RMySQL")) install.packages("RMySQL"); library(RMySQL)
    if (!require("DBI")) install.packages("DBI"); library(DBI)
    if (!require("zoo")) install.packages("zoo"); library(zoo)
    if (!require("greeks")) install.packages("greeks"); library(greeks)
    if (!require("Quandl")) install.packages("Quandl"); library(Quandl)
    if (!require("splitstackshape")) install.packages("splitstackshape"); library(splitstackshape)
    if (!require("janitor")) install.packages("janitor"); library(janitor)
    if (!require("gridExtra")) install.packages("gridExtra"); library(gridExtra)
    if (!require("jsonlite")) install.packages("jsonlite"); library(jsonlite)
    if (!require("RQuantLib")) install.packages("RQuantLib"); library(RQuantLib)
    if (!require("pbapply")) install.packages("pbapply"); library(pbapply)
    if (!require("xml2")) install.packages("xml2"); library(xml2)
  })
}

libraries()

# Folder Creation ----
if(dir.exists("00_scripts/")){
  dump(list = c("libraries"), file = "00_scripts/Libraries.R", append = FALSE,
       control = "all", envir = parent.frame(), evaluate = TRUE)
}else{
  fs::dir_create("00_scripts/") 
  dump(list = c("libraries"), file = "00_scripts/Libraries.R", append = FALSE)
}