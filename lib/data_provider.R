
library(dplyr)

dat <- new.env()

data_init <- function(){
  
  d <- readRDS("data/complied_dat")
  abbr_list <- readRDS("data/complied_abbr_list")
  
  c("10-Year G-Sec Yield (FBIL)", "182-Day Treasury Bill (Primary) Yield", 
    "364-Day Treasury Bill (Primary) Yield", "91-Day Treasury Bill (Primary) Yield"
  )
  
  custom_abbr_list <-
    tibble(
      Sr.No. = NA,
      Abbreviation = c(
        "GDP",
        "GSec",
        "Tbill",
        "Tbill",
        "Tbill"
      ), 
      `Full Form` = c(
        "GDP At Market Prices  (At Current Prices)",
        "10-Year G-Sec Yield (FBIL)",
        "182-Day Treasury Bill (Primary) Yield", 
        "364-Day Treasury Bill (Primary) Yield", 
        "91-Day Treasury Bill (Primary) Yield"
      )
    ) %>% 
    mutate(
      ff_name = `Full Form` %>% str_curate(), 
      abb_name = Abbreviation %>% str_curate())
  
  abbr_list <- abbr_list %>% bind_rows(custom_abbr_list) %>% distinct()
  
  
  var_names <- d %>% distinct(var_name, var_short_name)
  
  assign("main", d, envir = dat)
  assign("abbr", abbr_list, envir = dat)
  assign("vars", var_names, envir = dat)
  assign("dates", d$date %>% unique(), envir = dat)
  
}


data_init()
