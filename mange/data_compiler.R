

# Data prepare 
require(tidyverse)
source("lib/string.R")

d1 <- tidycells::read_cells("data/50 Macroeconomic Indicators.xlsx")

#d2 <- tidycells::read_cells("data/Other Macroeconomic Indicators.xlsx")

d <- d1 %>% select(date = collated_2, freq = table_tag, var = collated_4, value)

d <- d %>% mutate(value = as.numeric(value) 
                  #var = var %>% str_remove_all("[^a-zA-Z0-9\\-= ]") %>% str_replace_all(" +"," ") %>% str_trim()
)


d <- d %>% mutate(unit_auto = var %>% str_extract_all("\\(.+\\)") %>% map_chr(~.x %>% rev %>% .[1]) %>% str_trim() %>%  str_replace(" \\(","<(") %>% str_split("<") %>% map_chr(~.x %>% rev %>% .[1]) %>% str_replace_all(" +"," "))

# d %>% filter(is.na(unit_auto)) %>% distinct(var)
# NA cases are % only
d$unit_auto[is.na(d$unit_auto)] <- "(%)"
d$unit_auto[d$unit_auto %in% c("(2012=100)","(2011-12=100)")] <- "(%)"
d$unit_auto[d$unit_auto %in% c("(Month End)")] <- "None"

d<- d %>% rename(unit = unit_auto)

# Abbreviation 
abbr_list <- readxl::read_excel("data/Abbreviation list_HBS_full.xlsx")

unit_list <- c("%","inr crore","usd million")

abbr_list <- abbr_list %>% mutate(ff_name = `Full Form` %>% str_curate(), abb_name = Abbreviation %>% str_curate())

d <- d %>% mutate(var_name = var %>% str_curate())

d <- d %>% mutate(var_name = var_name %>% str_remove_all("crore|us million") %>% str_curate())

# auto short name suggestion
stop_words <- c(
  # all only number words
  # can be obtained via 
  # d %>% pull(var_name) %>% unique() %>% str_split(" ") %>% unlist() %>% unique() %>% as.numeric() %>% unique() %>% as.character() %>% dput()
  "2012", "100", "2011", "12", "1", "3", "6", "91", "182", 
  "364", "10",
  "in","of","at","for","to","by","and","vis","s"
)

d <- d %>% 
  mutate(
    var_short_name = 
      var_name %>% 
      str_split(" ") %>% 
      map_chr(~{
        #.x %>% setdiff(stop_words) %>% paste0(collapse = " ")
        .x %>% setdiff(stop_words) %>% str_sub(1,1) %>% paste0(collapse = "")
      })
  )

# date range attachment 

d <- d %>% mutate(date = as.Date(date)) %>% arrange(date)

for_a_var_x_freq <- function(dp){
  
  dp <- dp %>% arrange(date)
  lag_this <- case_when(
    dp$freq[1]=="Fortnightly"~14,
    dp$freq[1]=="Weekly"~7,
    # Date will not matter only months and qtrs will matter
    dp$freq[1]=="Monthly"~30,
    dp$freq[1]=="Quarterly"~30*3,
    #daily
    .default = 1
  )
  
  if(dp$freq[1] %in% c("Fortnightly","Weekly")){
    
    dp <- dp %>% mutate(previous_date=lag(date))
    dp$previous_date[is.na(dp$previous_date)] <- min(dp$date)-lag_this
    
    dp <- dp %>% 
      mutate(
        from_date = pmax(previous_date+1, date-lag_this),
        to_date = date
      ) %>% 
      select(-previous_date)
  }
  
  if(dp$freq[1]=="Monthly"){
    
    tday <- function(x, is_max = TRUE){
      m <- month(x)
      y <- year(x)
      if(is_max){
        d <- days_in_month(x)
      }else{
        d <- 1
      }
      
      paste0(y,"-",m,"-", d) %>% as.Date()
      
    }
    
    dp <- dp %>% 
      mutate(
        from_date = date %>% tday(is_max = FALSE),
        to_date = date %>% tday(is_max = TRUE)
      ) 
  }
  
  if(dp$freq[1]=="Quarterly"){
    
    tday <- function(x, is_max = TRUE){
      q <- quarter(x)
      y <- year(x)
      if(is_max){
        m <- case_when(
          q==1~3,
          q==2~6,
          q==3~9,
          q==4~12,
          .default = 1)
        d <- paste0(y, "-", m, "-01") %>% as.Date() %>% days_in_month()
      }else{
        m <- case_when(
          q==1~1,
          q==2~4,
          q==3~7,
          q==4~10,
          .default = 1)
        d <- 1
      }
      
      paste0(y,"-",m,"-", d) %>% as.Date()
      
    }
    
    dp <- dp %>% 
      mutate(
        from_date = date %>% tday(is_max = FALSE),
        to_date = date %>% tday(is_max = TRUE)
      ) 
  }
  
  
  dp
}

dl <- d %>% 
  group_by(var, freq) %>% 
  group_split()

d <- dl %>% map_dfr(for_a_var_x_freq)

d <- d %>% 
  mutate(
    var_nice_name = var %>% str_fine_maker(),  
    var_without_unit = 
      map2_chr(
        var_nice_name, unit, 
        ~{
          str_split(.x," ")[[1]] %>% setdiff(str_split(.y," ")[[1]]) %>% paste0(collapse = " ")
        })
  )

saveRDS(d, "data/complied_dat")
saveRDS(abbr_list, "data/complied_abbr_list")




