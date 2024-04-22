
library(stringr)
library(stopwords)
library(stringdist)

library(tidyverse)

str_env <- new.env()

str_curate <- function(x){
  x %>% 
    tolower() %>% 
    str_replace_all("\\-="," ") %>% 
    str_replace_all("[^a-z0-9 ]"," ") %>% 
    str_remove_all("[^a-z0-9 ]") %>% 
    str_replace_all(" +"," ") %>% 
    str_trim()
}

str_fine_maker <- function(x){
  x %>% 
    str_replace_all("\r"," ") %>% 
    str_replace_all("\n"," ") %>% 
    str_replace_all(" +"," ")
}

str_init_env <- function(){
  
  sw_nltk <- stopwords(language = "en", source = "nltk")
  
  stop_words_qry <- c(sw_nltk)
  
  stop_words_qry <- stop_words_qry %>% 
    str_curate() %>% 
    c(stop_words_qry) %>% 
    str_split(" ") %>% 
    unlist() %>% 
    unique()  
  
  assign(x = "stop_words_qry", value = stop_words_qry, envir = str_env)
  
  wds_time <- c(
    month.abb,
    month.name,
    "quarter","qtr","qtrs",
    "q1","q2","q3","q4",
    "month","latest","last",
    "recent"
  ) %>% 
    str_curate()
  
  assign(x = "wds_time", value = wds_time, envir = str_env)
  
  
}


str_time_parse <- function(x){
  
  qy <- function(y){
    y %>% 
      str_split(" ") %>% 
      map_chr(~.x %>% rev %>% paste0(collapse = " ")) %>% 
      yq()
  }
  
  xw <- x %>% paste0(collapse = " ")
  
  fns <- list(
    ym, 
    my,
    yq,
    qy, 
    ymd, 
    ydm, 
    mdy, 
    myd, 
    dmy, 
    dym 
  )
  
  fns_tag <- c(
    "ym", 
    "my",
    "yq",
    "qy", 
    "ymd", 
    "ydm", 
    "mdy", 
    "myd", 
    "dmy", 
    "dym" 
  )
  
  fns_priority <- c(
    5, 
    10,
    5,
    10, 
    3, 
    -1, 
    5, 
    1, 
    10, 
    -10 
  )
  
  
  tries <- 
    fns %>% 
    map_dfr(~{
      a <- suppressWarnings(.x(xw))
      u <- if(is.na(a)){
        as.Date(NA)
      }else{
        a
      }
      tibble(dt = u)
    })
  
  tries <- tries %>% 
    mutate(
      tag = fns_tag, 
      priority=fns_priority, 
      sq = seq(nrow(tries)))
  
  tries <- tries %>% 
    filter(!is.na(dt))
  
  if(nrow(tries)>0){
    tries %>% 
      arrange(desc(priority), sq) %>% 
      .[1,] %>% 
      pull(dt)
  }else{
    NULL
  }
  
}


str_word_seq_score <- function(x, y){
  x <- tolower(x)
  y <- tolower(y)
  xlcs <- x %>% map_chr(~LCS(.x, y))
  xnc <- nchar(x)
  xncp <- rep(1, length(xnc))
  xncp[xnc<10] <- 10-xnc[xnc<10]
  xlcsnc <- nchar(xlcs)
  xscore <- (xlcsnc+1)/(xnc+1)
  xscore^xncp
}



str_approx_which <- function(x, y, mix = FALSE){
  
  # x  is vector
  # y  is scalar
  
  mthds <- c(
    "osa", "lv", "dl", 
    "hamming", "lcs", 
    "qgram", "cosine", 
    "jaccard", "jw", 
    "soundex"
  )
  
  wts <- tibble(
    method = mthds,
    wts = 1
  )
  
  wts$wts[wts$method=="osa"] <- 10
  
  wts$wts[wts$method %in% c("jw","lcs")] <- 0.5
  
  val <- mthds %>% map_dfr(~{
    tibble(
      method = .x, 
      t = seq_along(x),
      vals = stringsim(x, y, q = 2, method = .x)
    )
  })
  
  # test it 
  # val %>% ggplot(aes(t, vals, col = method))+geom_line()
  
  
  sdis <- val %>% 
    left_join(wts, by = join_by(method)) %>% 
    group_by(t) %>% 
    summarise(sim = mean(vals*wts)/mean(wts)) %>% 
    arrange(t) %>% 
    pull(sim)
  
  if(mix){
    ws <- str_word_seq_score(x, y)
    
    ws*0.8+sdis*0.2
  }else{
    sdis
  }
  
}

# define common vars
str_init_env()