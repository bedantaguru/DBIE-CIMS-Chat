
library(PTXQC)

source("lib/string.R")

source("lib/data_provider.R")

qry_persistence_info <- new.env() 

# sample
# qry <- "what was the food credit & IIP for march 18?"

qry_task_predictor <- function(vars, time, pre_task){
  if(length(vars)>0 & length(time)>0){
    return(
      list(
        type = "number_picker",
        vars = vars,
        time = time
      )
    )
  }
  
  if(
    (
      length(vars)==0 & 
      length(qry_persistence_info$selected_vars)>0
    ) & 
    length(time)>0
  ){
    return(
      list(
        type = "number_picker",
        vars = qry_persistence_info$selected_vars,
        time = time
      )
    )
  }
  
  
  if(
    (
      length(time)==0 & 
      length(qry_persistence_info$selected_time)>0
    ) &
    !(pre_task %in% c("plot","data","analysis")) &
    length(vars)>0
  ){
    return(
      list(
        type = "number_picker",
        vars = vars,
        time = qry_persistence_info$selected_time
      )
    )
  }
  
  
  if(
    (    
      length(vars)>0 | 
      length(qry_persistence_info$selected_vars)>0
    ) &  
    (pre_task %in% c("plot","data","analysis","clear"))
  ){
    if(length(vars)>0){
      v <- vars
    }else{
      v <- qry_persistence_info$selected_vars
    }
    
    return(
      list(
        type = pre_task,
        vars = v
      )
    )
  }
  
  
  list(type = "unknown")
}

qry_date_min_dist <- function(single_var, single_date){
  dp <- dat$main %>% filter(var_name == single_var)
  dp <- dp %>% 
    mutate(
      within_ft = (from_date <= single_date) & (to_date >= single_date),
      date_dist = (abs(from_date - single_date) + abs(to_date - single_date))/2
    )
  ads <- as.numeric(dp$date %>% diff() %>% mean())+0.000001
  dp <-  dp %>% mutate(dist_date = ifelse(within_ft, 0, as.numeric(date_dist/ads)))
  
  dp_near <- dp %>% filter(dist_date == min(dist_date)) %>% select(-date_dist)
  dp_near
}

qry_model <- function(qry){
  qc <- str_curate(qry)
  
  # adding variable to query
  to_be_added <- FALSE
  if(str_detect(qc,"add|also|append") | str_detect(qry,"\\+")){
    if(length(qry_persistence_info$selected_vars)>0){
      to_be_added <- TRUE
    }
  }
  
  qwds <- qc %>% str_split(" ") %>% unlist()
  qsw <- qwds %>% intersect(str_env$stop_words_qry)
  qwds_core <- qwds %>% setdiff(qsw)
  qwds_time <- qwds_core %>% intersect(str_env$wds_time)
  time_wds <- which(qwds_core==qwds_time[1]) %>% 
    map(~.x+c(-1:1)) %>% 
    unlist()
  time_wds <- time_wds[time_wds>0 & time_wds <= length(qwds_core)]
  time_wds <- qwds_core[time_wds]
  rest_wds <- qwds_core %>% setdiff(qwds_time)
  rest_wds <- rest_wds[!str_detect(rest_wds, "^[0-9]+")]
  
  ptaskl <- qry_predefined_tasks(paste0(rest_wds, collapse = " "))
  ptask <- ptaskl$task
  
  pt <- rest_wds %>% str_detect(ptaskl$tags)
  
  # remove task words 
  rest_wds <- rest_wds[!pt]
  
  # attach Abbreviation List (depending upon needs/or always? - doing now always)
  rest_wds_expand1 <- rest_wds %>% 
    c(
      dat$abbr %>% filter(abb_name %in% rest_wds) %>% pull(ff_name)
    ) %>% 
    unique()
  
  rest_wds_expand2 <- rest_wds_expand1 %>% 
    c(
      dat$vars %>% filter(var_short_name %in% rest_wds) %>% pull(var_name)
    ) %>% 
    unique()
  
  rw <- rest_wds_expand2 %>% paste0(collapse = " ")
  
  q_vars_list <- tibble(
    vars = dat$vars$var_name) %>% 
    mutate(
      prob = str_approx_which(vars, rw),
      prob_lcs = str_word_seq_score(vars, rw),
      prob_mix = prob*0.2+prob_lcs*0.8
    )
  
  q_vars_list_sel <- q_vars_list
  
  # exact match 
  if((q_vars_list %>% filter(prob_lcs>0.9) %>% nrow())>0){
    q_vars_list_sel <- q_vars_list %>% filter(prob_lcs>0.9)
  }else{
    if((q_vars_list %>% filter(prob_mix>0.7) %>% nrow())>0){
      q_vars_list_sel <- q_vars_list %>% filter(prob_mix>0.7)
    }else{
      suppressWarnings(
        q_vars_list_sel <- q_vars_list %>% 
          filter(prob_mix>0.5) %>% 
          filter(prob_mix==max(prob_mix))
      )
      
    }
  }
  
  ask_time <- str_time_parse(time_wds %>% paste0(collapse = " "))
  
  ask_time <- as.Date(ask_time)
  
  # it is handled later
  # ask_time_core <- ask_time[ask_time <= max(dat$dates) & ask_time >= min(dat$dates)]
  
  vars_this <- q_vars_list_sel$vars
  
  if(to_be_added){
    vars_this <- vars_this %>% c(qry_persistence_info$selected_vars) %>% unique()
    vars_this <- vars_this %>% intersect(dat$vars$var_name)
  }
  
  what_task <- qry_task_predictor(
    vars = vars_this, 
    time = ask_time, 
    pre_task = ptask
  )
  
  if(length(vars_this)>0){
    qry_persistence_info$selected_vars <- vars_this
  }
  
  if(length(ask_time)>0){
    qry_persistence_info$selected_time <- ask_time
  }
  
  # task - 1
  # number picker (i,j)
  switch(
    what_task$type,
    number_picker = qry_number_picker(
      vars = what_task$vars, 
      time = what_task$time),
    plot = qry_pass_var(
      task = what_task$type,
      vars = what_task$vars
    ),
    data = qry_pass_var(
      task = what_task$type,
      vars = what_task$vars
    ),
    analysis = qry_pass_var(
      task = what_task$type,
      vars = what_task$vars
    ),
    clear = qry_pass_var(
      task = what_task$type,
      vars = what_task$vars
    ),
    exit = qry_pass_var(
      task = what_task$type,
      vars = what_task$vars
    ),
    list()
  )
  
}

# task specific qry models
qry_number_picker <- function(vars, time){
  
  near_dats <- vars %>% 
    map_dfr(~{
      v <- .x
      time %>% 
        map_dfr(~qry_date_min_dist(v, .x) %>% 
                  mutate(ask_date = .x))
    })
  
  near_dats <- near_dats %>% 
    mutate(
      time_tag = case_when(
        freq=="Weekly"~paste0("week ended on ", to_date), 
        freq=="Fortnightly"~paste0("fortnight ended on ", to_date),
        freq=="Monthly"~paste0(month(date, label = T),"-", year(date)),
        freq=="Quarterly"~paste0(
          month(from_date, label = T),"-",
          month(to_date, label = T)," ", year(date)),
        .default = ""
      )
    )
  
  near_dats0 <- near_dats %>% filter(within_ft)
  
  near_dats1 <- near_dats %>% filter(!within_ft)
  
  near_dats11 <- near_dats1 %>% filter(dist_date < 2)
  
  sel_vars_nice_names <- dat$main %>% 
    filter(var_name %in% vars) %>% 
    distinct(var_without_unit) %>% 
    pull() 
  
  # prepare massages 
  msg <- character(0)
  
  if(nrow(near_dats0)>0){
    
    gd <-  near_dats0 %>% 
      select(date, freq, var = var_without_unit, value, unit)
    
    qry_persistence_info$selected_data <- gd
    
    msg <- c(
      msg,
      near_dats0 %>% 
        arrange(var_without_unit, date) %>%  
        mutate(
          txt = paste0(
            var_without_unit," for ", time_tag," is ", 
            round(value,2), " ", unit)) %>% 
        pull(txt) %>% 
        paste0(collapse = "\n")
    )
    
    if(nrow(near_dats11)>0){
      msg <- c(
        msg,
        "\n\nNote: It's possible that the data points you requested are beyond what I currently have or can access. However, since I've already found some matching data, I haven't displayed those figures. If you need data for the specific dates that weren't retrieved, please feel free to ask again."
      )
    }
    
    
  }else{
    msg <- c(
      msg, 
      paste0(
        "Apologies! I couldn't find any data for the requested dates regarding:", 
        paste0(sel_vars_nice_names, collapse = ", ")
      )
    )
    
    
    if(nrow(near_dats11)>0){
      msg <- c(
        msg, 
        "While I've found a few data points that may be of interest, please note that they appear to be outside of my current data availability. Please use this information judiciously. Here's what I found:",
        near_dats11 %>% 
          arrange(var_without_unit, date) %>%  
          mutate(
            txt = paste0(
              var_without_unit," for ", time_tag," is ", 
              round(value,2), " ", unit)) %>% 
          pull(txt) %>% 
          paste0(collapse = "\n")
      )
    }else{
      if(nrow(near_dats1)>0){
        msg <- c(
          msg,
          "\n\n Note: It's possible that you've requested data points that fall beyond my current knowledge or data availability. Please note that the available data for the queried variables is as follows:",
          dat$main %>% 
            filter(var_name %in% vars) %>% 
            group_by(var_name, freq, var_without_unit) %>% 
            summarise(
              from_date = min(from_date), 
              to_date = max(to_date), .groups = "drop") %>% 
            mutate(
              time_tag_duration = 
                case_when(
                  freq=="Weekly"~paste0(
                    "week ended on ", from_date, " to ", to_date), 
                  freq=="Fortnightly"~paste0(
                    "fortnight ended on ", from_date, " to ", to_date),
                  freq=="Monthly"~paste0(
                    month(from_date, label = T),"-", year(from_date), " to ", 
                    month(to_date, label = T),"-", year(to_date)
                  ),
                  freq=="Quarterly"~paste0(
                    quarter(from_date, with_year = TRUE)," to ",
                    quarter(to_date, with_year = TRUE)
                  ),
                  .default = ""
                )
            ) %>% 
            arrange(var_without_unit, from_date) %>% 
            mutate(
              txt = paste0(
                var_without_unit," is available: ", time_tag_duration)
            ) %>% 
            pull(txt) %>% 
            paste0(collapse = "\n")
          
        )
      }
    }
    
  }
  
  list(
    task = "chat",
    msg = paste0(msg, collapse = "\n")
  )
  
}

qry_pass_var <- function(task, vars){
  list(
    task = task,
    vars = vars
  )
}

qry_predefined_tasks <- function(qry){
  
  l_tags  <- list(
    clear = "clear|clean|erase|remove|hide",
    plot = "plot|chart|graph|visual|diagram",
    data = "show|display|data|download|series",
    analysis = "analy|stat|summary",
    exit = "bye|exit|close|terminate"
  )
  
  if(missing(qry)){
    return(l_tags)
  }
  
  qry <- str_curate(qry)
  task = "unknown"
  
  
  for(t_name in names(l_tags)){
    if(task=="unknown" & str_detect(qry, l_tags[[t_name]])){
      task = t_name
    }
    if(task!="unknown") break()
  }
  
  
  # c_tag <- "clear|clean|erase|remove|hide"
  
  if(task=="unknown" & str_detect(qry, l_tags$clear)){
    task = "clear"
  }
  
  # p_tag <- "plot|chart|graph|visual|diagram"
  
  if(task=="unknown" & str_detect(qry, l_tags$plot)){
    task = "plot"
  }
  
  # d_tag <- "show|display|data|download|series"
  
  if(task=="unknown" & str_detect(qry,l_tags$data)){
    task = "data"
  }
  
  a_tag <- "analy|stat|summary"
  
  if(task=="unknown" & str_detect(qry,a_tag)){
    task = "analysis"
  }
  
  e_tag <- "bye|exit|close|terminate"
  
  if(task=="unknown" & str_detect(qry,e_tag)){
    task = "exit"
  }
  
  
  list(
    task = task,
    tags = paste0(c(c_tag, p_tag,d_tag,a_tag, e_tag), collapse = "|")
  )
}

qry_facts <- function(qry){
  qry <- str_curate(qry)
  
  if(qry %>% str_detect("all var")){
    return(
      list(
        task = "chat",
        msg = unique(dat$main$var_without_unit)
      )
    )
  }
  
  if(qry %>% str_detect("date range")){
    return(
      list(
        task = "chat",
        msg = dat$main$date %>% range() %>% paste0(collapse = " to ")
      )
    )
  }
  
  if(str_detect(qry,"hi|hello") & nchar(qry) < 10){
    
    greetings <- c(
      "Hi there!",
      "Hello!",
      "Hey!",
      "Hi, how can I assist you?",
      "Hello! What can I do for you today?",
      "Hi! How's it going?",
      "Hello! Need any help?",
      "Hi! What can I help you with?",
      "Hey there! How can I be of service?",
      "Hello! Welcome! How can I assist you?",
      "Hi!",
      "Hello! How are you doing?",
      "Hey! How can I help you today?",
      "Hi there! Need assistance?",
      "Hello! What brings you here?",
      "Hey! How's your day going?",
      "Hi! How can I support you?",
      "Hello! Need anything?",
      "Hey there! How may I assist you?",
      "Hi! Welcome! How can I help?",
      "Hello! What's up?",
      "Hey! What can I do for you?",
      "Hi! How can I be of service?",
      "Hello! How may I assist you today?",
      "Hey there! Need any assistance?",
      "Hi! How are you today?",
      "Hello! How's everything?",
      "Hey! How's your day?",
      "Hi! How can I assist you today?",
      "Hello! How may I help you?",
      "Hey! How can I assist you?",
      "Hi! How's it going?",
      "Hello! Need any support?",
      "Hey! How can I assist you today?",
      "Hi! How's your day going?",
      "Hello! How can I help you today?",
      "Hey! What's up?",
      "Hi! Need any assistance?",
      "Hello! How can I support you?",
      "Hey! How may I assist you?",
      "Hi there! How can I help?",
      "Hello! How can I assist you today?",
      "Hey there! How may I help you?",
      "Hi! Need any help?",
      "Hello! How can I support you today?",
      "Hey! How can I assist you now?",
      "Hi! How may I assist you?",
      "Hello! What can I do for you?",
      "Hey! How can I help today?",
      "Hi! How's everything going?",
      "Hello! Need any assistance today?",
      "Hey! How can I assist you now?"
    )
    
    set.seed(Sys.time())
    
    return(
      list(
        task = "chat",
        msg = sample(greetings)[1]
      )
    )
  }
  
  # fall back situation 
  list(
    task="forward"
  )
  
}

qry_response <- function(qry){
  
  template_msgs <- c(
    "Apologies, I didn't understand it. My current skillset is limited. Could you please try asking again in a different way?",
    "Sorry, I didn't grasp that. My abilities are somewhat restricted at the moment. Could you rephrase your question or provide more context?",
    "I'm sorry, I didn't quite catch that. At the moment, my capabilities are somewhat limited. Could you please try rephrasing your question or request?",
    "Apologies, I didn't catch that. Currently, I'm capable of searching values for various time periods of specific variables I'm trained on. I can attempt to plot one or more variables, download several series for further analysis, and perform basic statistics on certain series.",
    "Apologies for missing that. Currently, I can search values for various time periods of specific variables, plot variables, download series for further analysis, and perform basic statistics on them.",
    "Regrettably, I didn't grasp your message. Presently, I can search for values over different time periods for specific variables, generate plots, download series for further analysis, and perform basic statistical analysis on them.",
    "Apologies, I missed that. At the moment, I'm able to search for values across various time periods for specific variables, produce plots, download series for further analysis, and conduct basic statistical analysis.",
    "My apologies for not understanding your request. Currently, I can search for values over different time periods for specific variables, create plots, download series for further analysis, and perform basic statistical analysis.",
    "Sorry, I didn't catch that. Right now, I'm capable of searching for values across various time periods for specific variables, plotting them, downloading series for further analysis, and conducting basic statistical analysis.",
    "Apologies, I didn't grasp your message. Presently, I'm equipped to search for values over different time periods for specific variables, generate plots, download series for further analysis, and conduct basic statistical analysis.",
    "My apologies for not understanding your message. Currently, I can search for values over different time periods for specific variables, plot them, download series for further analysis, and perform basic statistical analysis.",
    "Sorry, I missed that. At the moment, I'm able to search for values across different time periods for specific variables, create plots, download series for further analysis, and conduct basic statistical analysis.",
    "Apologies, I didn't catch that. Presently, I can search for values over various time periods for specific variables, produce plots, download series for further analysis, and perform basic statistical analysis.",
    "My apologies for not understanding your request. Currently, I'm equipped to search for values over different time periods for specific variables, generate plots, download series for further analysis, and conduct basic statistical analysis.",
    "Sorry, I didn't grasp that. Right now, I'm capable of searching for values across different time periods for specific variables, plotting them, downloading series for further analysis, and conducting basic statistical analysis."
  )
  
  
  template_error_msgs <- c(
    "Something went wrong. Please try again with a different query.",
    "An issue has occurred. Please attempt your query again with a different one.",
    "Unfortunately, there was an error. Please try a different query.",
    "We encountered a problem. Please try again with a different query.",
    "Oops! Something didn't go as planned. Please try a different query.",
    "Uh-oh! It seems there was an error. Please try again with a different query.",
    "Sorry, something went awry. Please try a different query.",
    "Regrettably, there was an error. Please try again with a different query.",
    "Apologies, we hit a snag. Please try a different query.",
    "It looks like there's been an error. Please try again with a different query."
  )
  
  set.seed(Sys.time())
  
  lob <- list(
    task = "chat",
    msg = sample(template_msgs)[1]
  )
  
  lo <- tryCatch(
    {
      ut <- qry_facts(qry)
      if(ut$task=="forward"){
        ut <- qry_model(qry)
      }
      ut
    }, 
    error = function(e){
      list(
        task = "chat",
        msg = sample(template_error_msgs)[1]
      )
    }
  )
  
  if(is.null(lo$task)){
    lo <- lob
  }
  
  # convert into multi-chat for new lines
  if(lo$task=="chat"){
    if(is.null(lo$msg)){
      lo <-lob
    }
    
    lo$msg <- lo$msg %>% 
      str_split("\n") %>% 
      unlist() %>% 
      str_replace_all(" +"," ") %>% 
      str_trim()
    
    lo$msg <- lo$msg[nchar(lo$msg)>0] 
    
  }
  
  lo
}


