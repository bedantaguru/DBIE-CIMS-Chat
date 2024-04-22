
library(highcharter)
library(DT)
library(tidyr)

source("lib/data_provider.R")
source("lib/etc.R")

plot_vars <- function(vars, config, enable_export = FALSE){
  
  if(missing(config)){
    config <- plot_option_suggest(vars)
  }
  
  this_dat <- dat$main %>% 
    filter(var_name %in% vars)
  
  if(config$normalize){
    
    this_dat_p <- this_dat %>%
      group_by(var_name, unit) %>%
      mutate(val_p = normalize(value)) %>%
      ungroup() %>%
      mutate(date_p = from_date+(to_date-from_date)/2) %>%
      select(date_p, val_p, value, var_name, var_without_unit, unit)
    
  }else{
    
    this_dat_p <- this_dat %>%
      group_by(var_name, unit) %>%
      mutate(val_p = value) %>%
      ungroup() %>%
      mutate(date_p = from_date+(to_date-from_date)/2) %>%
      select(date_p, val_p, value, var_name, var_without_unit, unit)
    
  }
  
  
  # chart start 
  
  hc <- highchart(type = "stock") %>%
    hc_title(text = "Time Series") %>% 
    hc_rangeSelector(enabled = FALSE) %>% 
    hc_legend(enabled = TRUE) %>% 
    hc_xAxis(gridLineWidth = 1)
  
  if(enable_export){
    hc <- hc %>% hc_exporting(enabled = TRUE)
  }
  
  
  if(config$normalize){
    hc <- hc %>% 
      # this are for normalize
      hc_tooltip(pointFormat = "{point.var_without_unit}:{point.value}") %>% 
      hc_yAxis(
        # this are for normalize
        labels = list(enabled = FALSE),
        alternateGridColor = "#FAFAFA",
        minorTickInterval = "auto",
        minorGridLineDashStyle = "LongDashDotDot"
      ) 
  }else{
    hc <- hc %>% 
      hc_yAxis(
        alternateGridColor = "#FAFAFA",
        minorTickInterval = "auto",
        minorGridLineDashStyle = "LongDashDotDot"
      ) 
  }
  
  if(config$dual_axis){
    create_axis(naxis = 2)->x
    
    x[[1]]$height <- "100%"
    x[[2]]$height <- "100%"
    x[[1]]$top <- "0%"
    x[[2]]$top <- "0%"
    
    hc <- hc %>% 
      hc_yAxis_multiples(x)
    
  }
  
  
  for(v in vars){
    dt <- this_dat_p %>% filter(var_name == v)
    
    yax <- 0
    s_name <- dt$var_without_unit[1]
    
    if(config$dual_axis){
      if(v %in% config$axis_1){
        yax <- 1
        s_name <- paste0(s_name, " (RH)")
      }else{
        s_name <- paste0(s_name, " (LH)")
      }
    }
    
    hc <- hc %>% 
      hc_add_series(
        data = dt %>% select(date_p, val_p, value, var_without_unit), 
        type = "line", 
        hcaes(x = date_p, y = val_p), 
        name = s_name,
        yAxis = yax
      )
  }
  
  hc
  
}

plot_option_suggest <- function(vars){
  this_dat <- dat$main %>% 
    filter(var_name %in% vars)
  dunits <- this_dat %>% 
    distinct(unit) %>% pull()
  
  naxis <- 1
  
  chk <- this_dat %>% 
    group_by(var_name, unit) %>% 
    summarise(mV = min(value),MV = max(value), .groups = "drop") %>% 
    group_by(unit) %>% 
    mutate(
      mVD = (abs(mV)+0.001)/(min(abs(mV))+0.001), 
      MVD = (abs(MV)+0.001)/(min(abs(MV))+0.001)
    ) %>% 
    ungroup()
  
  chk2 <- chk %>% 
    group_by(unit) %>% 
    summarise(sds = (sd(mVD)+sd(MVD))/2, nv = n())
  chk2$sds[is.na(chk2$sds)] <- 0
  
  chk2 <- chk2 %>% mutate(n_axis = ifelse(sds<2, 1, nv))
  
  chk2$n_axis[is.na(chk2$n_axis)] <- 1
  
  naxis <- sum(chk2$n_axis)
  
  config <- list()
  
  if(naxis==1){
    # simple case and no complexity 
    config <- list(
      n_axis = 1,
      dual_axis = FALSE,
      normalize =  FALSE,
      axis_0 = vars
    )
  }
  
  if(naxis==2){
    # either single unit two series
    # or two unit and each are in similar range
    if(length(dunits)==1){
      config <- list(
        n_axis = 2,
        dual_axis = TRUE,
        normalize =  FALSE,
        axis_0 = vars[1],
        axis_1 = vars[-1]
      )
    }else{
      ll <- chk %>% split(.$unit)
      config <- list(
        n_axis = 2,
        dual_axis = TRUE,
        normalize =  FALSE,
        axis_0 = ll[[1]]$var_name,
        axis_1 = vars %>% setdiff(ll[[1]]$var_name)
      )
    }
    
  }
  
  if(naxis>2){
    # simple case and no complexity 
    # only values are normalized
    config <- list(
      n_axis = 1,
      dual_axis = FALSE,
      normalize =  TRUE,
      axis_0 = vars
    )
  }
  
  config
  
}


data_vis <- function(vars){
  this_dat <- dat$main %>% 
    filter(var_name %in% vars) %>% 
    select(
      Date = date, 
      Frequency = freq, 
      Variable = var_without_unit, 
      Value = value, 
      Unit = unit
    ) %>% 
    mutate(Value = round(Value, 2))
  
  nuf <- this_dat %>% distinct(Frequency, Unit) %>% nrow()
  
  if(nuf==1){
    td <- this_dat %>% 
      select(Date, Variable, Value) %>% 
      pivot_wider(id_cols = Date, names_from = Variable, values_from = Value)
  }else{
    td <- this_dat
  }
  
  td %>% 
    dt_proto()
}







