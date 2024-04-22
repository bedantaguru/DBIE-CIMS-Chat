

normalize <- function(x){
  if(length(unique(x))==1) return(rep(0.5, length(x)))
  (x-min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE))
}


enable_shadow_div <- function(){
  tags$head(
    tags$style(
      HTML(
        "
        .shadow-div {
          background-color: #ffffff;
          padding: 20px;
          border-radius: 10px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1), 0 1px 3px rgba(0, 0, 0, 0.08);
        }
        "
      )
    )
  )
}

shadow_div<- function(...){
  div(
    class = "shadow-div",
    ...
  )
}

