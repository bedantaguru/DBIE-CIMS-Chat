

library(shiny)
library(shinyjs)
library(tidyverse)
library(highcharter)
library(shinyChatR)
library(DBI)
library(RSQLite)

source("lib/shiny.R")
source("lib/query_api.R")
source("lib/predefined_task.R")
source("lib/etc.R")

user_tag <- "You"
chat_name_tag <- "DBIE"
db_file <- tempfile()
conn <- dbConnect(RSQLite::SQLite(), db_file)

# initiate chat table
df <- data.frame(rowid = as.numeric(Sys.time()),
                 user = chat_name_tag,
                 text = "Welcome to DBIE chat!",
                 time = substr(as.character(Sys.time()), 1, 19))
dbWriteTable(conn, "chat_data", df, overwrite = TRUE)
rm(df, db_file)

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(
    text = "shinyjs.closeWindow = function() { window.close(); }", 
    functions = c("closeWindow")
  ),
  enable_shadow_div(),
  titlePanel(h4("DBIE Chat"), windowTitle = "DBIE Chat"),
  chat_set_enter_as_send("main_chat"),
  verticalLayout(
    chat_ui("main_chat"),
    hr(),
    uiOutput("report")
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    my_chats = character(0), 
    tiggered_task = "none", 
    tiggered_vars = NULL)
  
  chat_server(
    "main_chat", 
    db_connection = conn,
    db_table_name = "chat_data",
    chat_user = user_tag,
    invalidateDSMillis = 10)
  
  observe({
    invalidateLater(10)
    chats <- dbReadTable(conn, "chat_data")
    rv$my_chats <- chats %>% filter(user==user_tag) %>% pull(text) %>% rev()
  })
  
  
  
  observe({
    req(rv$my_chats)
    
    chats <- dbReadTable(conn, "chat_data")
    
    if((chats$user %>% rev() %>% .[1])==user_tag){
      
      response <- qry_response(rv$my_chats[1])
      
      rr <- response
      
      if(rr$task=="clear"){
        
        if(rv$tiggered_task=="none"){
          rt <- "Nothing to clear!"
        }else{
          rt <- "Done!"
        }
        
        rv$tiggered_task <- "none"
        rv$tiggered_vars <- NULL
        
        rr <- list(
          task = "chat",
          msg = rt
        ) 
      }
      
      if(rr$task=="exit"){
        
        try(dbDisconnect(conn))
        js$closeWindow()
        stopApp()
         
      }
      
      if(rr$task=="plot"){
        
        rv$tiggered_task <- "plot"
        
        rv$tiggered_vars <- rr$vars
        
        rr <- list(
          task = "chat",
          msg = "Plotted!"
        ) 
      }
      
      
      if(rr$task=="data"){
        
        rv$tiggered_task <- "data"
        
        rv$tiggered_vars <- rr$vars
        
        rr <- list(
          task = "chat",
          msg = "Done!"
        ) 
      }
      
      if(rr$task=="analysis"){
        
        rv$tiggered_task <- "none"
        
        rv$tiggered_vars <- rr$vars
        
        rr <- list(
          task = "chat",
          msg = "I am yet to learn this!"
        ) 
      }
      
      
      
      if(rr$task == "chat" & is.character(rr$msg)){
        df <- data.frame(rowid = as.numeric(Sys.time()),
                         user = chat_name_tag,
                         text = rr$msg,
                         time = substr(as.character(Sys.time()), 1, 19))
        tryCatch(
          dbWriteTable(conn, "chat_data", df, append = TRUE), 
          error = function(e){
            NULL
          }
        )
      }
      
    }
  })
  
  
  output$report <- renderUI({
    
    switch(
      rv$tiggered_task,
      plot = shadow_div(highchartOutput("highchart_plot")),
      data = shadow_div(DTOutput("DT_out")),
      div()
    )
    
  })
  
  
  output$highchart_plot <- renderHighchart({
    if(rv$tiggered_task=="plot"){
      plot_vars(rv$tiggered_vars)
    }
  })
  
  output$DT_out <- renderDT({
    if(rv$tiggered_task=="data"){
      data_vis(rv$tiggered_vars)
    }
  })
  
  session$onSessionEnded(function() {
    try(
      suppressWarnings(
        dbDisconnect(conn)
      )
    )
    stopApp()
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
