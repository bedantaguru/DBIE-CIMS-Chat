

library(shiny)


chat_set_enter_as_send <- function(id){
  js <- paste0(
    '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    document.getElementById("',id,'-chatFromSend").click();
  }
});
'
  )
  tags$script(js)
}

