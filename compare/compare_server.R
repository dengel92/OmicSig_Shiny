textinput_id_root<-"signature"
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
`%-=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 - e2))

observeEvent(
  input$add, {
    insertUI(
      selector = "#add",
      where = "afterEnd",
      ui = textInput(paste0(textinput_id_root,input$add),"ree")
    )
  } 
)

observeEvent(
  input$subtract, {
    removeUI(selector=paste(".um .shiny-input-container:last",sep=""))
  } 
)