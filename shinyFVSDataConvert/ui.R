# $Id$

library(shiny)

shinyUI(fixedPage(
  tags$head(tags$style(HTML(".shiny-notification {height: 80px;width: 500px;
              position:fixed;top: calc(50% - 40px);;left: calc(50% - 250px);;}"
              ))),
  tags$style(type="text/css", ".progress-bar {color: transparent!important}"),
  tags$style(HTML(paste0(
    ".nav>li>a {padding:3px;}",
    ".btn {padding:2px 2px;color:darkred; background-color:#eef8ff;}",
    ".form-control {padding:2px 4px; height:auto;}",
    ".form-group {margin-bottom:4px}"
    ))),
  singleton(tags$head(tags$script(src = "message-handler.js"))),
  fixedRow(
    column(width=10,offset=0,
      HTML(paste0(
             '<h4><img src="FVSlogo.png" align="middle"</img>',
             '&nbsp;Sqlite3 database builder for input into FVS</h4>'))),
    column(width=2,uiOutput("serverDate"))
  ),
  fixedRow(
    tags$script('
       Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {   
           var el = $("#" + x);
           el.replaceWith(el = el.clone(true));
           var idƒ = "#" + x + "_progress";     
           $(id).css("visibility", "hidden");});'
      ),       
      tags$head(tags$script(HTML('
        Shiny.addCustomMessageHandler("jsCode",
          function(message) {eval(message.code);});'
          ))),
      h6(),
      fileInput("uploadNewDB",paste0("Upload FVS-Ready data ",
                "(.accdb, .mdb, .xlsx, or .zip that contains one of these)"),
                width="90%"),
      tags$style(type="text/css","#actionMsg{color:darkred;}"), 
      uiOutput("actionMsg"),
      h6(),
      downloadButton("dbDownload","Download the sqlite3 database")
  ))
)
