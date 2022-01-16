library(shiny)

shinyUI(fluidPage(
  tags$style(HTML(paste0(
    ".nav>li>a {padding:6px;}",
    ".btn {padding:4px 6px;color:darkred; background-color:#eef8ff;}",
    ".form-control {padding:2px 4px; height:auto;}",
    ".form-group {margin-bottom:6px}"))),
  verticalLayout(
    h2("Forest Vegetation Simulator (FVS-Online)"),
    h3("Hosted by Virginia Tech, College of Natural Resources and Environment",
      " / Department of Forest Resources and Environmental Conservation."),
    h5("Set up an FVSOnline project by filling out this form. ",
      "You will be sent an Email ",
      "with a link to this project.  Note that your project(s) will be ",
      "removed from this server 60 days after the last access."),
    p("Check your Email spam files if you don't get the Email promptly."),
    tags$style(type="text/css", "#title { width: 500px; }"),
    textInput("title", "Your new project title"),
    textInput("emailnew", "Your Email address"),
    actionButton("submitnew","Submit"),
    tags$style(type="text/css","#actionMsg{color:darkred;}"), 
    textOutput("actionMsg"),p(),
    div(style="height:1px;width:100%;background-color:lightgray;"),
    h5("Get a list of existing projects"),
    textInput("emailexist", "Your Email address", ""),
    actionButton("submitexist","Submit"),p(),
    div(style="height:1px;width:100%;background-color:lightgray;"),
    h4("Notice"),
    p("The software is provided “as is”, without warranty of any kind, express or ",
    "implied, including but not limited to the warranties of merchantability, ",
    "fitness for a particular purpose and noninfringement. In no event shall the ",
    "authors or copyright holders be liable for any claim, damages or other ",
    "liability, whether in an action of contract, tort or otherwise, arising ",
    "from, out of or in connection with the software or the use or other dealings ",
    "in the software."),
    p("This site is hosted by Virginia Tech, College of Natural Resources and Environment",
      " / Department of Forest Resources and Environmental Conservation ",
      "and is intended to be used for educational purposes. In no event will Virginia Tech ",
      "be liable for any claim, damages or other ",
      "liability, whether in an action of contract, tort or otherwise, arising ",
      "from, out of or in connection with the software or the use or other dealings ",
      "in the software.")
  )
))

