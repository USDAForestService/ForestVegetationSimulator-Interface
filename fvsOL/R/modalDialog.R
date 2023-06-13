# code taken form shiny-confirm-dialog, Wei Cheng, with thanks!
modalDialog <- function(id, header = "Confirmation", body = "Confirm action", 
  footer = list(actionButton("confirmDlgOkBtn", "OK")))
{
 div(id = id, class = "modal fade",
    div(class = "modal-dialog",
      div(class = "modal-content",
        div(class = "modal-header",
          tags$button(type = "button", class = "close", 
            'data-dismiss' = "modal", 'aria-hidden' = "true", HTML('&times;')),
          tags$h4(class = "modal-title", header)),
        div(class = "modal-body", tags$p(body)),
        div(class = "modal-footer", tagList(footer)))
  ))
}

# code taken form shiny-confirm-dialog, Wei Cheng, with thanks!
modalTriggerButton <- function(inputId, target, label)
{
  tags$button(id = inputId, type = "button", 
    class = "btn action-button btn-primary", 'data-toggle' = "modal", 
    'data-target' = target, label)
}