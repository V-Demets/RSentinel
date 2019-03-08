#' @title cnes_gui
#'
#' @param param_list
#'
#' @export
cnes_gui <- function(param_list = NULL) {
  .cnes_gui(
    param_list = param_list,
    par_fun = "parent"
  )
}

# Internal function with parameter par_fun (parent function):
# default is "parent", but it can be set with another value
# for internal purposes.
# For now, value "cnes" is used when cnes_gui() is launched from
# cnes(), and is used to change save button label.
.cnes_gui <- function(param_list = NULL,
                      par_fun = "parent") {
  cnes_gui.shiny <- shinyApp(
    ui = cnes_gui.ui,
    server = cnes_gui.server
  )

  # run
  if (interactive()) {
    options(device.ask.default = FALSE)
    return(runApp(cnes_gui.shiny))
  } else {
    stop(i18n$t("The function must be run from an interactive R session."))
  }
}
