#' @title Insert an extent
#' @description Internal functions and modal dialogs to specify an extent
#'  in the GUI.
#' @author Pascal Obstetar (2018) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' @importFrom shiny actionButton div fileInput htmlOutput icon modalButton modalDialog
#'  numericInput span tagList textInput fluidRow column strong div numericInput
#'
#' @name load_extent_bbox
#' @rdname load_extent
#' @export
load_extent_bbox <- function() {
  modalDialog(
    title = i18n$t("Specify a bounding box"),
    size = "l",
    fluidRow(
      column(
        width = 4,
        strong(i18n$t("Insert the coordinates of the bounding box:\n(Burgoundy : 46.8, 3.8, 5.8, 48.1)")),
        div(
          style = "width:200px;position:relative;margin-left:40px;top:0px;",
          numericInput(
            "bbox_ymax",
            label = span(style = "font-weight:normal;color:grey", "upper northing"),
            value = NULL, width = "110px"
          )
        ),
        div(
          style = "width:200px;",
          div(
            style = "display:inline-block;position:relative;top:-5px;",
            numericInput(
              "bbox_xmin",
              label = span(style = "font-weight:normal;color:grey", "left easting"),
              value = NULL, width = "90px"
            )
          ),
          div(
            style = "display:inline-block;position:relative;margin-left:10px;top:-5px;",
            numericInput(
              "bbox_xmax",
              label = span(style = "font-weight:normal;color:grey", "right easting"),
              value = NULL, width = "90px"
            )
          )
        ),
        div(
          style = "width:200px;position:relative;margin-left:40px;top:-10px;",
          numericInput(
            "bbox_ymin",
            label = span(style = "font-weight:normal;color:grey", "lower northing"),
            value = NULL,
            width = "110px"
          )
        ),
        strong(i18n$t("Specify the projection of the coordinates:")),
        div(
          div(
            style = "display:inline-block;position:relative;",
            textInput("bboxproj", NULL,
              value = "4326", width = "190px"
            )
          ),
          div(
            style = "display:inline-block;position:relative;bottom:0;margin-left:10px;",
            htmlOutput("bboxproj_message")
          )
        )
      ),
      column(
        width = 8,
        leafletOutput("view_map_bbox", height = 400, width = "100%")
      )
    ),
    easyClose = FALSE,
    footer = tagList(
      actionButton("save_extent_bbox", strong(i18n$t("\u2000Ok")), icon = icon("check")),
      modalButton(i18n$t("\u2000Cancel"), icon = icon("ban"))
    )
  )
}

#' @name load_extent_vectfile
#' @rdname load_extent
#' @export
load_extent_vectfile <- function() {
  modalDialog(
    title = i18n$t("Select vector file"),
    size = "m",
    helpText(em(
      p(i18n$t("Chose the vector file to be used as extent.")),
      p(
        i18n$t("To upload a shapefile, select all the related files"),
        i18n$t("(at most the .shp, .shx, .dbf and .prj ones must be present).")
      )
    )),
    fileInput("path_vectfile_sel",
      i18n$t("Select"),
      multiple = TRUE
    ),
    div(
      style = "display:inline-block;vertical-align:top;",
      htmlOutput("path_vectfile_errormess")
    ),
    leafletOutput("view_map_vectfile", height = 400, width = "100%"),
    easyClose = FALSE,
    footer = tagList(
      actionButton("save_extent_vectfile", strong(i18n$t("\u2000Ok")), icon = icon("check")),
      modalButton(i18n$t("\u2000Cancel"), icon = icon("ban"))
    )
  )
}


#' @name load_extent_draw
#' @rdname load_extent
#' @param extent_ns_name Name of the namespace to be used
#' @export
load_extent_draw <- function(extent_ns_name) {
  modalDialog(
    title = i18n$t("Draw the extent"),
    size = "l",
    helpText(em(i18n$t("Use the tools on the left to draw the extent of your calc."))),
    editModUI(extent_ns_name, height = 500, width = "100%"),
    easyClose = FALSE,
    footer = tagList(
      actionButton("save_extent_draw", strong(i18n$t("\u2000Ok")), icon = icon("check")),
      modalButton(i18n$t("\u2000Cancel"), icon = icon("ban"))
    )
  )
}

#' @name load_extent_forest
#' @rdname load_extent
#' @export
load_extent_forest <- function() {
  modalDialog(
    title = i18n$t("Select forest"),
    size = "m",
    helpText(em(i18n$t("Choose the forest to be used as extent."))),
    selectInput("forest", i18n$t("Forest :"), choices = c(Choisir = "", paste0(forestdata$ccod_cact, "-", forestdata$ccod_frt))),
    leafletOutput("view_map_forest", height = 400, width = "100%"),
    easyClose = FALSE,
    footer = tagList(
      actionButton("save_extent_forest", strong(i18n$t("\u2000Ok")), icon = icon("check")),
      modalButton(i18n$t("\u2000Cancel"), icon = icon("ban"))
    )
  )
}
