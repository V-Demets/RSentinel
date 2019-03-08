## server
#' Shiny server
cnes_gui.server <- function(input, output, session) {

  # extract and import tiles kml
  s2tiles <- s2_tiles()

  # link to www directory and objects
  addResourcePath("www", system.file("www", package = "shinycnes"))
  output$img_logo <- renderUI(
    img(src = "www/img/logo_v2.png", height = "80", width = "200")
  )

  # initialise rv
  # (list of reactive values to be passed as output)
  rv <- reactiveValues()

  # get server volumes
  volumes <- c("Home" = path.expand("~"), getVolumes()())

  # initialise buffer
  buffer <- 10

  #-- Function to update the map and the list of tiles --#
  # it returns TRUE if the input extent source was correctly read, FALSE elsewhere.
  # argument extent_source determines which source to be used:
  # "bbox", "vectfile", "draw" from selection buttons, "imported" from parameter;
  # in this case, the argument "custom_source" is the source to be passed.
  update_extent <- function(extent_source, custom_source = NA) {

    # 1. Define rv$extent
    if (extent_source == "forest") {
      # Forest mode #
      # check that the polygon is valid
      if (attr(rv$forest_polygon, "valid")) {
        rv$extent <- rv$forest_polygon
        attr(rv$extent, "new") <- TRUE
      } else {
        return(FALSE)
      }
    } else if (extent_source == "bbox") {
      # Bbox mode #
      # check that the polygon is valid
      if (attr(rv$bbox_polygon, "valid")) {
        rv$extent <- rv$bbox_polygon
        attr(rv$extent, "new") <- TRUE
      } else {
        return(FALSE)
      }
    } else if (extent_source == "vectfile") {
      # Vectfile mode #
      # check that the polygon is valid
      if (attr(rv$vectfile_polygon, "valid")) {
        rv$extent <- rv$vectfile_polygon
        attr(rv$extent, "new") <- TRUE
      } else {
        return(FALSE)
      }
    } else if (extent_source == "draw") {
      # Drawn mode #
      # namespace for extent selection
      sel_drawn <- if (!is.null(rv$extent_edits()$finished)) {
        x <- rv$extent_edits()$finished
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      } else {
        x <- st_polygon()
        attr(x, "valid") <- FALSE
        x
      }
      if (!attr(sel_drawn, "valid")) {
        return(FALSE)
      }
      rv$extent <- sel_drawn
    } else if (extent_source == "imported") {
      # Imported from parameters #
      sel_imported_extent <- if (is.null(custom_source) | anyNA(custom_source)) {
        x <- st_polygon()
        attr(x, "valid") <- FALSE
        x
      } else {
        x <- st_read(custom_source, quiet = TRUE) %>%
          st_transform(4326)
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      }
      if (!attr(sel_imported_extent, "valid")) {
        return(FALSE)
      }
      rv$extent <- sel_imported_extent
    } else {
      # For any other value of extent_source, use the existing rv$extent
      if (is.null(rv$extent)) {
        return(FALSE)
      } else if (!attr(rv$extent, "valid")) {
        return(FALSE)
      } else {
        attr(rv$extent, "new") <- FALSE
      }
    }

    # 2. Update the list of overlapping tiles and the tiles on the map
    if (length(rv$extent) > 0) {
      rv$draw_tiles_overlapping <- s2tiles[unique(unlist(suppressMessages(st_intersects(st_transform(rv$extent, 4326), s2tiles)))), ]

      if (attr(rv$extent, "new")) {
        # update the list of tiles
        updateCheckboxGroupInput(
          session, "tiles_checkbox",
          choiceNames = lapply(rv$draw_tiles_overlapping$tile_id, span, style = "family:monospace;"),
          choiceValues = rv$draw_tiles_overlapping$tile_id,
          selected = rv$draw_tiles_overlapping$tile_id,
          inline = nrow(rv$draw_tiles_overlapping) > 8 # inline if they are many
        )
      }

      # reset and update the map
      react_map(base_map())
      rv$draw_tiles_overlapping_ll <- st_transform(rv$draw_tiles_overlapping, 4326)
      rv$extent_ll <- st_transform(rv$extent, 4326)
      leafletProxy("view_map") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[, "X"]),
          lat1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[, "Y"]),
          lng2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[, "X"]),
          lat2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[, "Y"])
        ) %>%
        addPolygons(
          data = rv$draw_tiles_overlapping,
          group = "S2 tiles",
          label = ~tile_id,
          labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "orange",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "red"
        ) %>%
        # add extent
        addPolygons(
          data = rv$extent_ll,
          group = "Extent",
          # label = ~ccod_frt,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "blue",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        ) # %>%
    } else {
      rv$draw_tiles_overlapping <- NULL
      # empty the list of tiles
      updateCheckboxGroupInput(session, "tiles_checkbox",
        choices = NULL
      )
      # reset the map
      react_map(base_map())
    }
    return(TRUE)
  }

  #-- Create the map (once) --#
  base_map <- function() {
    leaflet() %>%
      # add tiles
      addTiles(group = "OpenStreetMap") %>%
      # addTiles(paste0("https://{s}.tile.thunderforest.com/outdoors/{z}/{x}/{y}.png",
      #                 if (!is.na(thunderforest_api)) {paste0("?apikey=",thunderforest_api)}),
      #          group = "OpenStreetMap Outdoors") %>%
      # addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addTiles("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
        group = "OpenTopoMap"
      ) %>%
      # addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
        group = "CartoDB"
      ) %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        group = "Satellite"
      ) %>%
      # addProviderTiles(providers$CartoDB.PositronOnlyLabels, group = "Dark names") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_only_labels/{z}/{x}/{y}.png",
        group = "Light names"
      ) %>%
      # addProviderTiles(providers$CartoDB.DarkMatterOnlyLabels, group = "Dark names") %>%
      addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_only_labels/{z}/{x}/{y}.png",
        group = "Dark names"
      ) %>%
      # view and controls
      addLayersControl(
        baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
        overlayGroups = c("Light names", "Dark names", "Extent", "S2 tiles"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Light names", "Dark names"))
  }

  # create a new map for principal view
  react_map <- reactiveVal({
    base_map()
  })
  output$view_map <- renderLeaflet({
    react_map()
  })
  
  # create a new map for prevision view
  react_map_prevision <- reactiveVal(base_map())
  output$view_map_prevision <- renderLeaflet({
    react_map_prevision()
  })

  ############### - Forest mode -####################

  # create a new map (to be shown in modal dialog)
  react_map_forest <- reactiveVal(base_map())
  output$view_map_forest <- renderLeaflet({
    react_map_forest()
  })

  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_forest, {
    showModal(load_extent_forest())
  })

  # load the forest on the map
  observeEvent(input$forest, {
    # Check that the forest is valid
    frt <- str_sub(input$forest, 6)
    agc <- str_sub(input$forest, 1, 4)
    rv$forest_polygon <- if (str_length(frt) == 0) {
      x <- st_polygon()
      attr(x, "valid") <- FALSE
      x
    } else {
      tryCatch({
        x <- forestdata %>%
          filter(ccod_frt == frt, ccod_cact == agc) %>%
          st_transform(4326)
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      },
      error = function(e) {
        x <- st_polygon()
        attr(x, "valid") <- FALSE
        x
      }
      )
    }

    if (attr(rv$forest_polygon, "valid")) {
      # if the forest is valid, update the map
      rv$forest_polygon_ll <- st_transform(rv$forest_polygon, 4326)
      leafletProxy("view_map_forest") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = st_bbox(rv$forest_polygon_ll)$xmin[[1]],
          lat1 = st_bbox(rv$forest_polygon_ll)$ymin[[1]],
          lng2 = st_bbox(rv$forest_polygon_ll)$xmax[[1]],
          lat2 = st_bbox(rv$forest_polygon_ll)$ymax[[1]]
        ) %>%
        addPolygons(
          data = rv$forest_polygon_ll,
          group = "Extent",
          # label = ~tile_id,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        ) # %>%
    } else {
      # if the forest is not valid, reset the map
      react_map_forest(base_map())
    }
  })

  # use forest
  observeEvent(input$save_extent_forest, {
    withProgress(message = "Creating the extent", value = 0, {
      forest_valid <- update_extent(extent_source = "forest")
      if (forest_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Please choose a valid forest.",
          text = NULL,
          type = "error",
          btn_labels = "Ok"
        )
      }
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })


  ############### - Bbox mode -####################

  # message for bboxproj
  output$bboxproj_message <- renderUI({
    bboxproj_validated <- tryCatch(
      st_crs2(input$bboxproj),
      error = function(e) {
        st_crs(NA)
      }
    )$proj4string
    if (input$bboxproj == "") {
      rv$bboxproj <- NA
      ""
    } else if (is.na(bboxproj_validated)) {
      rv$bboxproj <- NA
      span(
        style = "color:red",
        "Insert a valid projection (EPSG code)."
      )
    } else {
      rv$bboxproj <- bboxproj_validated
      # span(style="color:darkgreen", "\u2714") # check
      div(strong("Selected projection:"),
        br(),
        bboxproj_validated,
        style = "color:darkgreen"
      )
    }
  })

  # create a new map (to be shown in modal dialog)
  react_map_bbox <- reactiveVal(base_map())
  output$view_map_bbox <- renderLeaflet({
    react_map_bbox()
  })

  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_bbox, {
    showModal(load_extent_bbox())
  })

  # update the map dynamically
  observeEvent(c(
    input$bbox_xmin, input$bbox_xmax,
    input$bbox_ymin, input$bbox_ymax,
    rv$bboxproj
  ), {

    # Check that the bounding box is valid
    if (!anyNA(c(
      input$bbox_xmin, input$bbox_xmax,
      input$bbox_ymin, input$bbox_ymax
    )) &
      !(is.null(rv$bboxproj) || is.na(rv$bboxproj))) {
      if (input$bbox_xmin != input$bbox_xmax &
        input$bbox_ymin != input$bbox_ymax) {
        # create the polygon
        rv$bbox_polygon <- st_as_sfc(
          st_bbox(
            c(
              "xmin" = input$bbox_xmin,
              "ymin" = input$bbox_ymin,
              "xmax" = input$bbox_xmax,
              "ymax" = input$bbox_ymax
            ),
            crs = rv$bboxproj
          )
        ) %>% st_transform(4326)
        attr(rv$bbox_polygon, "valid") <- TRUE
      } else {
        rv$bbox_polygon <- st_polygon()
        attr(rv$bbox_polygon, "valid") <- FALSE
      }
    } else {
      rv$bbox_polygon <- st_polygon()
      attr(rv$bbox_polygon, "valid") <- FALSE
    }

    # if bbox is valid, update the map
    if (attr(rv$bbox_polygon, "valid")) {
      rv$bbox_ll <- st_bbox(st_transform(rv$bbox_polygon, 4326))
      leafletProxy("view_map_bbox") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = as.numeric(rv$bbox_ll$xmin - (rv$bbox_ll$xmax - rv$bbox_ll$xmin) / 3),
          lat1 = as.numeric(rv$bbox_ll$ymin - (rv$bbox_ll$ymax - rv$bbox_ll$ymin) / 3),
          lng2 = as.numeric(rv$bbox_ll$xmax + (rv$bbox_ll$xmax - rv$bbox_ll$xmin) / 3),
          lat2 = as.numeric(rv$bbox_ll$ymax + (rv$bbox_ll$ymax - rv$bbox_ll$ymin) / 3)
        ) %>%
        addPolygons(
          data = rv$bbox_polygon,
          group = "Extent",
          # label = ~tile_id,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        ) # %>%
    } else {
      # if bbox is not valid, reset the map
      react_map_bbox(base_map())
    }
  })

  # use bbox
  observeEvent(input$save_extent_bbox, {
    # Add a progress bar while update_extent is running
    withProgress(message = "Creating the extent", value = 0, {
      bbox_valid <- update_extent(extent_source = "bbox")
      if (bbox_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Invalid bounding box",
          text = paste(
            "Please insert a valid bounding box."
          ),
          type = "error",
          btn_labels = "Ok"
        )
      }
      # Fake progress
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })

  ############# - Vector file mode -###############

  # if
  observeEvent(input$path_vectfile_sel, {
    uploaded_exts <- gsub("^.+\\.(.+)$", "\\1", input$path_vectfile_sel$name)
    # checks
    if (length(unique(gsub("\\..+$", "", input$path_vectfile_sel$name))) > 1) {
      # if more than one vector were chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = "Invalid vector",
        text = paste(
          "Please select a single vector",
          "(multiple selection is allowed only for shapefiles)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_path <- ""
    } else if (length(uploaded_exts) == 1 && !uploaded_exts %in% c("shp", "shx", "dbf", "prj")) {
      # if a single file was chosen and it is not a shapefile, use it
      rv$vectfile_path <- input$path_vectfile_sel$datapath
    } else if (anyNA(match(c("shp", "shx", "dbf", "prj"), uploaded_exts))) {
      # if a shapefile was chosen but some files are missing, do not use it
      sendSweetAlert(
        session,
        title = "Incomplete shapefile",
        text = paste(
          "Please select all the files of the shapefile",
          "(at most .shp, .shx, .prj, .dbf)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_path <- ""
    } else {
      # if a shapefile was chosen and all the files are present,
      # rename the uploaded files in order to have the same filename and use them
      path_vectfile_sel_new_datapath <- file.path(
        dirname(input$path_vectfile_sel$datapath), input$path_vectfile_sel$name
      )
      for (i in seq_len(nrow(input$path_vectfile_sel))) {
        file.rename(input$path_vectfile_sel$datapath[i], path_vectfile_sel_new_datapath[i])
      }
      rv$vectfile_path <- path_vectfile_sel_new_datapath[
        input$path_vectfile_sel$type == "application/x-esri-shape"
      ]
    }
  })

  # create a new map (to be shown in modal dialog)
  react_map_vectfile <- reactiveVal(base_map())
  output$view_map_vectfile <- renderLeaflet({
    react_map_vectfile()
  })

  # Open modal dialog to load the vector file
  observeEvent(input$button_extent_vectfile, {
    rv$vectfile_path <- ""
    showModal(load_extent_vectfile())
  })

  # load the vector on the map
  observeEvent(rv$vectfile_path, {

    # Check that the vector is valid
    rv$vectfile_polygon <- tryCatch({
      x <- st_read(rv$vectfile_path, quiet = TRUE) %>%
        st_transform(4326)
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    },
    error = function(e) {
      x <- st_polygon()
      attr(x, "valid") <- FALSE
      x
    }
    )

    if (attr(rv$vectfile_polygon, "valid")) {
      # if the vector is valid, update the map
      rv$vectfile_polygon_ll <- st_transform(rv$vectfile_polygon, 4326)
      leafletProxy("view_map_vectfile") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$vectfile_polygon_ll)[, "X"]),
          lat1 = min(st_coordinates(rv$vectfile_polygon_ll)[, "Y"]),
          lng2 = max(st_coordinates(rv$vectfile_polygon_ll)[, "X"]),
          lat2 = max(st_coordinates(rv$vectfile_polygon_ll)[, "Y"])
        ) %>%
        addPolygons(
          data = rv$vectfile_polygon_ll,
          group = "Extent",
          # label = ~tile_id,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        ) # %>%
    } else {
      # if the vector is not valid, reset the map
      react_map_vectfile(base_map())
    }
  })

  # use bbox
  observeEvent(input$save_extent_vectfile, {
    withProgress(message = "Creating the extent", value = 0, {
      vectfile_valid <- update_extent(extent_source = "vectfile")
      if (vectfile_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Please specify a valid vector file.",
          text = NULL,
          type = "error",
          btn_labels = "Ok"
        )
      }
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })


  ################### - Draw mode -######################"

  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_draw, {

    # create a new namespace every time the button is pushed,
    # in order not to make mess between modules
    extent_ns_name <- paste0("editor_", sample(1E9, 1))
    extent_ns <- NS(extent_ns_name)
    rv$extent_edits <- callModule(editModPoly, extent_ns_name, base_map())

    # show the modal dialog
    showModal(load_extent_draw(extent_ns_name))
  })

  # use bbox
  observeEvent(input$save_extent_draw, {
    withProgress(message = "Creating the extent", value = 0, {
      drawn_valid <- update_extent(extent_source = "draw")
      if (drawn_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Please draw a valid extent.",
          text = NULL,
          type = "error",
          btn_labels = "Ok"
        )
      }
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })


  #- Refresh the map if required -#
  observeEvent(input$button_refresh_map, {
    withProgress(message = "Refreshing the map", value = 0, {
      update_extent(extent_source = "fake")
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })

  ######## end of extent module ############

  ####### message help #############
  observeEvent(input$help_time_period, {
    showModal(modalDialog(
      title = i18n$t("Time period type"),
      p(HTML(
        i18n$t("<strong>Full</strong>:"),
        i18n$t("the specified time window is entirely processed"),
        i18n$t("(e.g., specifying a range from 2016-05-01 to 2018-09-30 will return"),
        i18n$t("all the products in this time window which match the other parameters).")
      )),
      p(HTML(
        i18n$t("<strong>Seasonal</strong>:"),
        i18n$t("the specified time window is processed from the first year to the"),
        i18n$t("last year, in the seasonal time windows from the first"),
        i18n$t("Julian day to the second Julian day"),
        i18n$t("(e.g., specifying a range from 2016-05-01 to 2018-09-30 will return"),
        i18n$t("all the products from 2016-05-01 to 2016-09-30, from 2017-05-01 to"),
        i18n$t("2017-09-30 and from 2018-05-01 to 2018-09-30,"),
        i18n$t("which also match the other parameters).")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$help_clip_on_extent, {
    showModal(modalDialog(
      title = i18n$t("Clip outputs on the selected extent?"),
      p(HTML(
        i18n$t("<strong>Yes</strong>:"),
        i18n$t("the extent selected in the tab \"Spatio-temporal selection\""),
        i18n$t("is used as extent for output products."),
        i18n$t("The user can pass other geometry parameters in the box"),
        i18n$t("\"Output geometry\".")
      )),
      p(HTML(
        i18n$t("<strong>No</strong>:"),
        i18n$t("the extent selected in the tab \"Spatio-temporal selection\""),
        i18n$t("is used to select tiles overlapping it;"),
        i18n$t("output products maintain the full extent and the geometry of"),
        i18n$t("Sentinel-2 input tiles.")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  ####### End of message help #############

  ###### Path module ######

  # Accessory functions to check that the new directory exists and is writable
  path_check <- function(path) {
    if (length(path) > 0 & path[1] != "") {
      if (!dir.exists(path)) {
        return(renderUI(span(
          style = "color:red",
          i18n$t("\u2718 (the directory does not exist)")
        )))
      } else if (file.access(path, mode = 2) < 0) {
        return(renderUI(span(
          style = "color:red",
          i18n$t("\u2718 (the directory is not writable)")
        )))
      } else {
        return(renderUI(span(
          style = "color:darkgreen",
          "\u2714"
        )))
      }
      #
    } else {
      return(renderText(""))
    }
  }

  shinyDirChoose(input, "path_project_sel", roots = volumes)

  # if paths change after using the shinyDirButton, update the values and the textInput
  observe({
    path_project_string <- parseDirPath(volumes, input$path_project_sel)
    updateTextInput(session, "path_project_textin", value = path_project_string)
  })


  # if path changes after using the textInput, update the value
  observe({
    output$path_project_errormess <- path_check(input$path_project_textin)
  })

  ############### Edit theia credentials login
  observeEvent(input$theia, {
    # open the modalDialog
    showModal(theia_modal(
      username = if (!is.null(input$theia_username)) {
        input$theia_username
      } else {
        NA
      },
      password = if (!is.null(input$theia_password)) {
        input$theia_password
      } else {
        NA
      }
    ))
    # dummy variable to define which save button has to be used
    output$switch_save_apitheia <- renderText({
      if (is.null(input$apitheia_default)) {
        ""
      } else if (input$apitheia_default) {
        "default"
      } else {
        "custom"
      }
    })
    outputOptions(output, "switch_save_apitheia", suspendWhenHidden = FALSE)
    # initialise the shinyFiles Save as button
    observe({
      apitheia_path_prev <- rv$apitheia_path
      shinyFileSave(input, "apitheia_path_sel", roots = volumes, session = session)
      apitheia_path_raw <- parseSavePath(volumes, input$apitheia_path_sel)
      rv$apitheia_path <- if (nrow(apitheia_path_raw) > 0) {
        as.character(apitheia_path_raw$datapath)
      } else {
        NA
      }
      if (!is.na(rv$apitheia_path)) {
        if (!rv$apitheia_path %in% apitheia_path_prev) {
          # if a change in the path is detected (= the button has been used),
          # close the modalDialog
          # FIXME if a user re-open the modalDialog and does not change
          # user nor password, the "Save as" button will not close the dialog
          shinyjs::click("save_apitheia")
        }
      }
    })
  })
  # save user/password
  observeEvent(input$save_apitheia, {
    write_theia_login(
      input$theia_username, input$theia_password,
      apitheia_path = if (!is.na(rv$apitheia_path)) {
        as.character(rv$apitheia_path)
      } else {
        NA
      }
    )
    removeModal()
  })
  ########### end login

  ########### indices

  create_indices_db()
  indices_db <- data.table(list_indices(c("n_index", "name", "longname", "s2_formula_mathml", "link", "checked")))
  check_mark <- icon("check") %>%
    span(style = "color:darkgreen;", .) %>%
    as.character() %>%
    gsub("\n *", "", .)
  indices_db[, extendedname := paste0(
    name,
    " (", longname, ")  ",
    ifelse(checked, check_mark, "")
  )]
  setkey(indices_db, "name")

  indices_rv <- reactiveValues()
  observe({
    indices_db_verified_idx <- if (input$verified_indices == TRUE) {
      indices_db$checked
    } else {
      rep(TRUE, nrow(indices_db))
    }
    indices_rv$matches <- indices_db[
      indices_db_verified_idx &
        grepl(
          tolower(input$filter_indices),
          tolower(indices_db$extendedname)
        ),
      name
    ]
    indices_rv$filtered <- indices_db[
      unique(c(indices_rv$checked, indices_rv$matches)),
      list(name, extendedname)
    ]
  })

  observe({
    indices_rv$checked <- sort(input$list_indices)
  })

  output$check_indices <- renderUI({
    checkboxGroupInput(
      "list_indices",
      label = i18n$t("Indices to be exported"),
      choiceNames = lapply(indices_rv$filtered$extendedname, HTML),
      choiceValues = as.list(indices_rv$filtered$name),
      selected = indices_rv$checked
    )
  })

  index_details <- function(index) {
    extendedname <- link <- longname <- name <- providers <- s2_formula_mathml <- NULL
    return(box(
      width = 12,
      title = indices_db[name == index, name],
      p(em(indices_db[name == index, longname])),
      p(
        strong(i18n$t("Formula:")),
        br(),
        withMathJax(indices_db[
          name == index,
          HTML(s2_formula_mathml)
        ])
      ),
      p(a(i18n$t("More info"),
        target = "_blank",
        href = indices_db[name == index, link]
      ))
    ))
  }

  output$show_formula <- renderUI({
    column(
      width = 4,
      lapply(indices_rv$checked, index_details)
    )
  })
  ########## end indices

  # Disable clipping and masking if no spatial filter was enabled
  observeEvent(input$query_space, {
    if (input$query_space) {
      enable("clip_on_extent")
      enable("extent_as_mask")
    } else {
      updateRadioButtons(session, "clip_on_extent", selected = FALSE)
      updateRadioButtons(session, "extent_as_mask", selected = FALSE)
      disable("clip_on_extent")
      disable("extent_as_mask")
    }
  })

  #### button
  # functions to check that all is correctly set TODO
  # return TRUE if check passes, FALSE if errors occur
  check_param <- function(param_list) {
    error_list <- check_param_list(param_list, type = "string", correct = FALSE)
    if (!is.null(error_list)) {
      # if errors occur:
      # build modal dialog
      check_param_modal <- modalDialog(
        title = i18n$t("Parameter errors"),
        size = "m",
        if (length(error_list) == 1) {
          tagList(
            p(
              i18n$t("A parameter has not been correctly set:"),
              br(), error_list
            ),
            p(i18n$t("Please edit it using the GUI before continuing."))
          )
        } else {
          tagList(
            p(HTML(
              i18n$t("Some parameters have not been correctly set:"),
              "<ul><li>",
              paste(error_list, collapse = "</li><li>"),
              "</li></ul>"
            )),
            p(i18n$t("Please edit them using the GUI before continuing."))
          )
        },
        easyClose = TRUE,
        footer = NULL
      )
      # show modal dialog
      showModal(check_param_modal)
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  # function to create a list to objects to be returned
  create_return_list <- function() {
    rl <- list()

    # processing steps #
    rl$project_name <- input$project_name
    # set directories #
    rl$path_project <- input$path_project_textin
    res <- paste0(input$path_project_textin, "/projets/", input$project_name)
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE, recursive = TRUE)
    }
    # name of path are paste from path_project + project_name
    rl$path_data <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/data")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of entire tiled products
    rl$path_tiles <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/tiles")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of entire tiled products
    rl$path_mosaic <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/mosaic")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of mosaic tiled products
    rl$path_translate <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/translate")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of translate tiled products
    rl$path_merged <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/merged")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of merged tiled products
    rl$path_warped <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/warped")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of warped tiled products
    rl$path_masked <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/masked")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of masked tiled products
    rl$path_out <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/out")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of output products
    rl$path_rgb <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/rgb")
      res2 <- paste0(res, "/jpg")
      if (!dir.exists(res)) {
        dir.create(res2, showWarnings = FALSE, recursive = TRUE)
      }
      res
    } else {
      NA
    } # path of rgb products
    rl$path_indices <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/indices")
      res2 <- paste0(res, "/jpg")
      if (!dir.exists(res)) {
        dir.create(res2, showWarnings = FALSE, recursive = TRUE)
      }
      res
    } else {
      NA
    } # path of spectral indices

    ## product
    rl$product <- input$product # theia to download THEIA product, peps to download PEPS product
    rl$theiacollection <- if (rl$product == "theia") {
      input$theiacollection
    } else {
      NA
    } # landsat, spotworldheritage, sentinel2, snow, venus
    rl$pepscollection <- if (rl$product == "peps") {
      input$pepscollection
    } else {
      NA
    } # s1, s2, s2st, s3
    rl$theiaplatformlandsat <- if (rl$product == "theia" & rl$theiacollection == "landsat") {
      input$theiaplatformlandsat
    } else {
      NA
    } # landsat5, landsat7, landsat8
    rl$theiaplatformspotworldheritage <- if (rl$product == "theia" & rl$theiacollection == "spotworldheritage") {
      input$theiaplatformspotworldheritage
    } else {
      NA
    } # spot1, spot2, spot3, spot4, spot5
    rl$theiaplatformsentinel <- if (rl$product == "theia" & rl$theiacollection == "sentinel2") {
      input$theiaplatformsentinel
    } else {
      NA
    } # s2a, s2b
    rl$theiaplatformvenus <- if (rl$product == "theia" & rl$theiacollection == "venus") {
      input$theiaplatformvenus
    } else {
      NA
    } # venus

    # level
    rl$theiaplatformsentinellevel <- if (rl$product == "theia" & rl$theiacollection == "sentinel2") {
      input$theiaplatformsentinellevel
    } else {
      NA
    } # venus


    rl$online <- as.logical(input$online) # TRUE if online mode, FALSE if offline mode
    rl$downloader <- input$downloader # downloader ("wget" or "aria2")
    rl$overwrite_product <- as.logical(input$overwrite_product) # TRUE to overwrite existing product, FALSE not to

    # spatio-temporal selection #
    rl$timewindow <- if (input$query_time == TRUE) { # range of dates
      input$timewindow
    } else {
      NA
    }
    rl$timeperiod <- if (input$query_time == TRUE) { # range of dates
      input$timeperiod # "full" or "seasonal"
    } else {
      "full"
    }

    # polygons
    rl$extent <- if (input$query_space == TRUE & !is.null(rv$extent)) {
      rv$extent %>%
        st_transform(4326) %>%
        geojson_json(pretty = TRUE)
    } else {
      NA
    }

    rl$s2tiles_selected <- if (input$query_space == TRUE & !is.null(input$tiles_checkbox)) {
      input$tiles_checkbox
    } else {
      NA
    } # selected tile IDs

    # product selection #
    rl$verified_indices <- input$verified_indices
    rl$list_indices_checked <- indices_rv$checked # index names
    rl$index_source <- input$index_source # reflectance band for computing indices ("BOA" or "TOA")
    rl$mask_type <- if (input$atm_mask == FALSE) {
      NA
    } else if (input$atm_mask_type == "custom") {
      paste0("scl_", paste(input$atm_mask_custom, collapse = "_"))
    } else {
      input$atm_mask_type
    } # atmospheric masking (accepted types as in s2_mask())
    rl$max_mask <- input$max_masked_perc
    rl$mask_smooth <- if (input$mask_apply_smooth) {
      input$mask_smooth
    } else {
      0
    }
    rl$clip_on_extent <- as.logical(input$clip_on_extent) # TRUE to clip (and warp) on the selected extent, FALSE to work at tiles/merged level
    rl$extent_as_mask <- as.logical(input$extent_as_mask) # TRUE to mask outside the polygons of extent, FALSE to use as boundig box
    rl$mask_buffer <- if (input$mask_apply_smooth) {
      input$mask_buffer
    } else {
      0
    }

    # rgb
    rl$rgb_out <- input$rgb_out

    # output format (GDAL format name)
    rl$outformat <- input$outformat
    rl$index_datatype <- input$index_datatype
    # output compression ("LZW", "DEFLATE" etc.)
    rl$compression <- ifelse(rl$outformat == "GTiff",
      input$compression,
      NA
    )
    # overwrite or skip existing files (logical)
    rl$overwrite <- as.logical(input$overwrite)
    rl$thumbnails <- if (rl$product == "theia") {
      as.logical(input$check_thumbnails)
    } else {
      NA
    } # logical (create thumbnails)

    # save apitheia.txt path if it was customly set
    if (!is.null(NULL) & !anyNA(NULL)) {
      rl$apitheia_path <- rv$apitheia_path
    }

    # information about package version
    rl$pkg_version <- packageVersion("shinycnes") %>% as.character()

    return(rl)
  }

  # function to import saved parameters
  import_param_list <- function(pl) {

    # Add a progress bar while importing
    withProgress(message = i18n$t("Loading the parameters"), value = 0, {

      # set directories
      updateTextInput(session, "project_name", value = pl$project_name)
      updateTextInput(session, "path_project_textin", value = pl$path_project)
      updateSelectInput(session, "listimage01", choices = c("Choose a picture" = "", limage()))
      updateSelectInput(session, "listimage02", choices = c("Choose a picture" = "", limagergb()))
      updateSelectInput(session, "listimage03", choices = c("Choose a picture" = "", limageind()))
      updateRadioButtons(session, "check_thumbnails", selected = pl$thumbnails)
      setProgress(0.2)

      # processing steps
      # product
      updateRadioButtons(session, "product", selected = pl$product)
      if (pl$product == "theia") {
        updateRadioButtons(session, "theiacollection", selected = pl$theiacollection)
      } else {
        updateRadioButtons(session, "pepscollection", selected = pl$pepscollection)
      }
      # theiaplatform
      if (pl$theiacollection == "landsat") {
        updateRadioButtons(session, "theiaplatformlandsat", selected = pl$theiaplatformlandsat)
      } else if (pl$theiacollection == "spotworldheritage") {
        updateRadioButtons(session, "theiaplatformspotworldheritage", selected = pl$theiaplatformspotworldheritage)
      } else if (pl$theiacollection == "sentinel2") {
        updateRadioButtons(session, "theiaplatformsentinel", selected = pl$theiaplatformsentinel)
        updateRadioButtons(session, "theiaplatformsentinellevel", selected = pl$theiaplatformsentinellevel)
      } else if (pl$theiacollection == "venus") {
        updateRadioButtons(session, "theiaplatformvenus", selected = pl$theiaplatformvenus)
      }
      # saving options
      updateRadioButtons(session, "online", selected = pl$online)
      updateRadioButtons(session, "downloader", selected = pl$downloader)
      updateRadioButtons(session, "overwrite_product", selected = pl$overwrite_product)
      setProgress(0.3)

      # spatio-temporal selection
      if (anyNA(pl$timewindow)) {
        updateRadioButtons(session, "query_time", selected = FALSE)
      } else {
        updateRadioButtons(session, "query_time", selected = TRUE)
        updateDateRangeInput(session, "timewindow", start = pl$timewindow[1], end = pl$timewindow[2])
        updateRadioButtons(session, "timeperiod", selected = pl$timeperiod)
      }
      if (anyNA(pl$extent) & pl$online == FALSE) {
        updateRadioButtons(session, "query_space", selected = FALSE)
      } else {
        updateRadioButtons(session, "query_space", selected = TRUE)
      }
      setProgress(0.4)

      # indices
      updateCheckboxInput(session, "verified_indices", value = pl$verified_indices)
      indices_rv$checked <- pl$list_indices_checked
      updateCheckboxGroupInput(session, "list_indices", selected = pl$list_indices)

      # rgb
      updateCheckboxGroupInput(session, "rgb_out", selected = pl$rgb_out)

      updateRadioButtons(session, "atm_mask",
        selected = ifelse(is.na(pl$mask_type), FALSE, TRUE)
      )
      updateSliderInput(session, "max_masked_perc",
        value = ifelse(is.na(pl$mask_type), 80, pl$max_mask)
      )
      updateNumericInput(session, "mask_apply_smooth",
        value = if (all(c(pl$mask_smooth, pl$mask_buffer) == 0)) {
          FALSE
        } else {
          TRUE
        }
      )
      updateNumericInput(session, "mask_smooth", value = pl$mask_smooth)
      updateNumericInput(session, "mask_buffer", value = pl$mask_buffer)
      updateRadioButtons(session, "atm_mask_type",
        selected = ifelse(is.na(pl$mask_type), "cloud_medium_proba", pl$mask_type)
      )
      updateRadioButtons(session, "atm_mask_custom",
        selected = ifelse(grepl("^scl\\_", pl$mask_type), strsplit(pl$mask_type, "_")[[1]][-1], c(0, 8:9))
      )
      updateRadioButtons(session, "index_source", selected = pl$index_source)
      updateRadioButtons(session, "clip_on_extent", selected = pl$clip_on_extent)
      updateRadioButtons(session, "keep_tiles", selected = ifelse(is.na(pl$path_tiles), FALSE, TRUE))
      updateRadioButtons(session, "keep_merged", selected = ifelse(is.na(pl$path_merged), FALSE, TRUE))
      setProgress(0.6)

      # update apihub path
      rv$apitheia_path <- pl$apitheia_path

      updateRadioButtons(session, "outformat", selected = pl$outformat)
      updateRadioButtons(session, "index_datatype", selected = pl$index_datatype)
      updateRadioButtons(session, "compression", selected = ifelse(pl$outformat == "GTiff",
        pl$compression,
        character(0)
      ))
      updateRadioButtons(session, "overwrite", selected = pl$overwrite)

      setProgress(0.8)

      # update extent (at the end, not to interfer with other events
      # (the delay is required to update the map after the map is charged)
      shinyjs::delay(5E3, {
        update_extent("imported", custom_source = pl$extent)
        updateCheckboxGroupInput(session, "tiles_checkbox",
          selected = pl$s2tiles_selected
        )
      })
      setProgress(1)
    })
  }

  # build the modal dialog
  cnes_download_modal <- reactive({
    modalDialog(
      title = i18n$t("Download products"),
      size = "s",
      uiOutput("cnes_download_message"),
      easyClose = FALSE,
      footer = NULL
    )
  })

  # if "Press to launch calc" is pressend, return values
  observeEvent(input$goButton, {
    showModal(cnes_download_modal())

    # create the text to show in the modaldialog
    shinyjs::html(
      "cnes_download_message",
      as.character(div(
        align = "center",
        p(i18n$t("Patience")),
        p(
          style = "text-align:center;font-size:500%;color:darkgrey;",
          icon("spinner", class = "fa-pulse")
        )
      ))
    )

    isolate({
      withCallingHandlers({
        shinyjs::html(id = "text00", html = " ")
        return_list <- create_return_list() # run creation of return_list
        check_param_result <- check_param(return_list)
        if (check_param_result) {
          # shinyjs::js$closeWindow()
          cnes(return_list)
        }
        # return_list
      },
      message = function(m) {
        shinyjs::html(id = "cnes_download_message", html = paste(m$message, "<br>"), add = TRUE)
      },
      warning = function(m) {
        shinyjs::html(id = "text00", html = m$message, add = TRUE)
      }
      )
    })

    shinyjs::html(
      "cnes_download_message",
      as.character(div(
        p(i18n$t("Thank you for your patience\u0021")),
        div(
          style = "text-align:right;",
          modalButton(i18n$t("\u2000Close"), icon = icon("check"))
        )
      )),
      add = TRUE
    )
  })

  #### image list indices ####
  limageind <- reactive({
    if (!is.null(paste0(input$path_project_textin, "/", input$project_name))) {
      limageind <- list()
      limageind <- grep(list.files(paste0(input$path_project_textin, "/", input$project_name, "/indices/jpg")), pattern = ".jpg.aux.xml", invert = TRUE, value = TRUE)
      names(limageind) <- basename(limageind)
      limageind
    } else {
      return(NULL)
    }
  })

  # list image
  observeEvent(c(input$path_project_textin, input$project_name), {
    updateSelectInput(session, "listimage03", choices = c("Choose a picture" = "", limageind()))
  })

  # image
  output$image03 <- renderImage({
    if (!is.null(paste0(input$path_project_textin, "/", input$project_name))) {
      src <- paste0(input$path_project_textin, "/", input$project_name, "/indices/jpg/", input$listimage03)
    } else {
      src <- tempfile(fileext = ".jpg")
    }
    return(list(
      src = src,
      filetype = "image/jpeg",
      width = 500,
      height = 500,
      alt = i18n$t("Indice image")
    ))
  }, deleteFile = FALSE)

  #### image list RGB ####
  limagergb <- reactive({
    if (!is.null(paste0(input$path_project_textin, "/", input$project_name))) {
      limagergb <- list()
      limagergb <- grep(list.files(paste0(input$path_project_textin, "/", input$project_name, "/rgb/jpg")), pattern = ".jpg.aux.xml", invert = TRUE, value = TRUE)
      names(limagergb) <- basename(limagergb)
      limagergb
    } else {
      return(NULL)
    }
  })

  # list image
  observeEvent(c(input$path_project_textin, input$project_name), {
    updateSelectInput(session, "listimage02", choices = c("Choose a picture" = "", limagergb()))
  })

  # image
  output$image02 <- renderImage({
    if (!is.null(paste0(input$path_project_textin, "/", input$project_name))) {
      src <- paste0(input$path_project_textin, "/", input$project_name, "/rgb/jpg/", input$listimage02)
    } else {
      src <- tempfile(fileext = ".jpg")
    }
    return(list(
      src = src,
      filetype = "image/jpeg",
      width = 500,
      height = 500,
      alt = i18n$t("RGB image")
    ))
  }, deleteFile = FALSE)

  #### image list Tiles ####
  limage <- reactive({
    if (!is.null(input$path_project_textin)) {
      limage <- list()
      limage <- list.files(
        path = paste0(input$path_project_textin, "/data"),
        pattern = "QKL_ALL.jpg",
        recursive = TRUE,
        full.names = FALSE
      )
      names(limage) <- basename(limage)
      limage
    } else {
      return(NULL)
    }
  })

  # list image
  observeEvent(input$path_project_textin, {
    updateSelectInput(session, "listimage01", choices = c("Choose a picture" = "", limage()))
  })

  # image
  output$image01 <- renderImage({
    if (!is.null(input$path_project_textin)) {
      src <- paste0(input$path_project_textin, "/data/", input$listimage01)
    } else {
      src <- tempfile(fileext = ".jpg")
    }
    return(list(
      src = src,
      filetype = "image/jpeg",
      width = 500,
      height = 500,
      alt = i18n$t("Sentinel image")
    ))
  }, deleteFile = FALSE)

  # if Exit is pressend, exit from GUI
  observeEvent(input$exit_gui, {
    shinyjs::js$closeWindow()
    stopApp()
  })

  # if Export is pressed, export the values (using server-side button)
  shinyFileSave(input, "export_param",
    roots = volumes,
    session = session
  )

  observeEvent(input$export_param, {
    export_param_path <- parseSavePath(volumes, input$export_param)
    if (nrow(export_param_path) > 0) {
      return_list <- create_return_list() # run creation of return_list
      check_param_result <- check_param(return_list)
      if (check_param_result) {
        writeLines(
          toJSON(return_list, pretty = TRUE),
          as.character(export_param_path$datapath)
        )
      }
    }
  })

  # if Import is pressed, read a json object (using server-side button)
  shinyFileChoose(input, "import_param",
    roots = volumes,
    session = session,
    filetypes = c("JSON" = "json")
  )

  observeEvent(input$import_param, {
    import_param_path <- input$import_param
    import_param_path <- parseFilePaths(volumes, input$import_param)
    rv$imported_param <- if (nrow(import_param_path) > 0) {
      import_param_path$datapath %>%
        as.character() %>%
        readLines() %>%
        fromJSON()
    } else {
      NULL
    }

    if (!is.null(rv$imported_param)) {
      import_param_list(rv$imported_param)
      rv$imported_param <- NULL
    }
  })

  # Closing the connection when window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  ##### end button
}
