# Define UI for application that draws a histogram
cnes_gui.ui <- dashboardPage(
  skin = "green",
  title = i18n$t("Shiny CNES"),

  dashboardHeader(title = i18n$t("Shiny CNES")),
  dashboardSidebar(
    # logo
    div(
      style = "text-align:center;padding-top:17px;padding-bottom:30px;",
      a(
        href = "https://pobsteta.github.io/shiny-cnes",
        target = "_blank",
        uiOutput("img_logo")
      )
    ),

    ### tab content
    sidebarMenu(
      menuItem(i18n$t("Product selection"), tabName = "tab_steps", icon = icon("image")),
      menuItem(i18n$t("Spatio-temporal selection"), tabName = "tab_query", icon = icon("clone")),
      menuItem(i18n$t("Spectral indices selection"), tabName = "tab_index", icon = icon("calculator")),
      menuItem(i18n$t("Processing result"), tabName = "tab_launch_processing", icon = icon("refresh")),
      menuItem(i18n$t("Prevision result"), tabName = "tab_launch_prevision", icon = icon("calendar"))
    ),

    HTML("<script src=\"message-handler.js\"></script>"),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    shiny::tags$head(shiny::tags$style(".darkbutton{background-color:#28353b;color:#b8c7ce;width:200px;")), # background color and font color
    shiny::tags$head(shiny::tags$script(src = "message-handler.js")), # for actionbuttons
    shiny::tags$head(shiny::tags$link(rel = "icon", href = "favicon.ico")),

    ### button
    div(
      style = "position:absolute;top:430px;",
      # server-side buttons
      p(
        style = "margin-top:15pt;",
        shinySaveButton(
          "export_param",
          i18n$t("Save options as"), i18n$t("Save parameters as"),
          filetype = list(json = "json"),
          class = "darkbutton"
        )
      ),
      p(
        style = "margin-top:5pt;",
        shinyFilesButton(
          "import_param",
          i18n$t("Load options"),
          i18n$t("Import a JSON file with parameters"),
          multiple = FALSE,
          class = "darkbutton"
        )
      ),
      p(
        style = "margin-top:0pt;",
        actionButton(
          "exit_gui",
          label = i18n$t("\u2000Close without processing"),
          icon = icon("ban"),
          class = "darkbutton"
        )
      ),
      p(
        style = "margin-top:0pt;",
        actionButton(
          "goPreprocessing",
          label = i18n$t("\u2000Starts preprocessing"),
          icon = icon("cog", class = "fa-spin"),
          class = "darkbutton"
        )
      ),
      p(
        style = "margin-top:0pt;",
        actionButton(
          "goPrediction",
          label = i18n$t("\u2000Starts prediction"),
          icon = icon("cog", class = "fa-spin"),
          class = "darkbutton"
        )
      ),
      p(
        style = "margin-top:20pt;",
        actionButton(
          "open_github_doc",
          label = i18n$t("\u2000Open documentation"),
          icon = icon("info-circle"),
          onclick = "window.open('https://gitpitch.com/pobsteta/theia2r_presentation#/', '_blank')",
          class = "darkbutton"
        )
      )
    ) # end of div
  ),

  dashboardBody(
    tabItems(
      #### One tab content Select product ####
      tabItem(
        tabName = "tab_steps",
        h2(i18n$t("Selection server")),
        # Boxes need to be put in a row (or column)
        fluidRow(
          # Box project options
          box(
            title = i18n$t("Project options"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            ### project options ###
            column(
              width = 4,
              div(
                style = "display:inline-block;vertical-align:top;",
                strong(i18n$t("Name of project: \u00a0"))
              ),
              div(
                style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                textInput("project_name", NULL, "")
              )
            ),
            column(
              width = 8,
              div(
                style = "display:inline-block;vertical-align:top;",
                strong(i18n$t("Directory for project: \u00a0"))
              ),
              div(
                style = "display:inline-block;vertical-align:top;",
                htmlOutput("path_project_errormess")
              ),
              div(
                div(
                  style = "display:inline-block;vertical-align:top;width:50pt;",
                  shinyDirButton("path_project_sel", i18n$t("Select"), i18n$t("Specify directory for the project"))
                ),
                div(
                  style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                  textInput("path_project_textin", NULL, "")
                )
              )
            )
          ) # end box
        ), # end fluidrow
        # first box
        fluidRow(
          box(
            title = i18n$t("Type of products"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 3,
            radioButtons(
              "product", NULL,
              choiceNames = list(
                span(
                  i18n$t("THEIA product "),
                  a("Pleiades, Spots, Sentinelle, Venus",
                    href = "http://www.theia-land.fr/fr",
                    target = "_blank"
                  ),
                  i18n$t(" (download)")
                ),
                span(
                  i18n$t("PEPS product "),
                  a("Sentinelle (S1, S2, S2ST, S3)",
                    href = "https://peps.cnes.fr/rocket/#/home",
                    target = "_blank"
                  ),
                  i18n$t(" (download)")
                )
              ),
              choiceValues = list("theia", "peps"),
              selected = "theia",
              inline = FALSE
            ) # end radiobutton
          ), # end box

          box(
            title = i18n$t("Type of collection"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 3,
            ### theia ###
            conditionalPanel(
              condition = "input.product == 'theia'",
              radioButtons(
                "theiacollection", NULL,
                choiceNames = list(
                  span(
                    a("Landsat",
                      href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=3487",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("SpotWorldHeritage",
                      href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=12923",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("SENTINEL2",
                      href = "https://theia.cnes.fr/atdistrib/documents/PSC-NT-411-0362-CNES_01_00_SENTINEL-2A_L2A_Products_Description.pdf",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("Snow",
                      href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=10748#fr",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("VENUS",
                      href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=12984",
                      target = "_blank"
                    )
                  )
                ),
                choiceValues = list("landsat", "spotworldheritage", "sentinel2", "snow", "venus"),
                selected = "sentinel2",
                inline = FALSE
              ) # end radiobutton
            ), # end conditionalpanel

            ### peps ###
            conditionalPanel(
              condition = "input.product == 'peps'",
              radioButtons(
                "pepscollection", NULL,
                choiceNames = list(
                  span(
                    a("S1",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("S2",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("S2ST",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("S3",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel3",
                      target = "_blank"
                    )
                  )
                ),
                choiceValues = list("s1", "s2", "s2st", "s3"),
                selected = "s2",
                inline = FALSE
              ) # end radiobutton
            ) # end conditionalpanel
          ), # end box

          ### landsat
          conditionalPanel(
            condition = "input.product == 'theia' && input.theiacollection == 'landsat'",
            box(
              title = i18n$t("Type of platform"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 3,
              ### platform ###
              radioButtons(
                "theiaplatformlandsat", NULL,
                choiceNames = list(
                  span(
                    a("LANDSAT5",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("LANDSAT7",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("LANDSAT8",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  )
                ),
                choiceValues = list("landsat5", "landsat7", "landsat8"),
                selected = "landsat8",
                inline = FALSE
              ) # end radiobutton
            ) # end box
          ), # end conditionalpanel

          ### spotworldheritage
          conditionalPanel(
            condition = "input.product == 'theia' && input.theiacollection == 'spotworldheritage'",
            box(
              title = i18n$t("Type of platform"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 3,
              ### platform ###
              radioButtons(
                "theiaplatformspotworldheritage", NULL,
                choiceNames = list(
                  span(
                    a("SPOT1",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("SPOT2",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("SPOT3",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("SPOT4",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("SPOT5",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  )
                ),
                choiceValues = list("spot1", "spot2", "spot3", "spot4", "spot5"),
                selected = "spot5",
                inline = FALSE
              ) # end radiobutton
            ) # end box
          ), # end conditionalpanel

          ### sentinel2
          conditionalPanel(
            condition = "input.product == 'theia' && input.theiacollection == 'sentinel2'",
            box(
              title = i18n$t("Type of platform"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 3,
              ### platform ###
              radioButtons(
                "theiaplatformsentinel", NULL,
                choiceNames = list(
                  span(
                    a("SENTINEL2A",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("SENTINEL2B",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  )
                ),
                choiceValues = list("s2a", "s2b"),
                selected = "s2b",
                inline = FALSE
              ) # end radiobutton
            ) # end box
          ), # end conditionalpanel

          ### venus
          conditionalPanel(
            condition = "input.product == 'theia' && input.theiacollection == 'venus'",
            box(
              title = i18n$t("Type of platform"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 3,
              ### platform ###
              radioButtons(
                "theiaplatformvenus", NULL,
                choiceNames = list(
                  span(
                    a("VENUS",
                      href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                      target = "_blank"
                    )
                  )
                ),
                choiceValues = list("venus"),
                selected = "venus",
                inline = FALSE
              ) # end radiobutton
            ) # end box
          ), # end conditionalpanel

          ### level of sentinel2
          conditionalPanel(
            condition = "input.product == 'theia' && input.theiacollection == 'sentinel2'",
            box(
              title = i18n$t("Level"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 3,
              ### level ###
              radioButtons(
                "theiaplatformsentinellevel", NULL,
                choiceNames = list(
                  span(
                    a("LEVEL1C",
                      href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("LEVEL2A",
                      href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                      target = "_blank"
                    )
                  ),
                  span(
                    a("LEVEL3A",
                      href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                      target = "_blank"
                    )
                  )
                ),
                choiceValues = list("l1c", "l2a", "l3a"),
                selected = "l2a",
                inline = FALSE
              ) # end radiobutton
            ) # end box
          ) # end conditionalpanel
        ), # end fluidrow

        fluidRow(
          # Box saving options
          box(
            title = i18n$t("Saving options"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            ### saving options ###
            # mode on/off line
            column(
              width = 6,
              # online_mode (online/offline mode)
              radioButtons(
                "online",
                label = span(
                  i18n$t("Download mode\u2000"),
                  actionLink("help_online", icon("question-circle"))
                ),
                choiceNames = list(
                  i18n$t("Online"),
                  i18n$t("Offline")
                ),
                choiceValues = list(TRUE, FALSE),
                selected = TRUE,
                inline = TRUE
              ),

              # Theia credentials
              conditionalPanel(
                condition = "input.online == 'TRUE'",
                conditionalPanel(
                  condition = "input.product == 'theia'",
                  div(
                    style = "padding-bottom:10px;",
                    actionButton(
                      "theia",
                      label = i18n$t("\u2000Login in THEIA"),
                      icon = icon("user-circle")
                    )
                  )
                ), # end conditionalpanel
                conditionalPanel(
                  condition = "input.product == 'peps'",
                  div(
                    style = "padding-bottom:10px;",
                    actionButton(
                      "peps",
                      label = i18n$t("\u2000Login in PEPS"),
                      icon = icon("user-circle")
                    )
                  )
                ) # end conditionalpanel
              ) # end conditionalpanel
            ), # end column
            column(
              width = 6,
              # overwrite products
              radioButtons(
                "overwrite_product",
                label = span(
                  i18n$t("Overwrite existing products ?\u2000"),
                  actionLink("help_overwrite_product", icon("question-circle"))
                ),
                choiceNames = list(
                  i18n$t("Yes"),
                  i18n$t("No")
                ),
                choiceValues = list(TRUE, FALSE),
                selected = TRUE,
                inline = TRUE
              ), # end radiobutton
              radioButtons(
                "downloader",
                label = span(
                  i18n$t("Downloader\u2000"),
                  actionLink("help_downloader", icon("question-circle"))
                ),
                choiceNames = list("wget", "aria2"),
                choiceValues = list("wget", "aria2"),
                selected = "wget",
                inline = TRUE
              ) # end radiobutton
            ), # end column
            fluidRow(
              column(
                width = 12,
                radioButtons(
                  "clip_on_extent",
                  label = span(
                    i18n$t("Clip outputs on the selected extent?\u2000"),
                    actionLink("help_clip_on_extent", icon("question-circle"))
                  ),
                  choiceNames = list(
                    i18n$t("Yes"),
                    i18n$t("No")
                  ),
                  choiceValues = list(TRUE, FALSE),
                  selected = TRUE,
                  inline = TRUE
                )
              ),
              column(
                width = 12,
                radioButtons(
                  "extent_as_mask",
                  label = i18n$t("Mask data outside the selected polygons?"),
                  choiceNames = list(
                    i18n$t("Yes"),
                    i18n$t("No")
                  ),
                  choiceValues = list(TRUE, FALSE),
                  selected = FALSE,
                  inline = TRUE
                )
              )
            ),

            hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),

            fluidRow(
              column(
                width = 6,
                radioButtons("keep_tiles", i18n$t("Save single tiles?"),
                  choiceNames = list(
                    i18n$t("Yes"),
                    i18n$t("No")
                  ),
                  choiceValues = list(TRUE, FALSE),
                  selected = FALSE,
                  inline = TRUE
                )
              ), # end column
              conditionalPanel(
                condition = "input.clip_on_extent == 'TRUE'",
                column(
                  width = 6,
                  radioButtons("keep_merged", i18n$t("Save merged tiles?"),
                    choiceNames = list(
                      i18n$t("Yes"),
                      i18n$t("No")
                    ),
                    choiceValues = list(TRUE, FALSE),
                    selected = FALSE,
                    inline = TRUE
                  )
                )
              ) # end conditionalpanel
            ) # end of fluidRow keep_tiles
          ), # end box saving options

          ### Box output files
          box(
            title = i18n$t("Output files"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            fluidRow(
              column(
                width = 6,
                div(
                  style = "margin-top: -15px",
                  checkboxInput(
                    "check_thumbnails",
                    label = span(
                      i18n$t("Create thumbnails\u2000"),
                      actionLink("help_thumbnails", icon("question-circle"))
                    ),
                    value = TRUE
                  )
                )
              ), # end column
              column(
                width = 6,
                radioButtons("overwrite",
                  label = i18n$t("Overwrite existing outputs"),
                  choiceNames = list(
                    i18n$t("Yes (reprocess all)"),
                    i18n$t("No (skip processing if outputs exist)")
                  ),
                  choiceValues = list(TRUE, FALSE),
                  selected = FALSE,
                  inline = TRUE
                )
              ), # end of column
              column(
                width = 4,
                selectInput("outformat",
                  label = i18n$t("Output file format"),
                  choices = list(
                    "GeoTiff" = "GTiff",
                    "ENVI" = "ENVI"
                  ),
                  # TODO add others common formats
                  selected = "GTiff"
                )
              ), # end of column
              conditionalPanel(
                condition = "input.outformat == 'GTiff'",
                column(
                  width = 3,
                  selectInput("compression",
                    label = i18n$t("Output compression"),
                    choices = list(
                      "Uncompressed" = "NONE",
                      "Low (packbits)" = "PACKBITS",
                      "Medium (lzw)" = "LZW",
                      "High (deflate)" = "DEFLATE"
                    ),
                    selected = "LZW"
                  )
                ) # end of column
              ) # end of conditionalpanel
            ), # end of fluidrow
            # naturel: B4, B3, B2.
            # proche infrarouge: B8, B4, B3
            # infrarouge lointain: B12, B8, B4
            # végétation: B11, B8, B2
            fluidRow(
              column(
                width = 4,
                checkboxGroupInput(
                  "rgb_out",
                  label = i18n$t("Select RGB output:\u2000"),
                  choiceNames = list(
                    i18n$t("Natural"),
                    i18n$t("Near infrared"),
                    i18n$t("Far infrared"),
                    i18n$t("Vegetation")
                  ),
                  choiceValues = list(
                    "natural",
                    "nearinfra",
                    "farinfra",
                    "vegetation"
                  ),
                  selected = "natural"
                ) # end of checkboxGroupInput
              ) # end of column
            ) # end fluidrow rgb
          ) # end of box
        ), # end fluidrow

        fluidRow(
          ### cloud mask
          box(
            title = i18n$t("Cloud mask"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            radioButtons(
              "atm_mask",
              label = span(
                i18n$t("Mask cloud-covered pixels?\u2000"),
                actionLink("help_mask", icon("question-circle"))
              ),
              choiceNames = list(
                i18n$t("Yes"),
                i18n$t("No")
              ),
              choiceValues = list(TRUE, FALSE),
              selected = FALSE,
              inline = TRUE
            ), # end of radiobuttons

            conditionalPanel(
              condition = "input.atm_mask == 'TRUE'",
              selectInput(
                "atm_mask_type",
                label = span(
                  i18n$t("Apply mask to:\u2000"),
                  actionLink("help_mask_classes", icon("question-circle"))
                ),
                choices = list(
                  "No data" = "nodata",
                  "No data and clouds (high probability)" = "cloud_high_proba",
                  "No data and clouds (high-medium prob.)" = "cloud_medium_proba",
                  "No data and clouds (any probability)" = "cloud_low_proba",
                  "No data, clouds and shadows" = "cloud_and_shadow",
                  "All except clear-sky" = "clear_sky",
                  "All except land surface" = "land",
                  "Custom mask" = "custom"
                ),
                selected = "cloud_medium_proba"
              ), # end of selectinput

              conditionalPanel(
                condition = "input.atm_mask_type == 'custom'",
                checkboxGroupInput(
                  "atm_mask_custom", i18n$t("Select the classes to mask:"),
                  choiceNames = list(
                    HTML("<font style=\"family:monospace;background-color:#000000;color:white;\">\u20020\u2002</font>\u2002No data"),
                    HTML("<font style=\"family:monospace;background-color:#FF0000;color:white;\">\u20021\u2002</font>\u2002Saturated or defective"),
                    HTML("<font style=\"family:monospace;background-color:#424142;color:white;\">\u20022\u2002</font>\u2002Dark area pixels"),
                    HTML("<font style=\"family:monospace;background-color:#633400;color:white;\">\u20023\u2002</font>\u2002Cloud shadows"),
                    HTML("<font style=\"family:monospace;background-color:#29f329;color:black;\">\u20024\u2002</font>\u2002Vegetation"),
                    HTML("<font style=\"family:monospace;background-color:#ffff00;color:black;\">\u20025\u2002</font>\u2002Bare soils"),
                    HTML("<font style=\"family:monospace;background-color:#0000ff;color:white;\">\u20026\u2002</font>\u2002Water"),
                    HTML("<font style=\"family:monospace;background-color:#7b7d7b;color:white;\">\u20027\u2002</font>\u2002Cloud (low probability)"),
                    HTML("<font style=\"family:monospace;background-color:#bdbebd;color:black;\">\u20028\u2002</font>\u2002Cloud (medium probability)"),
                    HTML("<font style=\"family:monospace;background-color:#ffffff;color:black;\">\u20029\u2002</font>\u2002Cloud (high probability)"),
                    HTML("<font style=\"family:monospace;background-color:#63cbff;color:black;\">\u200510\u2005</font>\u2002Thin cirrus"),
                    HTML("<font style=\"family:monospace;background-color:#ff9aff;color:black;\">\u200511\u2005</font>\u2002Snow")
                  ),
                  choiceValues = as.list(0:11),
                  selected = list(0, 1, 8, 9)
                )
              ), # end of conditionalpanel

              sliderInput(
                "max_masked_perc",
                label = span(
                  i18n$t("Maximum allowed cloud cover"),
                  actionLink("help_masked_perc", icon("question-circle"))
                ),
                min = 0, max = 100, value = 80,
                step = 1, post = "%"
              ), # end of sliderinput

              radioButtons(
                "mask_apply_smooth",
                label = span(
                  i18n$t("Smooth / bufferize the cloud-covered surface?\u2000"),
                  actionLink("help_mask_smooth", icon("question-circle"))
                ),
                choiceNames = list(
                  i18n$t("Yes"),
                  i18n$t("No")
                ),
                choiceValues = list(TRUE, FALSE),
                selected = FALSE,
                inline = TRUE
              ), # end of radiobuttons

              conditionalPanel(
                condition = "input.mask_apply_smooth == 'TRUE'",
                fluidRow(
                  column(
                    width = 6,
                    numericInput("mask_smooth",
                      i18n$t("Smooth (m)"),
                      value = 250,
                      min = 0
                    )
                  ),
                  column(
                    width = 6,
                    numericInput("mask_buffer",
                      i18n$t("Buffer (m)"),
                      value = 250
                    )
                  ) # end of column
                ) # end of smooth/buffer fluidRow
              ) # end of conditionalPanel mask_apply_smooth
            ) # end of conditionalPanel atm_mask
          ) # end of fluidRow/box "Atmospheric mask"
        ) # end fluidrow cloud mask
      ), # end first tab content

      #### Two tab content Saptio temporel ####
      tabItem(
        tabName = "tab_query",
        h2(i18n$t("Calc extent")),
        # Boxes need to be put in a row (or column)
        # first box Temporal parameters
        fluidRow(
          box(
            title = i18n$t("Temporal parameters"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            conditionalPanel(
              condition = "input.online == 'FALSE'",
              radioButtons("query_time",
                label = i18n$t("Use temporal filter?"),
                choiceNames = list(
                  i18n$t("Yes"),
                  i18n$t("No (process all the input THEIA images)")
                ),
                choiceValues = list(TRUE, FALSE),
                selected = TRUE,
                inline = TRUE
              )
            ),
            conditionalPanel(
              condition = "input.query_time == 'TRUE'",
              column(
                width = 6,
                dateRangeInput("timewindow", label = i18n$t("Time interval"), language = str_sub(Sys.getlocale("LC_TIME"), 1, 2))
              ),
              column(
                width = 6,
                radioButtons(
                  "timeperiod",
                  label = span(
                    i18n$t("Time period type\u2000"),
                    actionLink("help_time_period", icon("question-circle"))
                  ),
                  choiceNames = list(
                    span(
                      i18n$t("Full")
                    ),
                    span(
                      i18n$t("Seasonal")
                    )
                  ),
                  choiceValues = list("full", "seasonal"),
                  selected = "full",
                  inline = TRUE
                ) # end radiobutton
              ) # end of column
            ) # end of conditionalpanel
          ) # end of box
        ), # end of fluidRow/box "Temporal parameters"

        # second box Map
        fluidRow(
          box(
            title = i18n$t("Map"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            conditionalPanel(
              condition = "input.online == 'FALSE'",
              radioButtons("query_space",
                label = i18n$t("Use spatial filter/clip?"),
                choiceNames = list(
                  i18n$t("Yes"),
                  i18n$t("No (process all the input THEIA images)")
                ),
                choiceValues = list(TRUE, FALSE),
                selected = TRUE,
                inline = TRUE
              )
            ), # end of conditionalpanel
            fluidRow(
              conditionalPanel(
                condition = "input.query_space == 'TRUE'",
                column(
                  width = 12,
                  # Buttons to load the extent with modal dialogs
                  strong(i18n$t("Specify the extent:\u2000")),
                  span(
                    div(
                      style = "padding-top:5px;padding-bottom:10px;",
                      actionButton(
                        "button_extent_forest",
                        label = i18n$t("\u2000Choose on a list"),
                        width = 196,
                        icon = icon("list-ul")
                      ),
                      actionButton(
                        "button_extent_bbox",
                        label = i18n$t("\u2000Specify a bounding box"),
                        width = 196,
                        icon = icon("object-group")
                      ),
                      actionButton(
                        "button_extent_vectfile",
                        label = i18n$t("\u2000Load a vector file"),
                        width = 196,
                        icon = icon("upload")
                      ),
                      actionButton(
                        "button_extent_draw",
                        label = i18n$t("\u2000Draw it on the map"),
                        width = 196,
                        icon = icon("paint-brush")
                      ),
                      actionButton(
                        "button_refresh_map",
                        label = i18n$t("\u2000Reload the extent on map"),
                        width = 196,
                        icon = icon("retweet")
                      ),
                      column(
                        width = 2,
                        div(
                          checkboxGroupInput(
                            "tiles_checkbox",
                            i18n$t("Tiles selected"),
                            choices = character(0),
                            selected = character(0),
                            inline = FALSE
                          ),
                          strong(i18n$t("Orbits selected")),
                          helpText(em(i18n$t("Not yet implemented.")))
                        )
                      ) # end column
                    ) # end of div
                  ) # end of span
                ), # end of column
                column(
                  width = 12,
                  # Map
                  leafletOutput("view_map", height = 600, width = "100%")
                ) # end of column
              ) # end of conditionalpanel
            ) # end of fluidrow
          ) # end of box
        ) # end of fluidrow
      ), # end second tab content Map

      #### Three tab content Indices ####
      tabItem(
        tabName = "tab_index",
        title = i18n$t("Spectral indices"),
        conditionalPanel(
          condition = "input.product == 'theia' | input.product == 'peps'",
          fluidRow(
            box(
              title = i18n$t("Spectral indices selection"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,
              fluidRow(
                column(
                  width = 6,
                  radioButtons(
                    "index_source",
                    label = span(
                      i18n$t("Build indices from:\u2000"),
                      actionLink("help_index_source", icon("question-circle"))
                    ),
                    choices = list(
                      "BOA" = "BOA",
                      "TOA" = "TOA"
                    ),
                    selected = "BOA",
                    inline = TRUE
                  )
                ),
                column(
                  width = 6,
                  selectInput(
                    "index_datatype",
                    label = i18n$t("Data type"),
                    choices = list(
                      "Byte" = "Byte",
                      "Integer (16 bits)" = "Int16",
                      "Float (32 bits)" = "Float32",
                      "Float (64 bits)" = "Float64"
                    ),
                    selected = "Int16"
                  )
                )
              ),

              hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),
              fluidRow(
                column(
                  width = 5,
                  textInput("filter_indices", i18n$t("Filter indices"))
                ),
                column(
                  width = 7,
                  checkboxInput(
                    "verified_indices",
                    label = span(
                      i18n$t("Show only verified indices\u2000"),
                      actionLink("note_list_indices", icon("warning"))
                    ),
                    value = TRUE
                  )
                )
              ),
              uiOutput("check_indices")
            ),
            uiOutput("show_formula")
          )
        ) # end of conditionalpanel on tab_index
      ), # end of tabItem tab_index


      #### Four tab content Launch processing ####
      tabItem(
        tabName = "tab_launch_processing",
        tags$head(
          tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    "))
        ),
        shinyjs::useShinyjs(),
        h2(i18n$t("Processing result")),
        br(),
        fluidRow(
          box(
            title = i18n$t("Tiles downloaded"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 4,
            selectInput("listimage01", i18n$t("Tiles downloaded:"), c("No tile" = "")),
            div(
              style = "display:inline-block;horizontal-align:center;",
              imageOutput("image01", height = 500, width = 500)
            ) # end div
          ), # end box
          box(
            title = i18n$t("Tiles RGB"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 4,
            selectInput("listimage02", i18n$t("Tiles RGB:"), c("No tile" = "")),
            div(
              style = "display:inline-block;horizontal-align:center;",
              imageOutput("image02", height = 500, width = 500)
            ) # end div
          ), # end box
          box(
            title = i18n$t("Tiles indice"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 4,
            selectInput("listimage03", i18n$t("Tiles indice:"), c("No tile" = "")),
            div(
              style = "display:inline-block;horizontal-align:center;",
              imageOutput("image03", height = 500, width = 500)
            ) # end div
          ) # end box
        ), # end fluidrow 1
        br(),
        fluidRow(
          box(
            title = i18n$t("Presence/Absence"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 4,
            # button
            column(
              width = 12,
              # Buttons to load the extent with modal dialogs
              strong(i18n$t("Specify the extent:\u2000")),
              span(
                div(
                  style = "padding-top:5px;padding-bottom:10px;",
                  actionButton(
                    "button_extent_vectfile_pa",
                    label = i18n$t("\u2000Load a vector file"),
                    width = 176,
                    icon = icon("upload")
                  ),
                  # actionButton(
                  #   "button_extent_draw_pa",
                  #   label = i18n$t("\u2000Draw it on the map"),
                  #   width = 176,
                  #   icon = icon("paint-brush")
                  # ),
                  actionButton(
                    "button_refresh_map_pa",
                    label = i18n$t("\u2000Reload the extent on map"),
                    width = 176,
                    icon = icon("retweet")
                  )
                ) # end of div
              ) # end of span
            ), # end of column
            # Map presence/absence
            leafletOutput("view_map_presabs", height = 500, width = "100%")
          ), # end box
          box(
            title = i18n$t("Tiles TIF"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 4,
            selectInput("listimage04", i18n$t("Tiles TIF:"), c("No tile" = "")),
            div(
              style = "display:inline-block;horizontal-align:center;",
              imageOutput("image04", height = 500, width = 500)
            ) # end div
          ), # end box
          box(
            title = i18n$t("Mask shapefile"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 4,
            # button
            column(
              width = 12,
              # Buttons to load the extent with modal dialogs
              strong(i18n$t("Specify the extent:\u2000")),
              span(
                div(
                  style = "padding-top:5px;padding-bottom:10px;",
                  actionButton(
                    "button_extent_vectfile_mask",
                    label = i18n$t("\u2000Load a vector file"),
                    width = 176,
                    icon = icon("upload")
                  ),
                  # actionButton(
                  #   "button_extent_draw_mask",
                  #   label = i18n$t("\u2000Draw it on the map"),
                  #   width = 176,
                  #   icon = icon("paint-brush")
                  # ),
                  actionButton(
                    "button_refresh_map_mask",
                    label = i18n$t("\u2000Reload the extent on map"),
                    width = 176,
                    icon = icon("retweet")
                  )
                ) # end of div
              ) # end of span
            ), # end of column
            # Map presence/absence
            leafletOutput("view_map_mask", height = 500, width = "100%")
          ) # end box
        ), # end fluidrow 2
        br(),
        column(12, textOutput("text00"))
      ), # end tabitem tab_launch_processing
      
      #### Five tab content launch prevision ####
      tabItem(
        tabName = "tab_launch_prevision",
        title = i18n$t("Prevision map"),
        shinyjs::useShinyjs(),
        h2(i18n$t("Prevision result")),
        br(),
        fluidRow(
          box(
            title = i18n$t("Prevision map"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            # proejct name and button
            column(
              width = 8,
              # Buttons to load the extent with modal dialogs
              span(
                div(
                  style = "display:inline-block;vertical-align:top;",
                  strong(i18n$t("Name of project: \u00a0"))
                ),
                div(
                  style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                  verbatimTextOutput("project_name_verbatim")
                )
              ) # end of span
            ), # end of column
            column(
              width = 4,
              # Buttons to load the extent with modal dialogs
              span(
                div(
                  style = "display:inline-block;vertical-align:top;",
                  strong(i18n$t("Click on button: \u00a0"))
                ),
                div(
                  style = "padding-top:5px;padding-bottom:10px;",
                  actionButton(
                    "button_refresh_map_prevision",
                    label = i18n$t("\u2000Reload the extent on map"),
                    width = 176,
                    icon = icon("retweet")
                  )
                ) # end of div
              ) # end of span
            ), # end of column
            # Map
            leafletOutput("view_map_prevision", height = 800, width = "100%")
          ) # end of box
        ) # end of fluidrow
      ) # end of tabitem
      
    ) # end of tabitems
  ) # end of dashboard
)
