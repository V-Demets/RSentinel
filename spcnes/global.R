# add libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny, shinythemes, shinydashboard, shinyjs, shinyWidgets, leaflet, ggvis, ggrepel, dplyr, RColorBrewer, raster, gstat,
  rgdal, ggmap, ggplot2, reticulate, tools, leaflet.extras, pool, RPostgreSQL, devtools, mapedit, shiny.i18n, Cairo,
  stringr, shinyFiles, data.table, jsonlite, geojsonio, geojsonlint, imager, DBI
)
pacman::p_load_gh("hadley/tidyverse", "tidyverse/ggplot2", "tidyverse/dplyr", "r-spatial/sf", "jrowen/rhandsontable", "pobsteta/cnes")

mega <- 200
options(shiny.maxRequestSize = mega * 1024^2)
options(encoding = "UTF-8")

# i18n
i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("translations/translation.json", package = "cnes"))
i18n$set_translation_language("fr")


# connection to onf databse
options(pgsql = list(
  "host" = "0.0.0.0",
  "port" = 35432,
  "user" = "tryton",
  "password" = "tryton",
  "dbname" = "tryton"
))

# pool instance
pool <- pool::dbPool(
  drv = "PostgreSQL",
  port = options()$pgsql$port,
  dbname = options()$pgsql$dbname,
  host = options()$pgsql$host,
  user = options()$pgsql$user,
  password = options()$pgsql$password
)
onStop(function() {
  pool::poolClose(pool)
})


# check le seuil des 16 connexions ouvertes autorisees
getConnection <- function(group) {
  if (!exists(".connection", where = .GlobalEnv)) {
    .connection <<- DBI::dbConnect("PostgreSQL",
      dbname = options()$pgsql$dbname, host = options()$pgsql$host,
      port = options()$pgsql$port, user = options()$pgsql$user,
      password = options()$pgsql$password, group = group
    )
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    DBI::dbDisconnect(.connection)
    .connection <<- DBI::dbConnect("PostgreSQL",
      dbname = options()$pgsql$dbname, host = options()$pgsql$host,
      port = options()$pgsql$port, user = options()$pgsql$user,
      password = options()$pgsql$password, group = group
    )
  }
  return(.connection)
}

#' BDDQueryONF
#'
#' @param query = SQL format text
#'
#' @return result of database SQL query request
#'
BDDQueryONF <- function(query) {
  # query posgresql database onf
  # set up connection
  conn <- DBI::dbConnect("PostgreSQL",
    dbname = options()$pgsql$dbname, host = options()$pgsql$host,
    port = options()$pgsql$port, user = options()$pgsql$user,
    password = options()$pgsql$password
  )
  # dummy query (obviously), including a spatial subset and ST_SimplifyPreserveTopology to simplify geometry (optionel)
  result <- sf::st_read(conn, query = query) %>%
    sf::st_transform(result, crs = 4326)
  DBI::dbDisconnect(conn)
  return(result)
}

# Data
forestdata <- BDDQueryONF(query = "SELECT id, ccod_cact, ccod_frt, llib2_frt, geom FROM forest WHERE length(ccod_frt)>0 ORDER BY ccod_cact, ccod_frt")

# script js for close
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Define regular expressions to identify products
def_regex <- list(
  tile = list(regex = "SENTINEL([12][AC])\\_([0-9]{8})\\-([0-9]{6})\\-([0-9]{3})\\_L([1-3][A-C])\\_([A-Z])([0-9]{2})([A-Z]{3})\\_D"),
  image = list(regex = "SENTINEL([12][AC])\\_([0-9]{8})\\-([0-9]{6})\\-([0-9]{3})\\_L([1-3][A-C])\\_([A-Z])([0-9]{2})([A-Z]{3})\\_D_V1-9"),
  cloud = list(regex = "clouCover:")
)

# Define list of names of product tiles
def_names <- list(
  sentinel2 = c(
    "ATB_R1", "ATB_R2", "FRE_B11", "FRE_B12", "FRE_B2", "FRE_B3", "FRE_B4", "FRE_B5", "FRE_B6", "FRE_B7", "FRE_B8", "FRE_B8A",
    "SRE_B12", "SRE_B2", "SRE_B3", "SRE_B4", "SRE_B5", "SRE_B6", "SRE_B7", "SRE_B8", "SRE_B8A"
  ),
  sentinel2_mask = c(
    "CLM_R1", "CLM_R2", "DFP_R1", "DFP_R2", "DTF_R1-D01", "DTF_R1-D02", "DTF_R1-D03", "DTF_R1-D04", "DTF_R1-D05",
    "DTF_R1-D06", "DTF_R2-D01", "DTF_R2-D02", "DTF_R2-D03", "DTF_R2-D04", "DTF_R2-D05", "DTF_R2-D06", "EDG_R1",
    "EDG_R2", "IAB_R1", "IAB_R2", "MG2_R1", "MG2_R2", "SAT_R1", "SAT_R2"
  )
)
