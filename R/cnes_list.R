#' @title cnes_list
#'
#' @param spatial_extent Spatial extent
#' @param tile Tiles
#' @param orbit Orbit
#' @param time_interval Time interval
#' @param time_period Time period
#' @param level Level
#' @param platform Plateform
#' @param maxcloud Max cloud cover
#' @param collection Collection
#' @param ignore_ingestion_time Ignore
#' @param api API
#'
#' @return List of products
#' @export
cnes_list <- function(spatial_extent = NULL, tile = NULL, orbit = NULL, # spatial parameters
                      time_interval = NULL, time_period = "full", # temporal parameters
                      level = "L2A", platform = "SENTINEL2A", maxcloud = 101,
                      collection = "SENTINEL", ignore_ingestion_time = TRUE, api = NA) {

  # convert input NA arguments in NULL
  for (a in c("spatial_extent", "tile", "orbit", "time_interval", "api")) {
    if (suppressWarnings(all(is.na(get(a))))) {
      assign(a, NULL)
    }
  }

  # check if spatial_extent was provided
  spatial_extent_exists <- if (!exists("spatial_extent")) {
    FALSE
  } else if (is.null(spatial_extent)) {
    FALSE
  } else if (is(spatial_extent, "POLYGON")) {
    if (length(spatial_extent) == 0) {
      FALSE
    } else {
      TRUE
    }
  } else {
    TRUE
  }

  # if not, retrieve it from tile
  if (!spatial_extent_exists) {
    if (is.null(tile)) {
      print_message(
        type = "error",
        i18n$t("At least one parameter among spatial_extent and tile must be specified.")
      )
    } else {
      # extract and import tiles kml
      s2tiles_kmz <- system.file("extdata", "vector", "s2_tiles.kmz", package = "shinycnes")
      s2tiles_kml <- gsub("\\.kmz$", ".kml", s2tiles_kmz)
      if (!file.exists(s2tiles_kml)) {
        unzip(
          zipfile = s2tiles_kmz,
          files = basename(s2tiles_kml),
          exdir = dirname(s2tiles_kml),
          unzip = "internal"
        )
      }
      s2tiles <- st_read(s2tiles_kml, stringsAsFactors = FALSE, quiet = TRUE)
      # take the the selected tiles as extent
      # (this will result in the selection of more tiles, cause to overlapping
      # areas; it is filtered in s2_download, but it is slow: FIXME).
      # It is not possible to use tile centroids, because tile of external areas
      # of orbits could not be included).
      spatial_extent <- suppressWarnings(
        s2tiles[s2tiles$Name %in% tile, ]
      )
    }
  }

  # checks on inputs
  spatext <- st_bbox(st_transform(spatial_extent, 4326))

  # pass lat,lon if the bounding box is a point or line; latmin,latmax,lonmin,lonmax if it is a rectangle
  if (spatext["xmin"] == spatext["xmax"] || spatext["ymin"] == spatext["ymax"]) {
    lon <- mean(spatext["xmin"], spatext["xmax"])
    lat <- mean(spatext["ymin"], spatext["ymax"])
    lonmin <- lonmax <- latmin <- latmax <- NULL
  } else {
    lonmin <- spatext["xmin"]
    lonmax <- spatext["xmax"]
    latmin <- spatext["ymin"]
    latmax <- spatext["ymax"]
    lon <- lat <- NULL
  }

  # checks on dates
  # TODO add checks on format
  if (length(time_interval) == 1) {
    time_interval <- rep(time_interval, 2)
  }
  # split time_interval in case of seasonal download
  time_intervals <- if (time_period == "full") {
    data.frame(
      "start" = strftime(time_interval[1], "%Y%m%d"),
      "end" = strftime(time_interval[2], "%Y%m%d"),
      stringsAsFactors = FALSE
    )
  } else if (time_period == "seasonal") {
    data.frame(
      "start" = strftime(seq(time_interval[1], time_interval[2], by = "year"), "%Y%m%d"),
      "end" = strftime(rev(seq(time_interval[2], time_interval[1], by = "-1 year")), "%Y%m%d"),
      stringsAsFactors = FALSE
    )
  }

  # convert orbits to integer
  if (is.null(orbit)) {
    orbit <- list(NULL)
  } else {
    orbit <- as.integer(orbit)
    if (anyNA(orbit)) {
      orbit <- list(NULL)
    }
  }

  # define theia_download path
  theia_download_path <- system.file("theia_download", package = "shinycnes")

  # link to api
  if (is.null(api)) {
    api <- file.path(theia_download_path, "config_theia.cfg")
  }
  if (!file.exists(api)) {
    print_message(
      type = "error",
      i18n$t("File config_theia.cfg with the THEIA credentials is missing.")
    ) # TODO build it
  }

  # set level
  level <- switch(
    level,
    l1c = "LEVEL1C",
    l2a = "LEVEL2A",
    l3a = "LEVEL3A",
    "LEVEL2A"
  )

  # set collection
  collection <- switch(
    collection,
    landsat = "Landsat",
    spotworldheritage = "SpotWorldHeritage",
    sentinel2 = "SENTINEL2",
    snow = "Snow",
    venus = "VENUS",
    "SENTINEL2"
  )

  # set platform
  platform <- switch(
    platform,
    landsat5 = "LANDSAT5",
    landsat7 = "LANDSAT7",
    landsat8 = "LANDSAT8",
    spot1 = "SPOT1",
    spot2 = "SPOT2",
    spot3 = "SPOT3",
    spot4 = "SPOT4",
    spot5 = "SPOT5",
    s2a = "SENTINEL2A",
    s2b = "SENTINEL2B",
    venus = "VENUS",
    "SENTINEL2A"
  )

  # command to pass
  cmd <- paste(
    paste0(file.path(theia_download_path, "theia_download.py")), "--latmin", latmin,
    "--latmax", latmax, "--lonmin", lonmin, "--lonmax", lonmax, "--maxcloud", maxcloud,
    "--collection", collection, "--alternative_config", paste0(file.path(theia_download_path, "config_theia.cfg")),
    "--start_date", time_interval[1], "--end_date", time_interval[2], "--platform", platform,
    "--no_download"
  )

  # message from server
  msg <- capture.output(system2("python", cmd, stdout = TRUE, stderr = FALSE), split = TRUE)[-c(1, 2)]

  return(msg)
}
