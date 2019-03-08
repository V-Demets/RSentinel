#' @title Load Sentinel-2 tiles
#' @description Load the vector object of the Sentinel-2 tiles.
#' @return An sf spatial object containing the extent of the tiles.
#' @importFrom sf st_read
#' @importFrom utils unzip
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}, pascal Obstetar, (2019) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' @export

s2_tiles <- function() {

  # extract and import tiles kml
  s2tiles_kmz <- system.file("extdata", "vector", "s2_tiles.kmz", package = "cnes")
  s2tiles_kml <- gsub("\\.kmz$", ".kml", s2tiles_kmz)
  if (!file.exists(s2tiles_kml)) {
    unzip(zipfile = s2tiles_kmz, files = basename(s2tiles_kml), exdir = dirname(s2tiles_kml), unzip = "internal")
  }
  s2tiles <- sf::st_read(s2tiles_kml, stringsAsFactors = FALSE, quiet = TRUE)
  s2tiles[, !names(s2tiles) %in% c("Name", "geometry")] <- NULL
  names(s2tiles) <- gsub("^Name$", "tile_id", names(s2tiles))
  s2tiles
}
