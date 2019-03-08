#' @title Make a mosaic of each band of S2 tiles with the same date and orbit
#' @description The function mosaic the input Sentinel-2 files with
#'  the same date, orbit number, product type and file format.
#'  Outputs are a set of products in the same format of corresponding
#'  input files.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted to a format managed by
#'  GDAL.
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE).
#'  This parameter takes effect only if the output files are not VRT
#'  (in this case temporary files cannot be deleted, because rasters of source
#'  bands are included within them).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is to maintain each input format.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param vrt_rel_paths (optional) Logical: if TRUE (default on Linux),
#'  the paths present in the VRT output file are relative to the VRT position;
#'  if FALSE (default on Windows), they are absolute.
#'  This takes effect only with `format = "VRT"`.
#' @param out_crs (optional) proj4string (character) of the output CRS
#'  (default: the CRS of the first input file). The tiles with CRS different
#'  from `out_crs` will be reprojected (and a warning returned).
#' @param parallel (optional) Logical: if TRUE, the function is run using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#' @param overwrite Logical value: should existing output files be
#'  overwritten? (default: FALSE)
#' @param .log_message (optional) Internal parameter
#'  (it is used when the function is called by `theia2r()`).
#' @param .log_output (optional) Internal parameter
#'  (it is used when the function is called by `theia2r()`).
#' @return A vector with the names of the merged products (just created or
#'  already existing).
#' @importFrom rgdal GDALinfo
#' @importFrom magrittr "%>%"
#' @importFrom jsonlite fromJSON
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @import data.table
#' @export
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


s2_mosaic <- function(infiles,
                      cnes_list,
                      tilesdir,
                      outdir = ".",
                      subdirs = NA,
                      tmpdir = NA,
                      rmtmp = TRUE,
                      format = NA,
                      compress = "DEFLATE",
                      vrt_rel_paths = NA,
                      parallel = FALSE,
                      overwrite = FALSE) {

  # Define vrt_rel_paths
  if (is.na(vrt_rel_paths)) {
    vrt_rel_paths <- Sys.info()["sysname"] != "Windows"
  }

  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  msg <- NULL

  # list UTM in paths
  for (fic in infiles) {
    # verify if *MOSAIC*.tif exist
    if (!file.exists(file.path(outdir, basename(fic)))) {
      # verify that *TILES.tif exist
      if (file.exists(file.path(tilesdir, gsub("\\.vrt", ".tif", gsub("_MOSAIC_", "_TILES_", basename(fic)))))) {
        cmd <- paste0(
          "parallel ", binpaths$gdalbuildvrt, " -q ",
          gsub("_MOSAIC_([A-Z]){3}_([A-Z][0-9A-Z]{1,2})", "_MOSAIC_{}", fic), " ",
          file.path(tilesdir, gsub("_MOSAIC_([A-Z]){3}_([A-Z][0-9A-Z]{1,2}).vrt", "_TILES_{}.tif ::: ", basename(fic))),
          str_sub(str_extract(fic, "_([A-Z]){3}_[A-Z]([0-9A-Z]{1,2})"), 2)
        )
        msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
      }
    }
  }

  return(msg)
  
}
