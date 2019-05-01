#' @title Merge S2 tiles with the same date and orbit
#' @description The function merge the input Sentinel-2 files with
#'  the same date, orbit number, product type and file format.
#'  Outputs are a set of products in the same format of corresponding
#'  input files.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a format managed by
#'  GDAL (use [s2_translate] to do it); their names must be in the
#'  theia2r naming convention ([safe_shortname]).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param subdirs (optional) Logical: if TRUE, differet output products are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  `infiles` relate to more than a single product.
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


s2_merge <- function(infiles,
                     tilesdir,
                     warpedir,
                     extent,
                     outdir = ".",
                     subdirs = NA,
                     tmpdir = NA,
                     rmtmp = TRUE,
                     format = NA,
                     compress = "DEFLATE",
                     vrt_rel_paths = NA,
                     resolution = 10,
                     overwrite = FALSE) {

  # Define vrt_rel_paths
  if (is.na(vrt_rel_paths)) {
    vrt_rel_paths <- Sys.info()["sysname"] != "Windows"
  }

  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Extent + buffer(500m)
  ext <- extent %>% st_transform(2154) %>% st_buffer(500) %>% st_bbox()

  msg <- NULL

  warped <- stringr::str_replace(basename(infiles), 'MERGED', 'WARPED')
  
  regx <- unique(paste0(str_extract(basename(warped), "SENTINEL2A\\_([0-9]{8})"), "*_L2A*", str_extract(basename(warped), "\\_T([0-9]{2})[A-Z]")))

  # export and crop as a RGB image at full resolution
  for (fic in regx) {
    for (BAND in def_names$sentinel2) {
      if (!file.exists(file.path(outdir, paste0(gsub("\\*", "", fic), "_L93_MERGED_CROP_", BAND, ".tif")))) {
        cmd <- paste(
          paste0(
            binpaths$gdal_merge, " -n 0 ",
            file.path(warpedir, paste0(fic, "*_", BAND, "*.tif", " -o ")),
            file.path(outdir, paste0(gsub("\\*", "", fic), "_L93_MERGED_", BAND, ".tif"))
          )
        )
        msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
        # crop it to the extent
        cmd <- paste0(
          binpaths$gdalwarp, " -overwrite -dstnodata 0  -te ",
          ext$xmin, " ", ext$ymin, " ", ext$xmax, " ", ext$ymax, " ",
          file.path(outdir, paste0(gsub("\\*", "", fic), "_L93_MERGED_", BAND, ".tif ")),
          file.path(outdir, paste0(gsub("\\*", "", fic), "_L93_MERGED_CROP_", BAND, ".tif"))
        )
        msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
      }
    }
  }
  
  #### Remove all non CROP files ####
  fl <- grep(list.files(outdir), pattern = "CROP", invert = TRUE, value = TRUE)
  if (length(fl) >= 1) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Deletes all temporary files.")
    )
    file.remove(paste0(outdir, "/", fl))
  }

  return(msg)
}
