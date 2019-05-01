#' @title Create RGB images from S2 reflectance products.
#' @description Function to create RGB images from Sentinel-2 reflectances.
#' natural: B4, B3, B2
#' near infrarouge: B8, B4, B3
#' far infrarouge: B12, B8, B4
#' vegetation: B11, B8, B2
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the theia2r naming convention
#'  ([safe_shortname]).
#' @param outdir (optional) Full name of the existing output directory
#'  where the files should be created. Default is the same directory of
#'  input reflectance files. # FIXME use a subdir with product name
#' @param subdirs (optional) Logical: if TRUE, different indices are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  more than a single spectral index is required.
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is the same format of input images
#'  (or 'GTiff' in case of VRT input images).
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE)
#' @param parallel (optional) Logical: if TRUE, the function is run using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#' @param overwrite (optional) Logical value: should existing thumbnails be
#' @param rgblist rgblist
#' @param tilesdir Directory of tiles
#' @param mosaicdir Directory of mosaic
#' @param vrt_rel_paths Directory vrt
#' @param resolution Resolution
#' @param overwritten? (default: TRUE)
#' @return A vector with the names of the created images.
#'
#' @author pascal Obstetar, (2019) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom foreach foreach '%do%' '%dopar%'
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom rgdal GDALinfo
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_buffer st_transform st_bbox
#' @export

s2_rgb <- function(infiles,
                   rgblist,
                   tilesdir,
                   extent,
                   mosaicdir,
                   project,
                   outdir = ".",
                   subdirs = NA,
                   tmpdir = NA,
                   rmtmp = TRUE,
                   format = NA,
                   compress = "DEFLATE",
                   vrt_rel_paths = NA,
                   parallel = FALSE,
                   resolution = 10,
                   overwrite = FALSE) {

  # Define vrt_rel_paths
  if (is.na(vrt_rel_paths)) {
    vrt_rel_paths <- Sys.info()["sysname"] != "Windows"
  }

  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  msg <- NULL

  # calc cpu
  cpu <- parallel::detectCores()
  
  # final outdir from project
  finaloutdir <- gsub("/rgb", paste0("/projets/", project, "/rgb"), outdir)

  # temp var
  war <- list.files(finaloutdir, pattern = "CROP")
  reg <- unique(paste0(
    str_extract(basename(war), "SENTINEL2A\\_([0-9]{8})"), "_L2A",
    str_extract(basename(war), "\\_T([0-9]{2})[A-Z]"),
    str_extract(basename(war), "_RGB_L93_CROP_[a-z].*.tif")
  ))
  regx1 <- paste0(finaloutdir, reg)

  #### list UTM in paths ####
  for (fic in infiles) {
    # verify if *CROP*.tif exist
    regx2 <- paste0(
      str_extract(basename(fic), "SENTINEL2A\\_([0-9]{8})"), "_L2A",
      str_extract(basename(fic), "\\_T([0-9]{2})[A-Z]"),
      str_extract(basename(fic), "\\_RGB_L93_CROP_[a-z].*.tif")
    )
    if (length(grep(regx2, regx1)) == 0) {
      if ("natural" %in% rgblist && !is.na(str_extract(basename(fic), "natural"))) {
        # verify that all MOSAIC.vrt exist
        if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_natural.tif", "_MOSAIC_FRE_B2.vrt", basename(fic))))) {
          if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_natural.tif", "_MOSAIC_FRE_B3.vrt", basename(fic))))) {
            if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_natural.tif", "_MOSAIC_FRE_B4.vrt", basename(fic))))) {
              if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_natural.tif", "_RGB_natural.vrt", basename(fic))))) {
                cmd <- paste0(
                  binpaths$gdalbuildvrt, " -q -separate ",
                  gsub("_RGB_L93_CROP_natural.tif", "_RGB_natural.vrt ", fic),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_natural.tif", "_MOSAIC_FRE_B2.vrt ", basename(fic))),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_natural.tif", "_MOSAIC_FRE_B3.vrt ", basename(fic))),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_natural.tif", "_MOSAIC_FRE_B4.vrt ", basename(fic)))
                )
                msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
              }
            }
          }
        }
        # verify if vrt exists
        if (file.exists(file.path(outdir, gsub("_RGB_L93_CROP_natural.tif", "_RGB_natural.vrt", basename(fic))))) {
          if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_natural.tif", "_RGB_TRANSLATE_natural.tif", basename(fic))))) {
            cmd <- paste0(
              binpaths$gdal_translate, " -q -r cubic -ot Byte -scale 0 2000 -tr ",
              resolution, " ", resolution, " ",
              file.path(outdir, gsub("_RGB_L93_CROP_natural.tif", "_RGB_natural.vrt ", basename(fic))),
              file.path(outdir, gsub("_RGB_L93_CROP_natural.tif", "_RGB_TRANSLATE_natural.tif ", basename(fic)))
            )
            msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
        }
      } else if ("nearinfra" %in% rgblist && !is.na(str_extract(basename(fic), "nearinfra"))) {
        # verify that all MOSAIC.vrt exist
        if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_MOSAIC_FRE_B8.vrt", basename(fic))))) {
          if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_MOSAIC_FRE_B3.vrt", basename(fic))))) {
            if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_MOSAIC_FRE_B4.vrt", basename(fic))))) {
              if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_RGB_nearinfra.vrt", basename(fic))))) {
                cmd <- paste0(
                  binpaths$gdalbuildvrt, " -q -separate ",
                  gsub("_RGB_L93_CROP_nearinfra.tif", "_RGB_nearinfra.vrt ", fic),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_MOSAIC_FRE_B8.vrt ", basename(fic))),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_MOSAIC_FRE_B3.vrt ", basename(fic))),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_MOSAIC_FRE_B4.vrt ", basename(fic)))
                )
                msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
              }
            }
          }
        }
        # verify if vrt exists
        if (file.exists(file.path(outdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_RGB_nearinfra.vrt", basename(fic))))) {
          if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_RGB_TRANSLATE_nearinfra.tif", basename(fic))))) {
            cmd <- paste0(
              binpaths$gdal_translate, " -q -r cubic -ot Byte -scale 0 2000 -tr ",
              resolution, " ", resolution, " ",
              file.path(outdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_RGB_nearinfra.vrt ", basename(fic))),
              file.path(outdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_RGB_TRANSLATE_nearinfra.tif ", basename(fic)))
            )
            msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
        }
      } else if ("farinfra" %in% rgblist && !is.na(str_extract(basename(fic), "farinfra"))) {
        # verify that all MOSAIC.vrt exist
        if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_farinfra.tif", "_MOSAIC_FRE_B8.vrt", basename(fic))))) {
          if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_farinfra.tif", "_MOSAIC_FRE_B12.vrt", basename(fic))))) {
            if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_farinfra.tif", "_MOSAIC_FRE_B4.vrt", basename(fic))))) {
              if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_farinfra.tif", "_RGB_farinfra.vrt", basename(fic))))) {
                cmd <- paste0(
                  binpaths$gdalbuildvrt, " -q -separate ",
                  gsub("_RGB_L93_CROP_farinfra.tif", "_RGB_farinfra.vrt ", fic),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_farinfra.tif", "_MOSAIC_FRE_B8.vrt ", basename(fic))),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_farinfra.tif", "_MOSAIC_FRE_B12.vrt ", basename(fic))),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_farinfra.tif", "_MOSAIC_FRE_B4.vrt ", basename(fic)))
                )
                msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
              }
            }
          }
        }
        # verify if vrt exists
        if (file.exists(file.path(outdir, gsub("_RGB_L93_CROP_farinfra.tif", "_RGB_farinfra.vrt", basename(fic))))) {
          if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_farinfra.tif", "_RGB_TRANSLATE_farinfra.tif", basename(fic))))) {
            cmd <- paste0(
              binpaths$gdal_translate, " -q -r cubic -ot Byte -scale 0 2000 -tr ",
              resolution, " ", resolution, " ",
              file.path(outdir, gsub("_RGB_L93_CROP_farinfra.tif", "_RGB_farinfra.vrt ", basename(fic))),
              file.path(outdir, gsub("_RGB_L93_CROP_farinfra.tif", "_RGB_TRANSLATE_farinfra.tif ", basename(fic)))
            )
            msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
        }
      } else if ("vegetation" %in% rgblist && !is.na(str_extract(basename(fic), "vegetation"))) {
        # verify that all MOSAIC.vrt exist
        if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_vegetation.tif", "_MOSAIC_FRE_B8.vrt", basename(fic))))) {
          if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_vegetation.tif", "_MOSAIC_FRE_B11.vrt", basename(fic))))) {
            if (file.exists(file.path(mosaicdir, gsub("_RGB_L93_CROP_vegetation.tif", "_MOSAIC_FRE_B2.vrt", basename(fic))))) {
              if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_vegetation.tif", "_RGB_vegetation.vrt", basename(fic))))) {
                cmd <- paste0(
                  binpaths$gdalbuildvrt, " -q -separate ",
                  gsub("_RGB_L93_CROP_vegetation.tif", "_RGB_vegetation.vrt ", fic),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_vegetation.tif", "_MOSAIC_FRE_B8.vrt ", basename(fic))),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_vegetation.tif", "_MOSAIC_FRE_B11.vrt ", basename(fic))),
                  file.path(mosaicdir, gsub("_RGB_L93_CROP_vegetation.tif", "_MOSAIC_FRE_B2.vrt ", basename(fic)))
                )
                msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
              }
            }
          }
        }
        # verify if vrt exists
        if (file.exists(file.path(outdir, gsub("_RGB_L93_CROP_vegetation.tif", "_RGB_vegetation.vrt", basename(fic))))) {
          if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_vegetation.tif", "_RGB_TRANSLATE_vegetation.tif", basename(fic))))) {
            cmd <- paste0(
              binpaths$gdal_translate, " -q -r cubic -ot Byte -scale 0 2000 -tr ",
              resolution, " ", resolution, " ",
              file.path(outdir, gsub("_RGB_L93_CROP_vegetation.tif", "_RGB_vegetation.vrt ", basename(fic))),
              file.path(outdir, gsub("_RGB_L93_CROP_vegetation.tif", "_RGB_TRANSLATE_vegetation.tif ", basename(fic)))
            )
            msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
        }
      }
    }

    #### warpe tif and reproject in EPSG:2154 ####
    for (fic in infiles) {
      if (!file.exists(fic)) {
        if ("natural" %in% rgblist && !is.na(str_extract(basename(fic), "natural"))) {
          if (file.exists(file.path(outdir, gsub("_RGB_L93_CROP_natural.tif", "_RGB_TRANSLATE_natural.tif", basename(fic))))) {
            # verify if _RGB_L93_natural.tif exist
            if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_natural.tif", "_RGB_L93_natural.tif", basename(fic))))) {
              cmd <- paste0(
                "parallel -j", cpu, " ",
                binpaths$gdalwarp, " -q -r cubic -t_srs 'EPSG:2154' -tr ",
                resolution, " ", resolution, " ",
                paste0(file.path(outdir, paste0(
                  gsub(
                    "\\.tif", ".tif",
                    gsub(
                      "_RGB_L93_CROP_", "_RGB_TRANSLATE_",
                      gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(fic))
                    )
                  )
                )), " "),
                paste0(
                  file.path(outdir, paste0(
                    gsub(
                      "\\.tif", ".tif ::: ",
                      gsub(
                        "_RGB_L93_CROP_", "_RGB_L93_",
                        gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(fic))
                      )
                    )
                  )),
                  str_extract(basename(fic), "T([0-9]{2})[A-Z]")
                )
              )
              msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
            }
          }
        } else if ("nearinfra" %in% rgblist && !is.na(str_extract(basename(fic), "nearinfra"))) {
          if (file.exists(file.path(outdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_RGB_TRANSLATE_nearinfra.tif", basename(fic))))) {
            # verify if _RGB_L93_nearinfra.tif exist
            if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_nearinfra.tif", "_RGB_L93_nearinfra.tif", basename(fic))))) {
              cmd <- paste0(
                "parallel -j", cpu, " ",
                binpaths$gdalwarp, " -q -r cubic -t_srs 'EPSG:2154' -tr ",
                resolution, " ", resolution, " ",
                paste0(file.path(outdir, paste0(
                  gsub(
                    "\\.tif", ".tif",
                    gsub(
                      "_RGB_L93_CROP_", "_RGB_TRANSLATE_",
                      gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(fic))
                    )
                  )
                )), " "),
                paste0(
                  file.path(outdir, paste0(
                    gsub(
                      "\\.tif", ".tif ::: ",
                      gsub(
                        "_RGB_L93_CROP_", "_RGB_L93_",
                        gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(fic))
                      )
                    )
                  )),
                  str_extract(basename(fic), "T([0-9]{2})[A-Z]")
                )
              )
              msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
            }
          }
        } else if ("farinfra" %in% rgblist && !is.na(str_extract(basename(fic), "farinfra"))) {
          if (file.exists(file.path(outdir, gsub("_RGB_L93_CROP_farinfra.tif", "_RGB_TRANSLATE_farinfra.tif", basename(fic))))) {
            # verify if _RGB_L93_farinfra.tif exist
            if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_farinfra.tif", "_RGB_L93_farinfra.tif", basename(fic))))) {
              cmd <- paste0(
                "parallel -j", cpu, " ",
                binpaths$gdalwarp, " -q -r cubic -t_srs 'EPSG:2154' -tr ",
                resolution, " ", resolution, " ",
                paste0(file.path(outdir, paste0(
                  gsub(
                    "\\.tif", ".tif",
                    gsub(
                      "_RGB_L93_CROP_", "_RGB_TRANSLATE_",
                      gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(fic))
                    )
                  )
                )), " "),
                paste0(
                  file.path(outdir, paste0(
                    gsub(
                      "\\.tif", ".tif ::: ",
                      gsub(
                        "_RGB_L93_CROP_", "_RGB_L93_",
                        gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(fic))
                      )
                    )
                  )),
                  str_extract(basename(fic), "T([0-9]{2})[A-Z]")
                )
              )
              msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
            }
          }
        } else if ("vegetation" %in% rgblist && !is.na(str_extract(basename(fic), "vegetation"))) {
          if (file.exists(file.path(outdir, gsub("_RGB_L93_CROP_vegetation.tif", "_RGB_TRANSLATE_vegetation.tif", basename(fic))))) {
            # verify if _RGB_L93_vegetation.tif exist
            if (!file.exists(file.path(outdir, gsub("_RGB_L93_CROP_vegetation.tif", "_RGB_L93_vegetation.tif", basename(fic))))) {
              cmd <- paste0(
                "parallel -j", cpu, " ",
                binpaths$gdalwarp, " -q -r cubic -t_srs 'EPSG:2154' -tr ",
                resolution, " ", resolution, " ",
                paste0(file.path(outdir, paste0(
                  gsub(
                    "\\.tif", ".tif",
                    gsub(
                      "_RGB_L93_CROP_", "_RGB_TRANSLATE_",
                      gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(fic))
                    )
                  )
                )), " "),
                paste0(
                  file.path(outdir, paste0(
                    gsub(
                      "\\.tif", ".tif ::: ",
                      gsub(
                        "_RGB_L93_CROP_", "_RGB_L93_",
                        gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(fic))
                      )
                    )
                  )),
                  str_extract(basename(fic), "T([0-9]{2})[A-Z]")
                )
              )
              msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
            }
          }
        }
      }
    }
  }

  #### Extent + buffer(500m), Merge and Crop ####
  ext <- extent %>% st_transform(2154) %>% st_buffer(500) %>% st_bbox()

  # merge same date and same UTM
  if ("natural" %in% rgblist) {
    warped <- list.files(outdir, pattern = "*_RGB_L93_natural.tif")
    if (length(warped) != 0) {
      regx <- unique(paste0(str_extract(basename(warped), "SENTINEL2A\\_([0-9]{8})"), "*_L2A*",
                            str_extract(basename(warped), "\\_T([0-9]{2})[A-Z]")))

      # export as a RGB image at full resolution
      for (fic in regx) {
        if (!file.exists(file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_CROP_natural.tif")))) {
          if (!file.exists(file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_natural.tif")))) {
            cmd <- paste0(
              binpaths$gdal_merge, " -n 0 ",
              file.path(outdir, paste0(fic, "*RGB_L93_natural.tif -o ")),
              file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_natural.tif"))
            )
            msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
          # crop it to the extent
          cmd <- paste0(
            binpaths$gdalwarp, " -overwrite -dstnodata 0  -te ",
            ext$xmin, " ", ext$ymin, " ", ext$xmax, " ", ext$ymax, " ",
            file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_natural.tif ")),
            file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_CROP_natural.tif"))
          )
          msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
        }
      }
    }
  }
  if ("nearinfra" %in% rgblist) {
    warped <- list.files(outdir, pattern = "*_RGB_L93_nearinfra.tif")
    if (length(warped) != 0) {
      regx <- unique(paste0(str_extract(basename(warped), "SENTINEL2A\\_([0-9]{8})"), "*_L2A*", 
                            str_extract(basename(warped), "\\_T([0-9]{2})[A-Z]")))

      # export as a RGB image at full resolution
      for (fic in regx) {
        if (!file.exists(file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_CROP_nearinfra.tif")))) {
          if (!file.exists(file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_nearinfra.tif")))) {
            cmd <- paste0(
              binpaths$gdal_merge, " -n 0 ",
              file.path(outdir, paste0(fic, "*RGB_L93_nearinfra.tif -o ")),
              file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_nearinfra.tif"))
            )
            msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
          # crop it to the extent
          cmd <- paste0(
            binpaths$gdalwarp, " -overwrite -dstnodata 0  -te ",
            ext$xmin, " ", ext$ymin, " ", ext$xmax, " ", ext$ymax, " ",
            file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_nearinfra.tif ")),
            file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_CROP_nearinfra.tif"))
          )
          msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
        }
      }
    }
  }
  if ("farinfra" %in% rgblist) {
    warped <- list.files(outdir, pattern = "*_RGB_L93_farinfra.tif")
    if (length(warped) != 0) {
      regx <- unique(paste0(str_extract(basename(warped), "SENTINEL2A\\_([0-9]{8})"), "*_L2A*", 
                            str_extract(basename(warped), "\\_T([0-9]{2})[A-Z]")))

      # export as a RGB image at full resolution
      for (fic in regx) {
        if (!file.exists(file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_CROP_farinfra.tif")))) {
          if (!file.exists(file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_farinfra.tif")))) {
            cmd <- paste0(
              binpaths$gdal_merge, " -n 0 ",
              file.path(outdir, paste0(fic, "*RGB_L93_farinfra.tif -o ")),
              file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_farinfra.tif"))
            )
            msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
          # crop it to the extent
          cmd <- paste0(
            binpaths$gdalwarp, " -overwrite -dstnodata 0  -te ",
            ext$xmin, " ", ext$ymin, " ", ext$xmax, " ", ext$ymax, " ",
            file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_farinfra.tif ")),
            file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_CROP_farinfra.tif"))
          )
          msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
        }
      }
    }
  }
  if ("vegetation" %in% rgblist) {
    warped <- list.files(outdir, pattern = "*_RGB_L93_vegetation.tif")
    if (length(warped) != 0) {
      regx <- unique(paste0(str_extract(basename(warped), "SENTINEL2A\\_([0-9]{8})"), "*_L2A*",
                            str_extract(basename(warped), "\\_T([0-9]{2})[A-Z]")))

      # export as a RGB image at full resolution in finaloutdir
      for (fic in regx) {
        if (!file.exists(file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_CROP_vegetation.tif")))) {
          if (!file.exists(file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_vegetation.tif")))) {
            cmd <- paste0(
              binpaths$gdal_merge, " -n 0 ",
              file.path(outdir, paste0(fic, "*RGB_L93_vegetation.tif -o ")),
              file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_vegetation.tif"))
            )
            msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
          # crop it to the extent
          cmd <- paste0(
            binpaths$gdalwarp, " -overwrite -dstnodata 0  -te ",
            ext$xmin, " ", ext$ymin, " ", ext$xmax, " ", ext$ymax, " ",
            file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_MERGED_vegetation.tif ")),
            file.path(finaloutdir, paste0(gsub("\\*", "", fic), "_RGB_L93_CROP_vegetation.tif"))
          )
          msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
        }
      }
    }
  }

  #### Remove all non CROP files ####
  fl <- grep(list.files(finaloutdir), pattern = "CROP", invert = TRUE, value = TRUE)
  if (length(fl) > 1) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Deletes all temporary files.")
    )
    file.remove(paste0(finaloutdir, "/", fl[-1]))
  }

  #### export all .tif in .jpg ####
  fc <- grep(list.files(finaloutdir), pattern = "CROP", value = TRUE)
  fj <- grep(list.files(paste0(finaloutdir, "/jpg")), pattern = ".jpg.aux", value = TRUE)
  if (length(fj) != length(fc)) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Export all tif files in jpg.")
    )
    for (fic in fc) {
      cmd <- paste0(
        binpaths$gdal_translate, " -q -of JPEG -co QUALITY=90 -a_nodata 0 ",
        file.path(paste0(finaloutdir, "/", fic)), " ",
        file.path(paste0(finaloutdir, "/jpg"), paste0(gsub("\\.tif", ".jpg", basename(fic))))
      )
      msg <- system(cmd, intern = Sys.info()["sysname"] == "Windows")
    }
  }

  return(msg)
  
}
