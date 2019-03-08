#' @title Compute maps of spectral indices
#' @description Create maps of a set of spectral indices. Since
#'  `gdal_calc.py` is used to perform computations, output files
#'  are physical rasters (no output VRT is allowed).
#'
#' @param infiles A vector of input filenames. Input files are paths
#'  of BOA (or TOA) products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the theia2r naming convention
#'  ([safe_shortname]).
#' @param indices Character vector with the names of the required
#'  indices. Values should be included in names corresponding to the
#'  Abbreviations of the following indices:
#'  [IDB](http://www.indexdatabase.de/db/is.php?sensor_id=96)
#'  FIXME the list of the accepted values is a subset; this reference
#'  will be replaced with an internal html page integrated in the
#'  shiny interface).
#' @param outdir (optional) Full name of the output directory where
#'  the files should be created (default: current directory).
#'  `outdir` can bot be an existing or non-existing directory (in the
#'  second case, its parent directory must exists).
#'  If it is a relative path, it is expanded from the common parent
#'  directory of `infiles`.
#' @param parameters (optional) Values of index parameters. This variable
#'  must be a named list, in which each element is a list of parameters,
#'  i.e.:
#'  `parameters = list('SAVI' = list('a' = 0.5))`
#'  Values can be both numeric values or band names (e.g. 'band_1').
#'  If not specified, parameters are set to default values.
#' @param source (optional) Vector with the products from which computing
#'  the indices. It can be 'BOA', 'TOA' or both (default). If both values
#'  are provided, indices are computed from the available products ('TOA'
#'  if TOA is available, BOA if BOA is available); in the case both are
#'  available, two files are produced (they can be distinguished from the
#'  level component - S2x1C or S2x2A - in the filename).
#' @param format (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is the same format of input images
#'  (or 'GTiff' in case of VRT input images).
#' @param subdirs (optional) Logical: if TRUE, different indices are
#'  placed in separated `outfile` subdirectories; if FALSE, they are placed in
#'  `outfile` directory; if NA (default), subdirectories are created only if
#'  more than a single spectral index is required.
#' @param tmpdir (optional) Path where intermediate files (GTiff) will be
#'  created in case `format` is 'VRT'.
#' @param compress (optional) In the case a GTiff format is
#'  present, the compression indicated with this parameter is used.
#' @param dataType (optional) Numeric datatype of the ouptut rasters.
#'  if 'Float32' or 'Float64' is chosen, numeric values are not rescaled;
#'  if 'Int16' (default) or 'UInt16', values are multiplicated by `scaleFactor` argument;
#'  if 'Byte', values are shifted by 100, multiplicated by 100 and truncated
#'  at 200 (so that range -1 to 1 is coherced to 0-200), and nodata value
#'  is assigned to 255.
#' @param scaleFactor (optional) Scale factor for output values when an integer
#'  datatype is chosen (default values are 10000 for 'Int16' and 'UInt16',
#'  1E9 for 'Int32' and 'UInt32'). Notice that, using 'UInt16' and 'UInt32' types,
#'  negative values will be truncated to 0.
#' @param parallel (optional) Logical: if TRUE, the function is run using parallel
#'  processing, to speed-up the computation for large rasters.
#'  The number of cores is automatically determined; specifying it is also
#'  possible (e.g. `parallel = 4`).
#'  If FALSE (default), single core processing is used.
#'  Multiprocess masking computation is always performed in singlecore mode
#' @param overwrite Logical value: should existing output files be overwrite
#' @param project Name of project
#' @return A vector with the names of the created products.
#' @export
#' @importFrom foreach foreach '%do%' '%dopar%'
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom jsonlite fromJSON
#' @import data.table
#' @importFrom rgdal GDALinfo
#' @importFrom magrittr '%>%'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

s2_calcindices <- function(infiles,
                           indices,
                           project,
                           extent,
                           outdir = ".",
                           parameters = NULL,
                           resolution = 10,
                           source = "BOA",
                           format = NA,
                           subdirs = NA,
                           tmpdir = NA,
                           compress = "DEFLATE",
                           dataType = "Int16",
                           scaleFactor = NA,
                           parallel = FALSE,
                           overwrite = FALSE) {

  # Load GDAL paths
  binpaths <- load_binpaths("gdal")

  # Compute n_cores
  n_cores <- if (is.numeric(parallel)) {
    as.integer(parallel)
  } else if (parallel == FALSE) {
    1
  } else {
    parallel::detectCores()
  }
  if (n_cores <= 1) {
    `%DO%` <- `%do%`
    parallel <- FALSE
    n_cores <- 1
  } else {
    `%DO%` <- `%dopar%`
  }

  # generate indices.json if missing and read it
  create_indices_db()
  indices_db <- list_indices(c("n_index", "name", "longname", "s2_formula", "a", "b", "x"))

  # check that the required indices exists
  if (!all(indices %in% indices_db$name)) {
    print_message(
      type = "error", if (!any(indices %in% indices_db$name)) {
        "The "
      } else {
        "Some of the "
      }, "requested index names (\"", paste(indices[!indices %in% indices_db$name], collapse = "\", \""), "\") are not recognisable; please use accepted ", "values. To list accepted index names, type ",
      "'sort(list_indices(\"name\"))'."
    )
  }
  if (!all(indices %in% indices_db$name)) {
    print_message(type = "warning", "Some of the specified index names (", paste(indices[!indices %in% indices_db$name], collapse = ", "), ") are not recognisable and will be skipped.")
    indices <- indices[indices %in% indices_db$name]
  }
  # extract needed indices_db
  indices_info <- indices_db[match(indices, indices_db$name), ]

  # check output format
  gdal_formats <- fromJSON(system.file("extdata", "gdal_formats.json", package = "cnes"))
  if (!is.na(format)) {
    sel_driver <- gdal_formats[gdal_formats$name == format, ]
    if (nrow(sel_driver) == 0) {
      print_message(
        type = "error", "Format \"", format, "\" is not recognised; ", "please use one of the formats supported by your GDAL installation.\n\n", "To list them, use the following command:\n",
        "gdalUtils::gdalinfo(formats=TRUE)\n\n", "To search for a specific format, use:\n", "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]"
      )
    }
  }

  # assign scaleFactor value
  if (grepl("Int", dataType) & is.na(scaleFactor)) {
    scaleFactor <- ifelse(grepl("Int32", dataType), 1e+09, 10000)
  }

  # read TOA/BOA image
  if (n_cores > 1) {
    cl <- makeCluster(n_cores, type = if (Sys.info()["sysname"] == "Windows") {
      "PSOCK"
    } else {
      "FORK"
    })
    registerDoParallel(cl)
    print_message(
      type = "message", 
      date = TRUE, 
      i18n$t("Starting parallel computation of indices..."))
  }
  
  # check bands to use
  if (source == "TOA") {
    gdal_bands <- data.frame(letter = LETTERS[1:12], band = paste0("band_", 1:12), ext = paste0("_TILES_FRE_B", 1:12))
  } else if (source == "BOA") {
    gdal_bands <- data.frame(letter = LETTERS[1:9], band = paste0("band_", c(2:8, 11:12)), ext = paste0("_TILES_FRE_B", c(2:8, 11:12)))
  } else {
    print_message(
      type = "error", 
      i18n$t("Internal error (this should not happen)."))
  }
  
  # CPU
  cpu <- parallel::detectCores()
  
  #### Extent + buffer(500m), Merge and Crop ####
  ext <- extent %>% st_transform(2154) %>% st_buffer(500) %>% st_bbox()
  
  for (i in seq_along(infiles)) {
    # file to process
    sel_infile <- infiles[[i]]["exp"]
    # extract name of indice
    sel_name <- indices_info[i, "name"]
    # extract parameters
    sel_parameters <- parameters[[indices[i]]]
    for (fic in unlist(sel_infile)) {
      # replace "indices" by "tiles"
      fid <- gsub("indices", "tiles", gsub(paste0("_", sel_name, ".tif"), "", fic))
      # change index formula to be used with bands
      for (sel_par in c("a", "b", "x")) {
        assign(sel_par, if (is.null(sel_parameters[[sel_par]])) {
          indices_info[i, sel_par]
        } else {
          sel_parameters[[sel_par]]
        })
      }
      sel_formula <- indices_info[i, "s2_formula"]
      for (sel_par in c("a", "b", "x")) {
        sel_formula <- gsub(paste0("([^0-9a-zA-Z])par\\_", sel_par, "([^0-9a-zA-Z])"), 
                            paste0("\\1", get(sel_par), "\\2"), sel_formula)
      }
      # extract unique bands in sel_formula
      g_bands <- gdal_bands[which(gdal_bands$band %in% unique(unlist(str_extract_all(sel_formula, "band\\_\\d{1,2}")))), ]
      for (sel_band in seq_len(nrow(g_bands))) {
        sel_formula <- gsub(paste0("([^0-9a-zA-Z])", g_bands[sel_band, "band"], 
                                   "([^0-9a-zA-Z])"),
                            paste0("\\1(", g_bands[sel_band, "letter"], ".astype(float)/10000)\\2"), 
                            sel_formula)
      }
      if (grepl("Int", dataType)) {
        sel_formula <- paste0("clip(", scaleFactor, "*(", sel_formula, "),", switch(dataType, Int16 = -2^15 + 2, UInt16 = 0, Int32 = -2^31 + 4, UInt32 = 0), 
                              ",", switch(dataType, Int16 = 2^15 - 1, UInt16 = 2^16 - 2, Int32 = 2^31 - 3, UInt32 = 2^32 - 4), ")")
      }
      sel_nodata <- switch(dataType, Int16 = -2^15, UInt16 = 2^16 - 1, Int32 = -2^31, UInt32 = 2^32 - 1, Float32 = -9999, Float64 = -9999, Byte = 255)
      if (dataType == "Byte") {
        sel_formula <- paste0("clip(100+100*(", sel_formula, "),0,200)")
      }
      
      # path out and format
      sel_format0 <- "GTiff"
      sel_outfile0 <- gsub(paste0(sel_name, ".tif"), paste0(sel_name, "_CALC.tif"), basename(fic))
      out_subdir0 <- gsub(paste0(project, "/"), "", dirname(fic))
      
      # # Processing to calculate indices
      if (!file.exists(file.path(out_subdir0, sel_outfile0)) && length(str_extract_all(file.path(out_subdir0, sel_outfile0), "\\.tif")) == 1) {
        # apply gdal_calc
        cmd <- paste0(binpaths$gdal_calc, " ",
                      paste(apply(g_bands, 1, function(l) {
          paste0("-", l["letter"], " \"", fid, g_bands$ext[which(g_bands$letter == l["letter"])], ".tif\"")
        }), collapse = " "), " ", "--outfile=\"", file.path(out_subdir0, sel_outfile0), "\" ", "--type=\"", dataType, "\" ",
        "--NoDataValue=", sel_nodata,
        " ", "--format=\"", sel_format0, "\" ", if (overwrite == TRUE) {
          "--overwrite "
        }, if (sel_format0 == "GTiff") {
          paste0("--co=\"COMPRESS=", toupper(compress), "\" ")
        }, "--calc=\"", sel_formula, "\"")
        system(cmd, intern = Sys.info()["sysname"] == "Windows")
      }
    } # end for
  } # end for i
  
  # We have 3 UTM zones over France. To merge them, we need to resample in common projection. We chose the French standard projection Lambert 93, epsg=2154 
  for (i in seq_along(infiles)) {
    # indices to process
    sel_infile <- infiles[[i]]["exp"]
    # extract name of indice
    sel_name <- indices_info[i, "name"]
    out_subdir1 <- gsub(paste0(project, "/"), "", dirname(sel_infile[[1]]))
    # verify if any indice *.tif exist
    if (any(!file.exists(unlist(sel_infile)))) {
      cmd <- paste0(
        "parallel -j", cpu, " ",
        binpaths$gdalwarp, " -q -r cubic -t_srs 'EPSG:2154' -tr ",
        resolution, " ", resolution, " ",
        paste0(file.path(out_subdir1, paste0(
          gsub("\\.tif", "_CALC.tif",
               gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(sel_infile[[1]]))
               ))), " "),
        paste0(file.path(out_subdir1, paste0(
          gsub(
            "\\.tif", "_CALC_L93.tif ::: ",
            gsub("\\_T([0-9]{2})[A-Z]", "_{}", basename(sel_infile[[1]]))
          ),
          str_extract(basename(unlist(sel_infile)), "T([0-9]{2})[A-Z]")
        )))
      )
      for (c in seq_along(cmd)) {
        if (!file.exists(gsub(paste0(sel_name, ".tif"), paste0(sel_name, "_CALC_L93.tif"), gsub(paste0(project, "/"), "", unlist(sel_infile)[c])))) {
          system(cmd[c], intern = Sys.info()["sysname"] == "Windows")
        }
      }
    } # end if
  } # end for i
  
  # now we can merge
  for (i in seq_along(infiles)) {
    # indices to process
    sel_infile <- infiles[[i]]["exp"]
    # extract name of indice
    sel_name <- indices_info[i, "name"]
    out_subdir1 <- gsub(paste0(project, "/"), "", dirname(unlist(sel_infile)[1]))
    # list files of indices
    warped <- list.files(out_subdir1, pattern = paste0("*D_", sel_name, "_CALC_L93.tif"))
    if (length(warped) != 0) {
      regx <- unique(paste0(str_extract(basename(warped), "SENTINEL2A\\_([0-9]{8})"), "*_L2A*",
                            str_extract(basename(warped), "\\_T([0-9]{2})[A-Z]")))
      
      # export as a RGB image at full resolution
      for (fic in regx) {
        if (!file.exists(file.path(outdir, paste0(gsub("\\*", "", fic), paste0("_", sel_name, "_CALC_L93_CROP.tif"))))) {
          if (!file.exists(file.path(outdir, paste0(gsub("\\*", "", fic), paste0("_", sel_name, "_CALC_L93_MERGE.tif"))))) {
            cmd <- paste0(
              binpaths$gdal_merge, " -n 0 ",
              file.path(out_subdir1, paste0(fic, paste0("*_", sel_name, "*_CALC_L93.tif -o "))),
              file.path(outdir, paste0(gsub("\\*", "", fic), paste0("_", sel_name, "_CALC_L93_MERGE.tif")))
            )
            system(cmd, intern = Sys.info()["sysname"] == "Windows")
          }
          # crop it to the extent
          cmd <- paste0(
            binpaths$gdalwarp, " -overwrite -dstnodata 0  -te ",
            ext$xmin, " ", ext$ymin, " ", ext$xmax, " ", ext$ymax, " ",
            file.path(outdir, paste0(gsub("\\*", "", fic),  paste0("_", sel_name, "_CALC_L93_MERGE.tif "))),
            file.path(outdir, paste0(gsub("\\*", "", fic), paste0("_", sel_name, "_CALC_L93_CROP.tif")))
          )
          system(cmd, intern = Sys.info()["sysname"] == "Windows")
        }
      }
    }
  } # end of i
  
  # now we can translate to jpeg
  for (i in seq_along(infiles)) {
    # indices to process
    sel_infile <- infiles[[i]]["exp"]
    # extract name of indice
    sel_name <- indices_info[i, "name"]
    out_subdir2 <- dirname(unlist(sel_infile)[1])
    # list files of indices
    croped <- list.files(out_subdir2, pattern = paste0("*_", sel_name, "_CALC_L93_CROP.tif"), full.names = TRUE)
    if (length(croped) != 0) {
      # export as a RGB image at full resolution
      for (fic in croped) {
        if (!file.exists(file.path(paste0(out_subdir2, "/jpg"), gsub("\\.tif", ".jpg", basename(fic))))) {
         raster2rgb(in_rast = fic,
                    out_file = file.path(paste0(out_subdir2, "/jpg"), gsub("\\.tif", ".jpg", basename(fic))),
                    palette = "NDVI"
         )
        }
      }
    }
  } # end of i
  
  #### Remove all non CROP files ####
  out_subdir3 <- dirname(unlist(infiles[[1]]["exp"])[1])
  fl <- grep(list.files(out_subdir3), pattern = "_CALC_L93_CROP.tif", invert = TRUE, value = TRUE)
  if (length(fl) > 1) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Deletes all temporary files.")
    )
    file.remove(paste0(out_subdir3, "/", fl[-1]))
  }

  
  
  if (n_cores > 1) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Parallel computation of indices done."))
  }


  # return(outfiles)
}
