#' @title Find, download and preprocess Sentinel-2 images
#' @description The function is a wrapper to perform the entire
#'  processing chain to find, download and pre-process Sentinel-2
#'  data. Input is a set of parameters that can be passed with a
#'  list or file (parameter `param_list`) or singularly (see the
#'  descriptions of all the other parameters).
#'
#' @param param_list (optional) List of input parameters:
#'  it can be both an R list or the path of a JSON file.
#'  If some parameters are passed both as elements of `param_list`
#'  and as function arguments, the values passed as function
#'  arguments are considered.
#'  If some parameters are missing in `param_list` and are not
#'  provided as arguments, default values will be used.
#'  Use the function [s2_gui()] to create a complete list of
#'  parameters.
#'  If `param_list` is NULL (default), values given with the
#'  parameters below (or default values for parameters not
#'  provided) are used.
#' @param online (optional) Logical: TRUE (default) to search for available
#'  products on SciHub (and download if needed); FALSE to work
#'  only with already downloaded SAFE products.
#' @param api Path of the text file containing credentials
#'  of cnes account. If NA (default) the default credentials
#'  (username "email", password "password") will be used.
#' @param downloader (optional) Character value corresponding to the executable
#'  which should be used to download SAFE products. It could be one among
#'  "wget" (default) and "aria2".
#'  If aria2 is not installed, Wget will be used instead.
#' @param overwrite_product (optional) Logical: TRUE to overwrite existing
#'  products with products found online or manually corrected,
#'  FALSE (default) to skip download for products already existing.
#' @param timewindow (optional) Temporal window for querying: Date object
#'  of length 1 (single day) or 2 (time window). Default is NA, meaning that
#'  no filters are used if online = FALSE, and all found images are processed;
#'  if online = TRUE, last 90 days are processed.
#'  Is it possible to pass also integer (or difftime) values, which are
#'  interpreted as the last n days.
#' @param timeperiod (optional) Character:
#'  * "full" (default) means that all
#'  the images included in the time window are considered;
#'  * "seasonal" means that only the single seasonal periods in the
#'  window are used (i.e., with a time window from 2015-06-01 to
#'  2017-08-31, the periods 2015-06-01 to 2015-08-31, 2016-06-01
#'  to 2016-08-31 and 2017-06-01 to 2017-08-31 are considered).
#' @param extent (optional) Spatial extent on which to clip products (it can
#'  be both the path of a vector file or a geoJSON).
#'  Default is NA for offline mode (meaning no extent:
#'  all found tiles are entirely used); in online mode, a sample extent is used
#'  as default.
#' @param list_rgb (optional) Character vector with the values of the
#'  RGB images to be produced.
#'  Images are in the form xRGBrgb, when:
#'  - x is B (if source is BOA) or T (is source is TOA);
#'  - r g and b are the the number of the bands to be used respectively
#'      for red, green and blue, in hexadecimal format.
#'      Notice that this is the [actual number name of the bands](
#'      https://earth.esa.int/web/sentinel/user-guides/sentinel-2-msi/resolutions/spatial):
#'      so, to use i.e. BOA band 11 (1610nm) use the value "b", even if band 11 is
#'      the 10th band of a BOA product (because band 10 is missing).
#'  Default is no one (NA).
#' @param index_source (optional) Character value: if "BOA" (default), indices
#'  are computed from BOA values; if "TOA", non corrected reflectances
#'  are instead used (be careful to use this setting!).
#' @param mask_type (optional) Character value which determines the categories
#'  in the Surface Classification Map to be masked (see [s2_mask()]
#'  for the accepted values). Default (NA) is not to mask.
#' @param max_mask (optional) Numeric value (range 0 to 100), which represents
#'  the maximum percentage of allowed masked surface (by clouds or any other
#'  type of mask chosen with argument `mask_type`) for producing outputs.
#'  Images with a percentage of masked surface greater than `max_mask`%
#'  are not processed (the list of expected output files which have not been
#'  generated is returned as an attribute, named "skipped").
#'  Default value is 80.
#'  Notice that the percentage is computed on non-NA values (if input images
#'  had previously been clipped and masked using a polygon, the percentage is
#'  computed on the surface included in the masking polygons).
#' @param mask_smooth (optional) Numeric positive value: the smoothing radius
#'  (expressed in unit of measure of the output projection, typically metres)
#'  to be applied to the cloud mask by function [s2_mask].
#' @param mask_buffer (optional) Numeric value: the buffering radius
#'  (expressed in unit of measure of the output projection, typically metres)
#'  to be applied to the cloud mask by function [s2_mask].
#'  Default value (0) means that no buffer is applied; a positive value causes
#'  an enlargement of the masked area; a negative value cause a reducement.
#' @param resampling (optional) Resampling method (one of the values supported
#'  by `gdal_translate`: "near" (default), "bilinear", "cubic",
#'  "cubicspline", "lanczos", "average" or "mode").
#' @param resampling_scl (optional) Resampling method for categorical products
#'  (for now, only SCL): one among "near" (default) and "mode".
#' @param outformat (optional) Format of the output file (in a
#'  format recognised by GDAL). Default is "GTiff".
#' @param rgb_outformat (optional) Format of the output RGB products (in a
#'  format recognised by GDAL). Default is "GTiff".
#' @param index_datatype (optional) Numeric datatype of the ouptut
#'  spectral indices.
#' @param compression (optional) In the case GTiff is chosen as
#'  output format, the compression indicated with this parameter is
#'  used (default is "DEFLATE").
#' @param rgb_compression (optional) In the case GTiff is chosen as
#'  output format for RGB products, the compression indicated
#'  with this parameter is used (default is "DEFLATE").
#'  In the cases GTiff or JPEG are chosen as output format for RGB products,
#'  this parameter can also be a 1-100 integer value, which is interpreted
#'  as the compression level for a JPEG compression.
#' @param overwrite (optional) Logical value: should existing output
#'  files be overwritten? (default: FALSE).
#' @param path_theia_landsat (optional) Path of the directory in which THEIA LANDSAT
#'  products are searched and/or downloaded. If not provided (default), a
#'  temporary directory is used.
#' @param path_theia_spotworldheritage (optional) Path of the directory in which THEIA SpotWorldHeritage
#'  products are searched, downloaded and/or generated. If not provided
#'  (default), a temporary directory is used.
#' @param path_theia_sentinel2 (optional) Path of the directory in which THEIA Sentinel2
#'  products are searched and/or downloaded. If not provided (default), a
#'  temporary directory is used.
#' @param path_theia_snow (optional) Path of the directory in which THEIA Snow
#'  products are searched, downloaded and/or generated. If not provided
#'  (default), a temporary directory is used.
#' @param path_theia_venus (optional) Path of the directory in which THEIA VENUS
#'  products are searched, downloaded and/or generated. If not provided
#'  (default), a temporary directory is used.
#' @param path_out (optional) Path of the directory in which Sentinel-2
#'  output products are searched and/or generated.
#'  If not provided (default), a temporary directory is used.
#' @param path_rgb (optional) Path of the directory in RGB products
#'  are searched and/or generated.
#'  If not provided (default), `path_out` is used.
#' @param path_indices (optional) Path of the directory in which files of
#' spectral indices are searched and/or generated.
#'  If not provided (default), `path_out` is used.
#' @param thumbnails (optional) Logical: if TRUE (default), a thumbnail is
#'  added for each product created. Thumbnails are JPEG or PNG georeferenced
#'  small images (width or height of 1024 pixels) with default colour palettes
#'  (for more details, see the help window in the GUI). They are placed in
#'  a subdirectory of the products names "thumbnails".
#'  If FALSE, they are not created.
#' @param parallel (optional) Logical or integer: if TRUE (default), some
#'  functions ([sen2cor], [s2_mask] and [s2_calcindices] for now)
#'  are executed using multiple cores in order to speed up the execution.
#'  The number of cores is automatically determined; specifying it is also
#'  possible (e.g. `parallel = 4`).
#'  If FALSE, the processing chain is forced to run with a single core
#'  (this can be useful if multiple [cnes] instances are run in parallel).
#'  This argument can be set only in commandline mode, not using the GUI.
#' @param use_python (optional) Logical: if TRUE (default), the presence of
#'  python in the system is checked before running the function;
#'  if FALSE, this is skipped. Setting this to FALSE can bge useful on
#'  systems with problems with python, when [theia2r()] is intended
#'  to be used only for processing existing SAFE files (python is required
#'  in any case to download SAFE).
#' @param tmpdir (optional) Path where intermediate files will be created.
#'  Default is a temporary directory (unless `outformat = "VRT"`: in this case,
#'  default is a subdirectory named ".vrt" within `path_out`).
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE). `rmtmp` is forced to `FALSE` if `outformat = "VRT"`.
#' @param log (optional) Character string with the path where the package
#'  messages will be redirected.
#'  Default (NA) is not to redirect (use standard output).
#'  A two-length character with tho paths (which can also coincide)
#'  can be used to redirect also the output: in this case, the first path
#' @param product Product
#' @param theiacollection Theia collection
#' @param pepscollection Peps collection
#' @param theiaplatformlandsat Platform THEIA landsat
#' @param theiaplatformspotworldheritage Platform THEIA Spot World Heritage
#' @param theiaplatformsentinel Platform THEIA SENTINEL 2
#' @param theiaplatformvenus Platform THEIA VENUS
#' @param extent_name Extent name
#' @param list_indices_checked List indices checked
#' @param reference_path
#'  is the path for messages, the second one for the output.
#' @return A vector with the paths of the files which were created (excluded
#'  the temporary files); NULL otherwise.
#'  The vector includes two attributes:
#'  - `cloudcovered` with the list of imags not created due to the higher
#'      percentage of cloud covered pixels;
#'  - `missing` with the list of imags not created due to other reasons.
#'
#' @import data.table
#' @importFrom utils packageVersion
#' @importFrom geojsonio geojson_json
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_cast st_read st_combine st_as_sf st_is_valid
#' @importFrom methods formalArgs
#' @importFrom stats na.omit
#' @importFrom stringr str_extract
#' @importFrom tools file_path_sans_ext
#' @export
#'
preprocessing <- function(param_list = NULL) {
  .preprocessing(
    param_list = param_list,
    par_fun = "parent"
  )
}

# Internal function, which is the "real" preprocessing() function insider the use of sink
# (this workaround was used in order to manage final sink() in those cases
# in which return() is used inside the function.)
# TODO: manage also errors (.preprocessing inside a trycatch; in case of errors, stop
# passing the error message)
.preprocessing <- function(param_list = NULL,
                  parallel = TRUE,
                  use_python = TRUE,
                  tmpdir = NA,
                  rmtmp = TRUE,
                  par_fun = "parent") {
  # Starting execution
  print_message(
    type = "message",
    date = TRUE,
    i18n$t("Starting session CNES execution")
  )

  ##### 1. Read / import parameters #####

  # Import param_list, if provided
  pm_list <- if (is(param_list, "character")) {
    # load json parameter file
    jsonlite::fromJSON(param_list)
    # TODO check package version and parameter names
  } else if (is(param_list, "list")) {
    param_list
    # TODO check parameter names
  } else {
    list("pkg_version" = packageVersion("shinycnes"))
  }

  ## Check consistency of parameters
  # TODO work in progress
  pm <- check_param_list(pm_list, type = "error", correct = TRUE)

  # convert from GeoJSON to sf
  if (is(pm$extent, "character") | is(pm$extent, "geojson")) {
    pm$extent <- st_read(pm$extent, quiet = TRUE)
  } else if (is(pm$extent, "Spatial")) {
    pm$extent <- st_as_sf(pm$extent)
  }

  # check parallel (workaroud)
  # TODO add parallel to the GUI, and threat as a normal parameter
  if (is.null(pm$parallel)) {
    pm$parallel <- parallel
  }

  # define and create tmpdir
  if (is.na(tmpdir)) {
    # if outformat is VRT, set as a subdirectory of path_out
    tmpdir <- if (pm$outformat == "VRT" &
      !all(is.na(pm[c(
        "path_data",
        "path_out",
        "path_rgb",
        "path_indices",
        "path_tiles",
        "path_translate",
        "path_mosaic",
        "path_merged"
      )]))) {
      # use path_out if it is not NA, otherwise path_indices, otherwise path_tiles, otherwise path_merged
      main_dir <-
        unlist(pm[c(
          "path_data",
          "path_out",
          "path_rgb",
          "path_indices",
          "path_tiles",
          "path_translate",
          "path_mosaic",
          "path_merged"
        )])[!is.na(pm[c(
          "path_data",
          "path_out",
          "path_rgb",
          "path_indices",
          "path_tiles",
          "path_translate",
          "path_mosaic",
          "path_merged"
        )])][1]
      dir.create(main_dir, showWarnings = FALSE)
      file.path(main_dir, ".vrt")
    } else {
      tempfile(pattern = "shinycnes_")
    }
  }
  if (pm$outformat == "VRT") {
    rmtmp <- FALSE # force not to remove intermediate files
  }

  dir.create(tmpdir, showWarnings = FALSE)

  # Temporay execution
  print_message(
    type = "message",
    date = TRUE,
    i18n$t("Temporary dir is created")
  )

  # internal parameters
  paths <- c()
  paths["data"] <- if (!is.na(pm$path_data)) {
    pm$path_data
  } else {
    file.path(tmpdir, "data")
  }
  paths["out"] <- if (!is.na(pm$path_out)) {
    pm$path_out
  } else {
    file.path(tmpdir, "out")
  }
  paths["translate"] <- if (!is.na(pm$path_translate)) {
    pm$path_translate
  } else {
    file.path(tmpdir, "translate")
  }
  paths["rgb"] <- if (!is.na(pm$path_rgb)) {
    pm$path_rgb
  } else {
    file.path(tmpdir, "rgb")
  }
  paths["indices"] <- if (!is.na(pm$path_indices)) {
    pm$path_indices
  } else {
    file.path(tmpdir, "indices")
  }
  paths["tiles"] <- if (!is.na(pm$path_tiles)) {
    pm$path_tiles
  } else {
    file.path(tmpdir, "tiles")
  }
  paths["merged"] <- if (!is.na(pm$path_merged)) {
    pm$path_merged
  } else {
    file.path(tmpdir, "merged")
  }
  paths["mosaic"] <- if (!is.na(pm$path_mosaic)) {
    pm$path_mosaic
  } else {
    file.path(tmpdir, "mosaic")
  }
  paths["warped"] <- if (!is.na(pm$path_warped)) {
    pm$path_warped
  } else {
    file.path(tmpdir, "warped")
  }

  # check that output parent directories exist, and create required paths
  parent_paths <- sapply(
    pm[c(
      "path_project",
      "path_data",
      "path_tiles",
      "path_merged",
      "path_mosaic",
      "path_translate",
      "path_out",
      "path_rgb",
      "path_warped",
      "path_indices"
    )],
    function(x) {
      if (is.na(x)) {
        NA
      } else {
        dirname(x)
      }
    }
  ) %>% unique() %>% na.omit() %>% as.character()

  # Check path execution
  print_message(
    type = "message",
    date = TRUE,
    i18n$t("Work directories have been created")
  )

  paths_exist <- sapply(parent_paths, file.exists)
  if (any(!paths_exist)) {
    print_message(
      type = "error",
      "The following output ",
      if (sum(!paths_exist) == 1) {
        "directory does "
      } else {
        "directories do "
      },
      "not exist:\n",
      paste(names(paths_exist[!paths_exist]), collapse = "\n"),
      ".\nPlease create ",
      if (sum(!paths_exist) == 1) {
        "it "
      } else {
        "them "
      },
      "before continuing."
    )
  } # end if

  if (pm$product == "theia") {
    sapply(
      pm[c(
        "path_project",
        "path_data",
        "path_tiles",
        "path_merged",
        "path_translate",
        "path_out",
        "path_mosaic",
        "path_rgb",
        "path_warped",
        "path_indices"
      )],
      function(x) {
        if (is.na(x)) {
          NA
        } else {
          dir.create(x, recursive = FALSE, showWarnings = FALSE)
        }
      }
    )
  }

  # check output format
  gdal_formats <-
    fromJSON(system.file("extdata", "gdal_formats.json", package = "shinycnes"))
  sel_driver <- gdal_formats[gdal_formats$name == pm$outformat, ]
  if (is.null(pm$rgb_outformat)) {
    pm$rgb_outformat <- pm$outformat
  } # to avoid errors
  if (is.null(pm$rgb_compression)) {
    pm$rgb_compression <- pm$compression
  } # to avoid errors
  sel_rgb_driver <-
    gdal_formats[gdal_formats$name == pm$rgb_outformat, ]

  # if (is.null(py_to_r(sel_driver))) {
  if (nrow(sel_driver) == 0) {
    print_message(
      type = "error",
      "Format \"",
      pm$outformat,
      "\" is not recognised; ",
      "please use one of the formats supported by your GDAL installation.\n\n",
      "To list them, use the following command:\n",
      "gdalUtils::gdalinfo(formats=TRUE)\n\n",
      "To search for a specific format, use:\n",
      "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]"
    )
  }
  if (nrow(sel_rgb_driver) == 0) {
    print_message(
      type = "error",
      "Format \"",
      pm$rgb_outformat,
      "\" is not recognised; ",
      "please use one of the formats supported by your GDAL installation.\n\n",
      "To list them, use the following command:\n",
      "gdalUtils::gdalinfo(formats=TRUE)\n\n",
      "To search for a specific format, use:\n",
      "gdalinfo(formats=TRUE)[grep(\"yourformat\", gdalinfo(formats=TRUE))]"
    )
  }
  # define output extension
  main_ext <- sel_driver[1, "ext"]

  ##### 2. THEIA Part (find, download, correct) #####

  # if product is required, define output formats
  if (pm$product == "theia") {
    ## Define output formats
    if (!anyNA(pm$list_prods)) {
      out_ext <- main_ext
      out_outformat <- pm$outformat
    } else {
      out_ext <- "vrt"
      out_outformat <- "VRT"
    }
    if (!is.na(pm$path_data)) {
      tiles_ext <- main_ext
      tiles_outformat <- pm$outformat
    } else {
      tiles_ext <- "vrt"
      tiles_outformat <- "VRT"
    }
    if (!is.na(pm$path_tiles)) {
      tiles_ext <- main_ext
      tiles_outformat <- pm$outformat
    } else {
      tiles_ext <- "vrt"
      tiles_outformat <- "VRT"
    }
    if (!is.na(pm$path_mosaic)) {
      mosaic_ext <- main_ext
      mosaic_outformat <- pm$outformat
    } else {
      mosaic_ext <- "vrt"
      mosaic_outformat <- "VRT"
    }
    if (!is.na(pm$path_translate)) {
      translate_ext <- main_ext
      translate_outformat <- pm$outformat
    } else {
      translate_ext <- "vrt"
      translate_outformat <- "VRT"
    }
    if (!is.na(pm$path_merged)) {
      merged_ext <- main_ext
      merged_outformat <- pm$outformat
    } else {
      merged_ext <- "vrt"
      merged_outformat <- "VRT"
    }
    if (is.na(pm$mask_type)) {
      warped_ext <- out_ext
      warped_outformat <- out_outformat
    } else {
      warped_ext <- "vrt"
      warped_outformat <- "VRT"
    }
    if (pm$index_source %in% pm$list_prods) {
      sr_masked_ext <- main_ext
      sr_masked_outformat <- pm$outformat
    } else {
      sr_masked_ext <- "vrt"
      sr_masked_outformat <- "VRT"
    }
  }

  ##### 3. Find PEPS or THEIA and compute the names of required files #####
  for (dummy in TRUE) {
    # dummy cycle, created only to allow "break" from this part    
    cnes_lists <- list()
    cnes_lists_downloaded <- list()
  
    if (pm$online == TRUE) {
      if (pm$product == "theia") {
        print_message(
          type = "message",
          date = TRUE,
          i18n$t("Searching for available THEIA products on theia-land")
        )
      } else {
        print_message(
          type = "message",
          date = TRUE,
          i18n$t("Searching for available PEPS products on peps")
        )
      }
  
      # if online mode, retrieve list with cnes_list() basing on parameters
      if (pm$product == "theia") {
        api <- pm$apitheia
      } else {
        api <- pm$apipeps
      }
  
      if ("sentinel2" %in% pm$theiacollection) {
        # list of THEIA needed for required Sentinel2 level
        cnes_lists <- cnes_list(
          spatial_extent = pm$extent,
          time_interval = pm$timewindow,
          time_period = pm$timeperiod,
          level = pm$theiaplatformsentinellevel,
          platform = pm$theiaplatformsentinel,
          maxcloud = pm$max_mask,
          collection = pm$theiacollection,
          api = api
        )
      }
    } else {
      # if offline mode, read the PEPS or THEIA product list from folders and filter
      cnes_lists <- list.dirs(path = paths["data"])
    } # end of if
  
    cnes_list <-
      stringr::str_extract(
        grep("SENTINEL2A", cnes_lists, value = TRUE),
        def_regex$tile$regex
      )
    rm(cnes_lists)
    
    # compute names for required files
    print_message(type = "message", date = TRUE, i18n$t("Computing output names"))
    
    cnes2names <- compute_cnes2_paths(
      pm = pm,
      cnes_list = cnes_list,
      force_tiles = TRUE
    )
  
    # if cnes_list is empty, exit
    if (length(cnes_list) == 0) {
      if (pm$product == "theia") {
        print_message(
          type = "message",
          date = TRUE,
          i18n$t("No THEIA products found with the parameters set "),
          i18n$t("(the searching parameters may be too restrictive, "),
          i18n$t("or the theia-land Access could be unavailable).")
        )
      } else {
        print_message(
          type = "message",
          date = TRUE,
          i18n$t("No PEPS products found with the parameters set "),
          i18n$t("(the searching parameters may be too restrictive, "),
          i18n$t("or the Peps Access could be unavailable).")
        )
      }
      break
    }
  
    # If cnes_list is empty, exit
    if (length(cnes_list) == 0) {
      if (pm$product == "theia") {
        print_message(
          type = "message",
          date = TRUE,
          if (pm$online == FALSE) {
            paste0(
              i18n$t(
                "No THEIA products which match the settings were found locally;\n "
              ),
              i18n$t(
                "please download them or set different spatial/temporal extents.\n"
              ),
              i18n$t("Execution halted.")
            )
          } else {
            paste0(i18n$t("No THEIA products matching the settings were found (1)."))
          }
        )
      } else {
        print_message(
          type = "message",
          date = TRUE,
          if (pm$online == FALSE) {
            paste0(
              "No PEPS products which match the settings were found locally;\n ",
              "please download them or set different spatial/temporal extents.\n",
              "Execution halted."
            )
          } else {
            paste0("No PEPS products matching the settings were found (1).")
          }
        )
      }
      break
    } else {
      if (pm$product == "theia") {
        print_message(
          type = "message",
          date = TRUE,
          paste0(
            length(cnes_list),
            i18n$t("\u2000THEIA products are found on Theia-land.")
          )
        )
      } else {
        print_message(
          type = "message",
          date = TRUE,
          paste0(
            length(cnes_list),
            i18n$t("\u2000PEPS products are found on PEPS Hub.")
          )
        )
      }
    }
  
    # Check if processing is needed
    if (all(sapply(cnes2names[c(
      "indices_names_new", "rgb_names_new", "out_names_new", "masked_names_new",
      "warped_names_new", "merged_names_new", "tiles_names_new", "mosaic_names_new"
    )], length) == 0)) {
      print_message(
        type = "message",
        date = TRUE,
        "All the required output files already exist; nothing to do.\n ",
        "To reprocess, run theia2r() with the argument overwrite = TRUE\n ",
        "or specify a different output directory."
      )
      return(invisible(NULL))
    }
  } # end of dummy cycle
  

  ##### 4. Download required THEIA #####
  # TODO implement ovwerite/skip
  # (now it skips, but analysing each single file)

  if (pm$online == TRUE) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t(
        "Starting to download the required level THEIA Sentinel2 products."
      )
    )

    cnes_lists_downloaded[["sentinel2"]] <- product_download(
      spatial_extent = pm$extent,
      time_interval = pm$timewindow,
      time_period = pm$timeperiod,
      level = pm$theiaplatformsentinellevel,
      platform = pm$theiaplatformsentinel,
      maxcloud = pm$max_mask,
      collection = pm$theiacollection,
      writedir = pm$path_data,
      api = api
    )

    cnes_list_downloaded <-
      stringr::str_extract(
        grep("already exists", cnes_lists_downloaded[[1]], value = TRUE),
        def_regex$tile$regex
      )
    rm(cnes_lists_downloaded)

    # If cnes_list is empty, exit
    if (length(cnes_list_downloaded) == 0) {
      if (pm$product == "theia") {
        print_message(
          type = "message",
          date = TRUE,
          paste0(
            length(cnes_list),
            i18n$t("\u2000THEIA products have been dowloaded on Theia-land.")
          )
        )
      } else {
        print_message(
          type = "message",
          date = TRUE,
          paste0(
            length(cnes_list),
            i18n$t("\u2000PEPS products have been downloaded on PEPS Hub.")
          )
        )
      }
    } else {
      if (pm$product == "theia") {
        print_message(
          type = "message",
          date = TRUE,
          paste0(
            length(cnes_list_downloaded),
            i18n$t("\u2000THEIA products already exist and have not been downloaded.")
          )
        )
      } else {
        print_message(
          type = "message",
          date = TRUE,
          paste0(
            length(cnes_list_downloaded),
            i18n$t("\u2000PEPS products already exist and have not been downloaded.")
          )
        )
      }
    }

    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Download of level THEIA Sentinel2 products terminated.")
    )

    # unzip files
    zipfile <- c(paste0(cnes_list, ".zip"))
    for (i in 1:length(zipfile)) {
      print(paste0(pm$path_data, "/", stringr::str_sub(file_path_sans_ext(zipfile[i]), end = -3)))
      tryCatch(if (!dir.exists(paste0(pm$path_data, "/", stringr::str_sub(file_path_sans_ext(zipfile[i]), end = -3)))) {
        print("pouet01")
        # list all files in zipfile
        li <- utils::unzip(
          zipfile = paste0(pm$path_data, "/", zipfile[i]),
          list = TRUE
        )
        print("pouet02")
        # unzip just FRE_ and ALL.jpg files
        utils::unzip(
          zipfile = paste0(pm$path_data, "/", zipfile[i]),
          exdir = paste0(pm$path_data, "/", stringr::str_sub(file_path_sans_ext(zipfile[i]), end = -3)),
          files = li$Name[grep('FRE_|ALL\\.jpg', li$Name)],
          junkpaths = TRUE,
          overwrite = FALSE
        )
        print_message(
          type = "message",
          date = TRUE,
          paste(i18n$t("Zip file"), zipfile[i], i18n$t("is uncompressed."))
        )
      },
      error = function(e) {
        print_message(
          type = "message",
          date = TRUE,
          paste(i18n$t("Zip file"), zipfile[i], i18n$t("is not uncompressed."))
        )
        },
      warning = function(w) {
        print_message(
          type = "message",
          date = TRUE,
          paste(i18n$t("Zip file"), zipfile[i], i18n$t("is not uncompressed."))
        )
      }) # end of tryCtach
    } # end of for

    # copy cnes2names$tiles_names_exp in project/tiles
    # find the files that i want
    oldf <- list.files(
      file.path(
        paste0(pm$path_data, "/", cnes_list)
      ),
      "\\.tif",
      full.names = T
    )
    if (length(oldf) > 0) {
      print_message(
        type = "message",
        date = TRUE,
        paste(length(oldf), i18n$t("tiles files are copied in tiles directory, it take times..."))
      )
      newf <- file.path(
        pm$path_tiles,
        basename(gsub("_[A-Z]_V[0-9].[0-9]_", "_TILES_", oldf))
      )
      file.copy(
        from = oldf,
        to = newf,
        overwrite = FALSE,
        recursive = FALSE,
        copy.mode = TRUE
      )
    } # end of if
  } # end of if of online

  ### GDAL processing: convert THEIA, merge tiles, warp, mask and compute indices ###

  ##### 5. Mosaic by orbit #####
  cnes_lists_mosaic <- list()
  if (sum(!file.exists(nn(cnes2names$mosaic_names_new))) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Starting to mosaic tiles by date and orbit.")
    )

    dir.create(paths["mosaic"], recursive = FALSE, showWarnings = FALSE)

    cnes_lists_mosaic[["cnes"]] <- s2_mosaic(
      infiles = cnes2names$mosaic_names_new,
      cnes_list = cnes_list,
      tilesdir = paths["tiles"],
      outdir = paths["mosaic"],
      parallel = TRUE
    )
  }

  ##### 6. Translate by orbit #####
  cnes_lists_translate <- list()

  if (sum(!file.exists(nn(cnes2names$translate_names_new))) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Starting to translate tiles, export as TIF full resolution.")
    )

    dir.create(paths["translate"], recursive = FALSE, showWarnings = FALSE)

    cnes_lists_translate[["cnes"]] <- s2_translate(
      infiles = cnes2names$translate_names_new,
      tilesdir = paths["tiles"],
      mosaicdir = paths["mosaic"],
      outdir = paths["translate"]
    )
  }

  ##### 7. Warpe #####
  cnes_lists_warped <- list()
  if (sum(!file.exists(nn(cnes2names$warped_names_new))) > 0) {
    ## Rescale, reproject ##
    if (pm$clip_on_extent == TRUE) {
      print_message(
        type = "message",
        date = TRUE,
        i18n$t("Starting to edit geometry (reproject, rescale).")
      )

      dir.create(paths["warped"], recursive = FALSE, showWarnings = FALSE)

      cnes_lists_warped[["cnes"]] <- s2_warped(
        infiles = cnes2names$warped_names_new,
        tilesdir = paths["tiles"],
        translatedir = paths["translate"],
        outdir = paths["warped"]
      )
    }
  }


  ##### 8. Merge #####
  cnes_lists_merged <- list()
  if (sum(!file.exists(nn(cnes2names$merged_names_new))) > 0) {
    ## Merge ##
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Starting to edit geometry (merge).")
    )

    dir.create(paths["merged"], recursive = FALSE, showWarnings = FALSE)

    cnes_lists_merged[["cnes"]] <- s2_merge(
      infiles = cnes2names$merged_names_new,
      extent = pm$extent,
      tilesdir = paths["tiles"],
      warpedir = paths["warped"],
      outdir = paths["merged"]
    )
  }



  # ##### 9. Apply mask #####
  # #   # FIXME understand if this should be done before warping (if so, how to manage virtual/physical files?)
  # #   # masked_names <- file.path(paths["out"],
  # #   #                           if(pm$path_subdirs==TRUE){basename(dirname(warped_names[!names_merged_exp_scl_idx]))}else{""},
  # #   #                           gsub(paste0(warped_ext,"$"),out_ext,basename(warped_names[!names_merged_exp_scl_idx])))
  # #
  # #   if (!is.na(pm$mask_type) & length(s2names$masked_names_new) > 0) {
  # #     print_message(
  # #       type = "message",
  # #       date = TRUE,
  # #       "Starting to apply cloud masks."
  # #     )
  # #
  # #     # index which is TRUE for SCL products, FALSE for others
  # #     names_warped_exp_scl_idx <- theia2r_getElements(s2names$warped_names_exp, format = "data.frame")$prod_type == "SCL"
  # #     names_warped_req_scl_idx <- theia2r_getElements(s2names$warped_names_req, format = "data.frame")$prod_type == "SCL"
  # #     # index which is TRUE for products to be atm. masked, FALSE for others
  # #     names_warped_tomask_idx <- if ("SCL" %in% pm$list_prods) {
  # #       names_warped_req_scl_idx > -1
  # #     } else {
  # #       !names_warped_req_scl_idx
  # #     }
  # #
  # #     # if SR outformat is different (because BOA was not required,
  # #     # bur some indices are) launch s2_mask separately
  # #     masked_names_infiles <- if (pm$clip_on_extent == TRUE) {
  # #       s2names$warped_names_req[names_warped_tomask_idx & file.exists(s2names$warped_names_req)]
  # #     } else {
  # #       s2names$merged_names_req[names_merged_tomask_idx & file.exists(s2names$merged_names_req)]
  # #     }
  # #     masked_names_infiles_sr_idx <- any(!is.na(pm$list_indices)) &
  # #       !pm$index_source %in% pm$list_prods &
  # #       sapply(masked_names_infiles, function(x) {
  # #         theia2r_getElements(x)$prod_type == pm$index_source
  # #       })
  # #
  # #     masked_names_out_nsr <- if (length(masked_names_infiles[!masked_names_infiles_sr_idx]) > 0) {
  # #       trace_function(
  # #         s2_mask,
  # #         infiles = masked_names_infiles[!masked_names_infiles_sr_idx],
  # #         maskfiles = if (pm$clip_on_extent == TRUE) {
  # #           s2names$warped_names_exp[names_warped_exp_scl_idx]
  # #         } else {
  # #           s2names$merged_names_exp[names_merged_exp_scl_idx]
  # #         },
  # #         smooth = pm$mask_smooth,
  # #         buffer = pm$mask_buffer,
  # #         mask_type = pm$mask_type,
  # #         max_mask = pm$max_mask,
  # #         outdir = paths["out"],
  # #         tmpdir = file.path(tmpdir, "s2_mask"), rmtmp = rmtmp,
  # #         format = out_outformat,
  # #         compress = pm$compression,
  # #         subdirs = pm$path_subdirs,
  # #         overwrite = pm$overwrite,
  # #         parallel = pm$parallel,
  # #         .log_message = .log_message, .log_output = .log_output,
  # #         trace_files = s2names$out_names_new
  # #       )
  # #     } else {
  # #       character(0)
  # #     }
  # #     masked_names_out_sr <- if (length(masked_names_infiles[masked_names_infiles_sr_idx]) > 0) {
  # #       trace_function(
  # #         s2_mask,
  # #         infiles = masked_names_infiles[masked_names_infiles_sr_idx],
  # #         maskfiles = if (pm$clip_on_extent == TRUE) {
  # #           s2names$warped_names_exp[names_warped_exp_scl_idx]
  # #         } else {
  # #           s2names$merged_names_exp[names_merged_exp_scl_idx]
  # #         },
  # #         mask_type = pm$mask_type,
  # #         smooth = pm$mask_smooth,
  # #         buffer = pm$mask_buffer,
  # #         max_mask = pm$max_mask,
  # #         outdir = paths["out"],
  # #         tmpdir = file.path(tmpdir, "s2_mask"), rmtmp = rmtmp,
  # #         format = sr_masked_outformat,
  # #         compress = pm$compression,
  # #         subdirs = pm$path_subdirs,
  # #         overwrite = pm$overwrite,
  # #         parallel = pm$parallel,
  # #         .log_message = .log_message, .log_output = .log_output,
  # #         trace_files = s2names$out_names_new
  # #       )
  # #     } else {
  # #       character(0)
  # #     }
  # #     masked_names_out <- c(masked_names_out_nsr, masked_names_out_sr)
  # #     masked_names_notcreated <- c(
  # #       attr(masked_names_out_nsr, "toomasked"),
  # #       attr(masked_names_out_sr, "toomasked")
  # #     )
  # #   }
  # # } # end of gdal_warp and s2_mask IF cycle
  # #
  # #
  ##### 10. Create RGB products #####
  cnes_lists_rgb <- list()
  if (sum(!file.exists(nn(cnes2names$rgb_names_new))) > 0) {
    ## RGB ##
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Starting to make RGB.")
    )

    dir.create(paths["rgb"], recursive = FALSE, showWarnings = FALSE)

    cnes_lists_rgb[["cnes"]] <- s2_rgb(
      infiles = cnes2names$rgb_names_new,
      project = pm$project_name,
      rgblist = pm$rgb_out,
      extent = pm$extent,
      tilesdir = paths["tiles"],
      mosaicdir = paths["mosaic"],
      outdir = paths["rgb"],
      resolution = 10
    )
  }

  ##### 11. Compute spectral indices #####
  cnes_lists_indices <- list()
  if (sum(!file.exists(unlist(nn(cnes2names$indices_names_list)))) > 0) {
    print_message(
      type = "message",
      date = TRUE,
      i18n$t("Computing required spectral indices.")
    )

    dir.create(paths["indices"], recursive = FALSE, showWarnings = FALSE)

    cnes_lists_indices[["cnes"]] <- s2_calcindices(
      infiles = cnes2names$indices_names_list,
      extent = pm$extent,
      indices = pm$list_indices,
      project = pm$project_name,
      outdir = paths["indices"],
      subdirs = pm$path_subdirs,
      source = pm$index_source,
      format = pm$outformat,
      dataType = pm$index_datatype,
      compress = pm$compression,
      overwrite = pm$overwrite,
      parallel = TRUE,
    )
  }

  # ##### 12. create thumbnails #####
  # 
  # if (thumbnails == TRUE) {
  #   thumb_names_req <- names_out_created
  # 
  #   if (length(thumb_names_req) > 0) {
  #     print_message(
  #       type = "message",
  #       date = TRUE,
  #       "Generating thumbnails."
  #     )
  # 
  #     # define expected output names
  #     thumb_names_new <- file.path(
  #       dirname(thumb_names_req),
  #       "thumbnails",
  #       sapply(
  #         basename(thumb_names_req),
  #         function(x) {
  #           gsub(
  #             "\\..+$",
  #             if (theia2r_getElements(x)$prod_type %in% c("SCL")) {
  #               ".png"
  #             } else {
  #               ".jpg"
  #             },
  #             x
  #           )
  #         }
  #       )
  #     )
  # 
  #     thumb_names_out <- trace_function(
  #       s2_thumbnails,
  #       infiles = thumb_names_req,
  #       tmpdir = file.path(tmpdir, "s2_thumbnails"), rmtmp = rmtmp,
  #       trace_files = c(thumb_names_new, paste0(thumb_names_new, ".aux.xml"))
  #     )
  #   }
  # } # end of thumbnails IF cycle


  # ##### 13. remove temporary files #####
  # # if (rmtmp == TRUE) {
  # #   unlink(tmpdir, recursive = TRUE)
  # # }
  # #
  # # # check if some files were not created
  # # names_cloudcovered <- nn(c(
  # #   if (exists("masked_names_notcreated")) {
  # #     masked_names_notcreated
  # #   },
  # #   if (exists("indices_names_notcreated")) {
  # #     indices_names_notcreated
  # #   }
  # # ))
  # # names_missing <- names_out[!file.exists(nn(names_out))]
  # # names_missing <- names_missing[!names_missing %in% names_cloudcovered]
  # # names_cloudcovered <- names_cloudcovered[!grepl(tmpdir, names_cloudcovered, fixed = TRUE)]
  # #
  # # # Add attributes related to files not created
  # # attr(names_out_created, "cloudcovered") <- names_cloudcovered
  # # attr(names_out_created, "missing") <- names_missing
  # #
  # # # Note down the list of non created files (#ignorePath)
  # # # Sometimes not all the output files are correctly created:
  # # # the main reason is the cloud coverage higher than the maximum allowed
  # # # value (argument "max_mask"), but also some other unexpected reasons could
  # # # happen, i.e. because of old name SAFE products which do not include all the tiles.
  # # # To prevent to try to create these files every time the function is called
  # # # with the same parameter file, if param_list is a path, this list is noted
  # # # in two hidden files ( one per file not created because of cloud coverage,
  # # # one other for all the other reasons) so to ignore them during next executions.
  # # # To try it again, delete the files or set overwrite = TRUE).
  # # if (length(names_missing) > 0) {
  # #   ignorelist_path <- gsub("\\.json$", "_ignorelist.txt", param_list)
  # #   if (is(param_list, "character")) {
  # #     write(names_missing, ignorelist_path, append = TRUE)
  # #   }
  # #   print_message(
  # #     type = "warning",
  # #     "Some files were not created:\n\"",
  # #     paste(names_missing, collapse = "\"\n\""), "\"",
  # #     if (is(param_list, "character")) {
  # #       paste0(
  # #         "\"\nThese files will be skipped during next executions ",
  # #         "from the current parameter file (\"", param_list, "\").\n",
  # #         "To try again to build them, remove the file \"",
  # #         ignorelist_path, "\"."
  # #       )
  # #     }
  # #   )
  # # }
  # # if (length(names_cloudcovered) > 0) {
  # #   cloudlist_path <- gsub("\\.json$", "_cloudlist.txt", param_list)
  # #   if (is(param_list, "character")) {
  # #     write(names_cloudcovered, cloudlist_path, append = TRUE)
  # #   }
  # #   print_message(
  # #     type = "message",
  # #     "Some files were not created ",
  # #     "because the cloud coverage was higher than \"max_mask\":\n\"",
  # #     paste(names_cloudcovered, collapse = "\"\n\""), "\"",
  # #     if (is(param_list, "character")) {
  # #       paste0(
  # #         "\"\nThe list of these files was written in a hidden file ",
  # #         "(\"", cloudlist_path, "\"), ",
  # #         "so to be skipped during next executions."
  # #       )
  # #     }
  # #   )
  # # }
  # 
  # 
  # 
  #### Exit ####
  print_message(
    type = "message",
    date = TRUE,
    i18n$t("Execution of CNES session terminated.")
  )

}
