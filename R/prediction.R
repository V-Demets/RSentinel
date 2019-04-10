#' @title Prediction with preprocess Sentinel-2 images calculated
#' @description The function is a wrapper to perform the entire
#'  processing chain to find, download and pre-process Sentinel-2
#'  data and produce prediction of Presence/Absence of a phenome.
#'  Input is a set of parameters that can be passed with a
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
#' @param path_indices (optional) Path of the directory in which files of
#' spectral indices are searched and/or generated.
#'  If not provided (default), `path_out` is used.
#' @param sigma (optional) Default value 0.999, this is the value for extract
#' point from raster file predict.
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
prediction <- function(param_list = NULL) {
  .prediction(
    param_list = param_list,
    par_fun = "parent"
  )
}

# Internal function, which is the "real" prediction() function insider the use of sink
# (this workaround was used in order to manage final sink() in those cases
# in which return() is used inside the function.)
# TODO: manage also errors (.prediction inside a trycatch; in case of errors, stop
# passing the error message)
.prediction <- function(param_list = NULL,
                        sdm = "sdm",
                        parallel = TRUE,
                        use_python = TRUE,
                        tmpdir = NA,
                        rmtmp = TRUE,
                        sigma = 0.999,
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
        "path_pred"
      )]))) {
      # use path_out if it is not NA, otherwise path_indices, otherwise path_tiles, otherwise path_merged
      main_dir <-
        unlist(pm[c(
          "path_data",
          "path_out",
          "path_rgb",
          "path_indices",
          "path_tiles",
          "path_pred"
        )])[!is.na(pm[c(
          "path_data",
          "path_out",
          "path_rgb",
          "path_indices",
          "path_tiles",
          "path_pred"
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
  paths["pred"] <- if (!is.na(pm$path_pred)) {
    pm$path_pred
  } else {
    file.path(tmpdir, "pred")
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

  # check that output parent directories exist, and create required paths
  parent_paths <- sapply(
    pm[c(
      "path_project",
      "path_data",
      "path_tiles",
      "path_pred",
      "path_out",
      "path_rgb",
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
        "path_pred",
        "path_out",
        "path_rgb",
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
    if (!is.na(pm$path_pred)) {
      pred_ext <- main_ext
      pred_outformat <- pm$outformat
    } else {
      pred_ext <- "vrt"
     pred_outformat <- "VRT"
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

  ##### 3. Compute observation with SDM Select to find best model #####
  if (sdm == "SDMSelect"){
    for (dummy in TRUE) {
      # dummy cycle, created only to allow "break" from this part
      # indices from project
      path_indices_project <- gsub("/indices", paste0("/projets/", pm$project_name, "/indices"), paths["indices"])
      
      # Vectors of paths to rasters of covariates
      covariates_paths <- 
        list.files(file.path(path_indices_project),
                   pattern = '\\.tif$',
                   full.names = TRUE)
      covariates <- SDMSelect::Prepare_covarStack(cov.paths = covariates_paths)
      
      # Change names of stack layer with the name of indice
      nm <- gsub("_CALC_L93_CROP", "",
                 gsub(
                   "\\.tif", "",
                   gsub("\\SENTINEL2A_([0-9]{8})_L2A_T([0-9]{2})[A-Z]_", "", basename(covariates_paths))
                 ))
      names(covariates) <- nm
  
      # calculte covariates
      print_message(type = "message", date = TRUE, i18n$t("Covariates are calculated"))
      
      # read vector observation of presence/absence
      # convert from GeoJSON to sf in EPSG=2154
      obs <- sf::read_sf(pm$extent_pa, quiet = TRUE) %>% st_transform(2154)
      data_obs <- as(obs, "Spatial")
      
      # Extract covariates, combine with dataset and set factor covariate
      data <- SDMSelect::CovarExtract(x = data_obs, cov.paths = covariates_paths)
      names(data) <- c("obs", nm)
      
      # Prepare the spatial dataset for modelling
      data_prepared <- SDMSelect::Prepare_dataset(
        x = data, 
        var = 1, 
        cov = 2:ncol(data),
        datatype = "PA", 
        na.rm = TRUE
      )
      
      # prepare data
      print_message(type = "message", date = TRUE, i18n$t("Data are prepared"))
      
      thd <- SDMSelect::spatialcor_dist(
        x = data_prepared, 
        longlat = !sp::is.projected(data),
        binomial = TRUE, 
        saveWD = pm$path_pred,
        plot = TRUE,
        figname = "Correlogram"
      )
      
      # correlogram
      print_message(type = "message", date = TRUE, i18n$t("Correlogram are calculated"))
      
      # Create a regular grid from dataset
      RefRaster <- SDMSelect::RefRasterize(x = data, res = round(thd[2]))
      # Use rectangular grid to resample dataset
      data_new <- SDMSelect::Prepare_dataset(
        x = data_prepared, 
        var = 1, 
        cov = 2:ncol(data),
        RefRaster = RefRaster, 
        datatype = "PA", 
        na.rm = TRUE
      )
      print_message(type = "message", date = TRUE, i18n$t("New data on regular grid are calculated"))
      
      # Covariates correlation
      corSpearman <- SDMSelect::Param_corr(
        x = data_new, 
        rm = 1, 
        thd = 0.7, 
        visual = FALSE,
        plot = TRUE, 
        img.size = 10, 
        saveWD = pm$path_pred
      )
      print_message(type = "message", date = TRUE, i18n$t("Spearman are calculated"))
      
      # Find the best combination of covariates
      SDMSelect::modelselect_opt(RESET = TRUE)
      SDMSelect::modelselect_opt$Max_nb_Var <- 7
      SDMSelect::modelselect_opt$datatype <- "PA"
  
      res_file <- SDMSelect::findBestModel(x = data_new, 
                                datatype = "PA", 
                                corSpearman = corSpearman, 
                                saveWD = pm$path_pred, 
                                verbose = 0,
                                na.max = 0.8)
      print_message(type = "message", date = TRUE, i18n$t("Find Best Model are calculated"))
      
      # Order models according to quality of prediction
      # Order models and find the bests
      BestModels <- SDMSelect::ModelOrder(saveWD = pm$path_pred, 
                                          plot = TRUE)
      
      # Predictions of the best model
      Num_Best <- BestModels$VeryBestModels_crossV$Num[1]
      res_file <- SDMSelect::ModelResults(saveWD = pm$path_pred, 
                               plot = TRUE, 
                               Num = Num_Best)
      print_message(type = "message", date = TRUE, i18n$t("Find Best Model are saved"))
  
     
      return(invisible(NULL))
    } # end of dummy cycle
  } else if (sdm == "sdm"){
    for (dummy in TRUE) {
      # dummy cycle, created only to allow "break" from this part
      # indices from project
      path_project <- gsub("/indices", paste0("/projets/", pm$project_name), paths["indices"])
      
      # read vector observation of presence/absence
      # convert from GeoJSON to sf in EPSG=2154
      obs <- sf::read_sf(pm$extent_pa, quiet = TRUE) %>% st_transform(2154)
      data_obs <- as(obs, "Spatial")
      
      # read raster and tif for prediction
      path_indices <- list.files(file.path(paste0(path_project, "/indices")),
                                 pattern = '\\.tif$',
                                 full.names = TRUE)
      path_tiles <- list.files(file.path(paste0(path_project, "/merged")),
                               pattern = '\\SENTINEL2A_20180922_L2A_T31T_L93_MERGED_CROP_FRE_',
                               full.names = TRUE)
      paths <- c(path_indices, path_tiles)
      preds <- stack(paths)
      
      # Change names of stack layer with the name of indice
      nmi <- gsub(
        "\\_CALC_L93_CROP.tif", "",
        gsub("\\SENTINEL2A_([0-9]{8})_L2A_T([0-9]{2})[A-Z]_", "", basename(path_indices))
      )
      nmt <- gsub(
        "\\.tif$", "",
        gsub(
          "\\L93_MERGED_CROP_", "",
          gsub("\\SENTINEL2A_([0-9]{8})_L2A_T([0-9]{2})[A-Z]_", "", basename(path_tiles)))
      )
      nm <- c(nmi, nmt)
      names(preds) <- nm
      
      # prepare data
      d <- sdm::sdmData(formula = obs~.,
                        train = data_obs, 
                        predictors = preds
      )
      
      # in the following example, we use 12 differents methods to fit the models.
      # evaluates using 20 runs of doth 5-folds cross-validation and bootstrapping
      # replication method
      print_message(type = "message", date = TRUE, i18n$t("Find the Best Model with sdm."))
      model1 <- suppressWarnings(
        sdm::sdm(formula = obs~.,
                 data = d, 
                 methods = c('glm','gam','brt','svm','cart','mars','mda','rf','mlp','rbf','bioclim','maxlike'),
                 replication = c('boot'), 
                 n = 20
        )
      )
      print_message(type = "message", date = TRUE, i18n$t("Find Best Model are calculated"))
      
      # dates of prediction
      rep <- paste0(pm$path_pred, "/", stringr::str_sub(gsub("\\SENTINEL2A_", "", basename(path_indices[1])), 1, 8))
      dir.create(rep, showWarnings = FALSE, recursive = TRUE)
      
      if (raster::ncell(preds[[1]]) <= 1000000) {
        print_message(type = "message", date = TRUE, i18n$t("Prediction with sdm are calculated, it take times..."))
        predict1 <- predict(
          model1,
          newdata = preds,
          filename = paste0(rep,'/sdm_predict.img')
        )
      } else {
        print_message(type = "message", date = TRUE, i18n$t("Prediction with rf, it take times..."))
        # assign raster values to training data
        v_tot <- as.data.frame(raster::extract(preds, data_obs))
        data_obs@data <- data.frame(data_obs@data, v_tot[match(rownames(data_obs@data), rownames(v_tot)),])
        
        # run rf model
        rf_model <- randomForest::randomForest(x = data_obs@data[,2:ncol(data_obs@data)],
                               y = as.factor(data_obs@data[,"obs"]),
                               ntree = 501,
                               importance=TRUE
        )
        
        # predict model
        rf_predict <- predict(preds, 
                       rf_model, 
                       filename=paste0(rep,'/rf_predict.img'),
                       type="prob", 
                       index=1, 
                       na.rm=TRUE, 
                       # progress="window", 
                       overwrite=TRUE
        )
        print_message(type = "message", date = TRUE, i18n$t("Prediction with rf are calculated."))
        
        # quantile of sigma
        quant <- quantile(rf_predict, probs = sigma, type=7, names = FALSE)
        
        # raster to points
        rf_predict_point <- raster::rasterToPoints(rf_predict, fun=function(x){x >= quant}, spatial = TRUE)
        print_message(type = "message", date = TRUE, i18n$t("Transform rf_predict to SpatialPointsDataFrame."))
        
        rgdal::writeOGR(
          obj=rf_predict_point,
          dsn=rep,
          layer="rf_predict_point",
          overwrite_layer = TRUE,
          driver="ESRI Shapefile"
        )
        print_message(type = "message", date = TRUE, i18n$t("File rf_predict_point are saved."))
        
      }
    }
      
  }
  

  #### Exit ####
  print_message(
    type = "message",
    date = TRUE,
    i18n$t("Execution of CNES session terminated.")
  )

}
