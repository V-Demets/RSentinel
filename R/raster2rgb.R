#' @title Produce an RGB image from a singleband raster file.
#' @description Internal function to create JPEG or PNG images from a 
#'  singleband raster file. This function is used by [s2_thumbnails], 
#'  and it will be exported when it would be more generalised.
#' @param in_rast Path of the input multiband raster.
#' @param out_file (optional) Path of the output RGB JPEG image; if NULL
#'  (default), a RasterLayer will be returned.
#' @param palette Path of the palette file to be used (cpt or txt),
#'  or character value of a builtin palette (the default 
#'  "generic_ndsi").
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#' @return The path of the output image; alternatively, the output image
#'  as RasterLayer (if `out_rast = NULL`).
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}, Pascal Obstetar, (2019) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' @importFrom raster raster
#' @importFrom jsonlite fromJSON
#' @export

raster2rgb <- function(in_rast, 
                       out_file = NULL, 
                       palette = "generic_ndsi",
                       tmpdir = NA,
                       scaleRange=NA) {

  # TODO minval, maxval: now they do not work,
  # add a code to read cpt file, rescale values and save as temp file
  
  # Check that GDAL suports JPEG JFIF format
  # TODO
  
  # Define builtin palette paths
  palette_builtin <- c(
    "NDVI" = system.file("extdata","palettes","NDVI.cpt", package="shinycnes"),
    "generic_ndsi" = system.file("extdata","palettes","NDSI.cpt", package="shinycnes")
  )
  
  # define scaleRange
  if (anyNA(scaleRange)) {
    scaleRange <- {
      sel_infile_datatype <- attr(
        suppressWarnings(GDALinfo(in_rast)),
        "df"
      )[1,"GDType"]
      if (grepl("^Float",sel_infile_datatype)) {
        c(-1, 1)
      } else if (grepl("^Int",sel_infile_datatype)) {
        c(-1E4,1E4)
      } else if (grepl("^Byte$",sel_infile_datatype)) {
        c(0,200)
      }
    }
  }
  minval <- scaleRange[1]
  maxval <- scaleRange[2]
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Load palette
  if (!is.character(palette)) {
    print_message(
      type = "error",
      i18n$t("Argument \"palette\" must be an allowed character value "),
      i18n$t("or the path of a cpt file.")
    )
  }
  if (palette %in% names(palette_builtin)) {
    palette <- palette_builtin[palette]
  }
  
  # define and create tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="raster2rgb_")
  }
  dir.create(tmpdir, recursive=FALSE, showWarnings=FALSE)
  
  # Rescale palette, if necessary
  if (!names(palette) %in% c("SCL") & (minval!=-1 | maxval!=1)) {
    palette_txt <- strsplit(gsub("^ *(.*) *$","\\1",readLines(palette))," +")
    palette_txt_new <- palette_txt
    for (i in seq_along(palette_txt)) {
      if (length(palette_txt[[i]])==8 & !anyNA(suppressWarnings(as.numeric(palette_txt[[i]])))) {
        palette_txt_new[[i]][c(1,5)] <- (as.numeric(palette_txt[[i]])[c(1,5)]+1)/2*(maxval-minval)+minval
      }
    }
    writeLines(
      sapply(palette_txt_new, paste, collapse=" "), 
      palette <- file.path(tmpdir, basename(tempfile(fileext = ".cpt")))
    )
  }
  
  # Set output file
  return_raster <- if (is.null(out_file)) {
    out_file <- file.path(tmpdir, basename(tempfile(pattern = "raster2rgb_")))
    TRUE
  } else {
    FALSE
  }
  
  # Compute RGB with gdaldem
  # (an intermediate step creating a GeoTiff is required,
  # since gdal_calc is not able to write in JPEG format)
  tif_path <- file.path(tmpdir, gsub("\\..+$","_temp.tif",basename(out_file)))
  system(
    paste0(
      binpaths$gdaldem," color-relief ",
      "-of GTiff -co COMPRESS=LZW ", # discrete values
      "-compute_edges ",
      "\"",in_rast,"\" ",
      "\"",palette,"\" ",
      "\"",tif_path,"\""
    ), intern = Sys.info()["sysname"] == "Windows"
  )
  system(
    paste0(
      binpaths$gdal_translate," ",
      if (gsub("^.*\\.(.+)$","\\1",out_file) == "png") {
        "-of PNG -co ZLEVEL=9 -co NBITS=8 " # discrete values
      } else if (gsub("^.*\\.(.+)$","\\1",out_file) %in% c("jpg","jpeg")) {
        "-of JPEG -co QUALITY=90 " # continuous values
      },
      "\"",tif_path,"\" \"",out_file,"\""
    ),
    intern = Sys.info()["sysname"] == "Windows"
  )
  unlink(tif_path)
  
  # Return output raster
  if (return_raster) {
    return(raster(out_file))
  } else {
    return(invisible(NULL))
  }
  
}
