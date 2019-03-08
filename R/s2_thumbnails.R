#' @title Create thumbnails for S2 products.
#' @description Function to create thumbnail images for Sentinel-2
#'  products. BOA and TOA multiband images are rendered as false colour
#'  JPEG images; SCL maps are rendered as 8-bit PNG;
#'  other singleband images (like spectral indices) are rendered as 
#'  JPEG images with a standard colour palette.
#'  Output images are georeferenced.
#' @param infiles A vector of input filenames. Input files are paths
#'  of products already converted from SAFE format to a
#'  format managed by GDAL (use [s2_translate] to do it);
#'  their names must be in the theia2r naming convention
#'  ([safe_shortname]).
#' @param prod_type (optional) Output product (see [safe_shortname] for the 
#'  list of accepted products). If not provided, it is retrieved from the
#'  file name.
#' @param rgb_type (optional) For BOA and TOA products, this value determine
#'  the type of false colours to be used for the thumbnails:
#'  * `"SwirNirR"` (default) for SWIR-NIR-Red;
#'  * `"NirRG"` for NIR-Red-Green;
#'  * `"RGB"` for true colours;
#'  
#' @param dim Integer value, with the maximum greater dimension in pixels (width or 
#'  height) of the output images (default: 1024 px). 
#'  If this is lower than the corresponding dimension of the maps, maps are
#'  rescaled before producing the thumbnails; otherwise the original dimensions
#'  are maintained. 
#'  To keep the original size in any case, set `dim = Inf`.
#' @param scaleRange (optional) Range of valid values. If not specified 
#'  (default), it is automatically retrieved from the product type. 
#'  Default ranges for BOA and TOA products are 0 to 8000 
#'  (`rgb_type = "SwirNirR"`), 0 to 7500 (`"NirRG"`) and 0 to 2500 (`"RGB"`).
#'  For spectral indices, default range is -1 to 1 for Float products, -10000
#'  to 10000 for Int and 0 to 200 for Byte; for "Zscore" products, default 
#'  range is -3 to 3 for Float and -3000 to 3000 for Int.
#'  It can be useful i.e. to stretch BOA "dark" products. 
#' @param outdir (optional) Full name of the existing output directory
#'  where the files should be created.  Default is a subdirectory (named 
#'  "thumbnails") of the parent directory of each input file.
#' @param tmpdir (optional) Path where intermediate files (VRT) will be created.
#'  Default is a temporary directory.
#' @param rmtmp (optional) Logical: should temporary files be removed?
#'  (Default: TRUE)
#' @param overwrite (optional) Logical value: should existing thumbnails be
#'  overwritten? (default: TRUE)
#' @return A vector with the names of the created images.
#'
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}, Pascal Obstetar, (2019) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom jsonlite fromJSON
#' @export

s2_thumbnails <- function(infiles, 
                          prod_type=NA, # TODO implement (for now only NOA / TOA)
                          rgb_type="SwirNirR",
                          dim=1024, 
                          scaleRange=NA,
                          outdir=NA,
                          tmpdir=NA,
                          rmtmp=TRUE,
                          overwrite=FALSE) {
  
  # Check that GDAL suports JPEG JFIF format
  # TODO
  
  # Set tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="s2thumbnails_")
  }
  dir.create(tmpdir, recursive = FALSE, showWarnings = FALSE)
  
  # Load GDAL paths
  binpaths <- load_binpaths("gdal")
  
  # Get files metadata
  if (is.na(prod_type)) {
    infiles_meta <- data.table(theia2r_getElements(infiles, format="data.frame"))
  }
  
  out_names <- character(0) # names of created files
  for (i in seq_along(infiles)) {
    sel_infile_path <- infiles[i]
    
    # set outdir
    if (is.na(outdir)) {
      sel_outdir <- file.path(dirname(sel_infile_path), "thumbnails")
      dir.create(sel_outdir, recursive = FALSE, showWarnings = FALSE)
    }
    
    # Determine prod_type
    sel_prod_type <- if (is.na(prod_type)) {
      infiles_meta[i,prod_type]
    } else {
      prod_type
    }
    
    # Set output path
    out_path <- file.path(
      sel_outdir, 
      gsub(
        "\\.[^\\.]+$",
        if (sel_prod_type %in% c("SCL")) {".png"} else {".jpg"}, # resp. discrete or continuous values
        basename(sel_infile_path)
      )
    )
    
    # if output already exists and overwrite==FALSE, do not proceed
    if (!file.exists(out_path) | overwrite==TRUE) {
      
      # Consider only the required bands
      if (sel_prod_type %in% c("BOA","TOA")) {
        rgb_bands = switch(
          rgb_type,
          "SwirNirR" = c(11,8,4),
          "NirRG" = c(8,4,3),
          "RGB" = c(4,3,2)
        )
        filterbands_path <- file.path(tmpdir, gsub("\\..+$","_filterbands.vrt",basename(sel_infile_path)))
        system(
          paste0(
            binpaths$gdal_translate," -of VRT ",
            "-b ",paste(rgb_bands, collapse=" -b ")," ",
            "\"",sel_infile_path,"\" ",
            "\"",filterbands_path,"\""
          ), intern = Sys.info()["sysname"] == "Windows"
        )
      } else {
        filterbands_path <- sel_infile_path
      }
      
      # Resize input if necessary
      sel_infile_size <- suppressWarnings(GDALinfo(sel_infile_path)[c("rows","columns")])
      resized_path <- file.path(tmpdir, gsub(
        "\\..+$",
        if (sel_prod_type %in% c("BOA","TOA")) {"_resized.tif"} else {"_resized.vrt"},
        basename(sel_infile_path)
      )) # GTiff is used for multiband images to avoid problems using gdal_calc (#82)
      if (dim < max(sel_infile_size)) {
        out_size <- round(sel_infile_size * min(dim,max(sel_infile_size)) / max(sel_infile_size))
        system(
          paste0(
            binpaths$gdalwarp,
            if (sel_prod_type %in% c("BOA","TOA")) {" -of GTiff -co COMPRESS=LZW "} else {" -of VRT "},
            "-ts ",out_size[2]," ",out_size[1]," ",
            if (sel_prod_type %in% c("SCL")) {"-r mode "} else {"-r average "}, # resp. discrete or continuous values
            "\"",filterbands_path,"\" ",
            "\"",resized_path,"\""
          ), intern = Sys.info()["sysname"] == "Windows"
        )
      } else {
        if (sel_prod_type %in% c("BOA","TOA")) {
          system(
            paste0(
              binpaths$gdal_translate," ",
              "-of GTiff -co COMPRESS=LZW ",
              "\"",filterbands_path,"\" ",
              "\"",resized_path,"\""
            ), intern = Sys.info()["sysname"] == "Windows"
          )
        } else {
          resized_path <- filterbands_path
        }
      }
      
      # define scaleRange
      if (anyNA(scaleRange)) {
        scaleRange <- if (sel_prod_type %in% c("BOA","TOA")) {
          c(0, switch(rgb_type, "SwirNirR" = 8000, "NirRG" = 7500, "RGB" = 2500))
        } else if (sel_prod_type %in% c("Zscore","rbias")){
          sel_infile_datatype <- attr(
            suppressWarnings(GDALinfo(sel_infile_path)),
            "df"
          )[1,"GDType"]
          if (grepl("^Float",sel_infile_datatype)) {
            if (sel_prod_type == "Zscore") {c(-3, 3)} else {c(-300, 300)}
          } else if (grepl("^Int",sel_infile_datatype)) {
            c(-3E3,3E3)
          } else if (grepl("^Byte$",sel_infile_datatype)) {
            c(0,200)
          }
        } else if (sel_prod_type %in% c("SCL")){
          rep(NA,2) # it is ignored
        } else { # spectral indices
          sel_infile_datatype <- attr(
            suppressWarnings(GDALinfo(sel_infile_path)),
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
      
      # generate RGB basing on prod_type
      if (sel_prod_type %in% c("BOA","TOA")) {
        
        stack2rgb(
          resized_path, 
          out_file = out_path,
          minval = scaleRange[1], 
          maxval = scaleRange[2],
          tmpdir = tmpdir
        )
        
      } else if (grepl("^((TCI)|(RGB[0-9a-f]{3}[BT]))$" ,sel_prod_type)) {
        
        system(
          paste0(
            binpaths$gdal_translate," ",
            "-of JPEG -co QUALITY=90 ",
            "-a_nodata 0 ",
            "\"",resized_path,"\" ",
            "\"",out_path,"\""
          ), intern = Sys.info()["sysname"] == "Windows"
        )
        
      } else {
        
        raster2rgb(
          resized_path, 
          out_file = out_path, 
          palette = if (sel_prod_type %in% c("SCL")) {
            sel_prod_type
          } else if (grepl("\\-Z$",sel_prod_type) | sel_prod_type=="Zscore") {
            "Zscore"
          } else {
            "generic_ndsi"
          },
          tmpdir = tmpdir
        )
        
      }
      
    } # end of overwrite IF cycle
    
    out_names <- c(out_names, out_path)
    
  } # end of infiles cycle
  
  # Remove temporary files
  if (rmtmp == TRUE) {
    unlink(tmpdir, recursive=TRUE)
  }
  
  print_message(
    type="message",
    length(out_names)," output files were correctly created."
  )
  return(out_names)
  
}
