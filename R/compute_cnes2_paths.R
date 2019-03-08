#' compute_cnes2_paths
#'
#' @param pm pm
#' @param cnes_list cnes_list
#' @param force_tiles force_tiles
#' @param check_tmp check_tmp
#'
#' @return names
#' @export
#'
#' @examples
#' 
compute_cnes2_paths <- function(pm,
                                cnes_list,
                                force_tiles = FALSE,
                                check_tmp = TRUE) {

  ## Define output file names and lists ##

  # expected names for tiles
  tiles_names_exp <- file.path(
    pm$path_tiles,
    c(outer(paste0(cnes_list, "_"), paste0("TILES_", def_names$sentinel2, ".tif"), paste0))
  )
  # existing tiles in project
  tiles_names_exi <- list.files(
    pm$path_tiles,
    full.names = TRUE
  )
  # required tiles to processing
  tiles_names_req <- unique(c(tiles_names_exp, tiles_names_exi))

  # expected names for mosaic
  mosaic_names_exp <- file.path(
    pm$path_mosaic,
    c(outer(paste0(cnes_list, "_"), paste0("MOSAIC_", def_names$sentinel2, ".vrt"), paste0))
  )
  # existing mosaic in project
  mosaic_names_exi <- list.files(
    pm$path_mosaic,
    full.names = TRUE
  )
  # required mosaic to processing
  mosaic_names_req <- unique(c(mosaic_names_exp, mosaic_names_exi))

  # expected names for merged
  merged_names_exp <- file.path(
    pm$path_merged,
    c(outer(paste0(cnes_list, "_"), paste0("MERGED_", def_names$sentinel2, ".tif"), paste0))
  )
  # existing merged in project
  merged_names_exi <- list.files(
    pm$path_merged,
    full.names = TRUE
  )
  # required merged to processing
  merged_names_req <- unique(c(merged_names_exp, merged_names_exi))

  # expected names for warped
  warped_names_exp <- file.path(
    pm$path_warped,
    c(outer(paste0(cnes_list, "_"), paste0("L93_WARPED_", def_names$sentinel2, ".tif"), paste0))
  )
  # existing warped in project
  warped_names_exi <- list.files(
    pm$path_warped,
    full.names = TRUE
  )
  # required warped to processing
  warped_names_req <- unique(c(warped_names_exp, warped_names_exi))

  # expected names for translate
  translate_names_exp <- file.path(
    pm$path_translate,
    c(outer(paste0(cnes_list, "_"), paste0("TRANSLATE_", def_names$sentinel2, ".tif"), paste0))
  )
  # existing translate in project
  translate_names_exi <- list.files(
    pm$path_translate,
    full.names = TRUE
  )
  # required translate to processing
  translate_names_req <- unique(c(translate_names_exp, translate_names_exi))

  # expected names for rgb
  rgb_names_exp <- file.path(
    pm$path_rgb,
    c(outer(paste0(cnes_list, "_"), paste0("RGB_L93_CROP_", c("natural", "nearinfra", "farinfra", "vegetation"), ".tif"), paste0))
  )
  # existing rgb in project
  rgb_names_exi <- list.files(
    pm$path_rgb,
    pattern = "*_RGB_L93_CROP_.*\\.tif",
    full.names = TRUE
  )
  # required rgb to processing
  rgb_names_req <- unique(c(rgb_names_exp, rgb_names_exi))

  # expected names for masked
  masked_names_exp <- file.path(
    pm$path_masked,
    c(outer(paste0(cnes_list, "_"), paste0("MASKED_", def_names$sentinel2, ".tif"), paste0))
  )
  # existing masked in project
  masked_names_exi <- list.files(
    pm$path_masked,
    full.names = TRUE
  )
  # required masked to processing
  masked_names_req <- unique(c(masked_names_exp, masked_names_exi))

  # expected names for out
  out_names_exp <- file.path(
    pm$path_out,
    paste0(pm$project, "_OUT.tif")
  )
  # existing out in project
  out_names_exi <- list.files(
    pm$path_out,
    full.names = TRUE
  )
  # required out to processing
  out_names_req <- unique(c(out_names_exp, out_names_exi))

  # expected names for indices
  indices_list <- list()
  for (i in pm$list_indices_checked) {
    indices_names_exp <- file.path(
      pm$path_indices,
      c(outer(paste0(cnes_list, "_"), paste0(i, ".tif"), paste0))
    )
    indices_list[[i]]["exp"] <- list(indices_names_exp)
  }

  # list of required files and steps

  # if overwrite is set to TRUE, works with expected names;
  # otherwise, compute non-existing values

  if (pm$overwrite == TRUE) {
    tiles_names_new <- tiles_names_exp
    mosaic_names_new <- mosaic_names_exp
    merged_names_new <- merged_names_exp
    warped_names_new <- warped_names_exp
    translate_names_new <- translate_names_exp
    rgb_names_new <- rgb_names_exp
    masked_names_new <- masked_names_exp
    out_names_new <- out_names_exp
    indices_list_new <- indices_list
  } else {
    tiles_names_new <- tiles_names_req
    mosaic_names_new <- mosaic_names_req
    merged_names_new <- merged_names_req
    warped_names_new <- warped_names_req
    translate_names_new <- translate_names_req
    rgb_names_new <- rgb_names_req
    masked_names_new <- masked_names_req
    out_names_new <- out_names_req
    indices_list_new <- indices_list
  } # end of pm$overwrite FALSE

  # End of the section of the creation of file names
  # List of the file names:
  # List of all the file names, in order of creation
  list(
    "tiles_names_exp" = tiles_names_exp,
    "tiles_names_req" = tiles_names_req,
    "tiles_names_new" = tiles_names_new,
    "mosaic_names_exp" = mosaic_names_exp,
    "mosaic_names_req" = mosaic_names_req,
    "mosaic_names_new" = mosaic_names_new,
    "merged_names_exp" = merged_names_exp,
    "merged_names_req" = merged_names_req,
    "merged_names_new" = merged_names_new,
    "warped_names_exp" = warped_names_exp,
    "warped_names_req" = warped_names_req,
    "warped_names_new" = warped_names_new,
    "masked_names_exp" = masked_names_exp,
    "masked_names_new" = masked_names_new,
    "masked_names_req" = masked_names_req,
    "out_names_exp" = out_names_exp,
    "out_names_req" = out_names_req,
    "out_names_new" = out_names_new,
    "translate_names_exp" = translate_names_exp,
    "translate_names_req" = translate_names_req,
    "translate_names_new" = translate_names_new,
    "rgb_names_exp" = rgb_names_exp,
    "rgb_names_new" = rgb_names_new,
    "rgb_names_req" = rgb_names_req,
    "indices_names_list" = indices_list
  )
}
