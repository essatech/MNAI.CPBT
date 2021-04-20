#' @title ErosionTotals
#'
#' @description Summarizes erosion totals and damage summaries across the
#' coastline section.
#'
#' @param dat A sf and dataframe of cross-profiles returned from WaveModel.
#' @param erosion Erosion summary table returned from ErosionTransectsUtil.
#' @param Longshore Longshore distance in meters should match
#' ShorelinePointDist used in samplePoints.
#'
#' @return A list object of erosion summaries: erosion_points and totals.
#'
#' @export
ErosionTotals <- function(
  dat = NA,
  erosion = NA,
  Longshore = NA
) {

  # Point on beach at shoreline
  # t1 <- dat[which(dat$Xpos == -1), ]
  t1 <- dplyr::slice(dplyr::group_by(dat, dat$line_id), n=1)

  t2 <- merge(t1, erosion, by.x="line_id", by.y="transect_id")


  # Total Erosion without veg
  t2$origins <- NULL
  t2$erosion_diff <- round(unlist(t2$retreat_NoVeg) - unlist(t2$retreat_Veg), 2)

  #-----------------------------------------
  # Calculate neighbor distance
  t2$shoreline_length <- Longshore


  # Calculate totals
  mlengths            <- t2$shoreline_length
  erosion_diff        <- t2$erosion_diff
  retreat_NoVeg       <- unlist(t2$retreat_NoVeg)
  retreat_Veg         <- unlist(t2$retreat_Veg)

  total_erosion_Veg_m2 <- round(sum(mlengths * retreat_Veg, na.rm=TRUE), 1)
  total_erosion_NoVeg_m2 <- round(sum(mlengths * retreat_NoVeg, na.rm=TRUE), 1)
  total_erosion_diff_m2 <- round(sum(mlengths * erosion_diff, na.rm=TRUE), 1)

  percent_diff <- round((1 - (total_erosion_Veg_m2 / total_erosion_NoVeg_m2)) * 100, 2)

  total_damage_veg <- sum(t2$damage_Veg)
  total_damage_noveg <- sum(t2$damage_NoVeg)
  percent_diff_damage <- round((1 - (total_damage_veg/total_damage_noveg)) * 100, 2)


  # Look at volume
  volume_NoVeg <- unlist(t2$vol_loss_NoVeg)
  volume_Veg <- unlist(t2$vol_loss_Veg)
  volume_NoVeg <- round(sum(volume_NoVeg, na.rm=TRUE))
  volume_Veg <- round(sum(volume_Veg, na.rm=TRUE))


  # Build return object
  ret_out <- list()
  ret_out[['erosion_points']] <- t2
  ret_out[['total_erosion_Veg_m2']] <- total_erosion_Veg_m2
  ret_out[['total_erosion_NoVeg_m2']] <- total_erosion_NoVeg_m2
  ret_out[['total_erosion_diff_m2']] <- total_erosion_diff_m2
  ret_out[['percent_diff']] <- percent_diff

  ret_out[['total_damage_Veg']] <- total_damage_veg
  ret_out[['total_damage_NoVeg']] <- total_damage_noveg
  ret_out[['percent_diff_damage']] <- percent_diff_damage

  ret_out[['volume_Veg']] <- volume_Veg
  ret_out[['volume_NoVeg']] <- volume_NoVeg


  return(ret_out)



}
