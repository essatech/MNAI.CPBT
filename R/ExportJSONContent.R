#' Exports JSON content
#' @keywords internal
ExportJSONContent <- function(
  path_output = NA,
  flood_contour = NA,
  merg_Veg = NA,
  wave_dat = NA,
  dat = NA,
  ero_tot = NA,
  total_wsl_adj = NA
) {



  # Helper function to export JSON as JS object for local src
  BuildjsJSONobj <- function(out_fname = NA, layer_name = NA, exp_obj = NA) {
    if(file.exists(out_fname)){
      file.remove(out_fname)
    }
    rgdal::writeOGR(exp_obj,
                    dsn=out_fname,
                    layer=layer_name,
                    driver="GeoJSON",
                    delete_dsn = TRUE,
                    overwrite_layer = TRUE)

    # Merge to js object
    fConn <- file(out_fname, 'r+')
    Lines <- readLines(fConn)

    new_line <- paste0("var ", layer_name, " = ")
    writeLines(c(new_line, Lines), con = fConn)
    close(fConn)

    # add bracket at end file
  }






  #============================================
  # Flood Contours to JSON
  #============================================

  # Convert to EPSG: 4326
  flood_contour <- flood_contour[which(flood_contour$name != "LowTide"),]
  in_obj_t <- sf::st_transform(flood_contour, 4326)

  # Convert to sp object
  in_obj_t_sp <- methods::as(in_obj_t, "Spatial")

  out_fname <- paste0(path_output, 'www/data/flood.json')

  BuildjsJSONobj(out_fname = out_fname,
                 layer_name = 'flood',
                 exp_obj = in_obj_t_sp)


  #============================================
  # Export vegetation layer to JSON
  #============================================

  in_obj <- merg_Veg[, c('Type','StemHeight','StemDiam','StemDensty','Cd')]

  # Convert to EPSG: 4326
  in_obj_t <- sf::st_transform(in_obj, 4326)

  # Convert to sp object
  in_obj_t_sp <- methods::as(in_obj_t, "Spatial")

  out_fname <- paste0(path_output,
                      'www/data/veg.json'
  )

  BuildjsJSONobj(out_fname = out_fname, layer_name='veg', exp_obj = in_obj_t_sp)







  #=========================================
  # Export Wave Transects
  #=========================================
  # Loop through transects to build export json
  # object - check size to not exceed 2MB

  ids <- unique(wave_dat$line_id)

  # HighCharts object for
  # add all highcharts transects to this list
  alldata <- list()
  transectfeatures <- list()
  linegeometry <- list()


  for(i in 1:length(ids)){

    this_id <- ids[i]

    # Wave transect
    this_transect = wave_dat[which(wave_dat$line_id == this_id),]

    # Export transect erosion data
    tran_e <- ero_tot[[1]]
    tran_e <- tran_e[which(tran_e$line_id == this_id),]

    #------------------------------------------
    # Original transect remainder
    # Add on remaining leg of transect to the high water mark
    # high water line must now include runup.

    orig_tran <- dat[which(dat$line_id == this_id),]
    total_wsl_adj_t <- total_wsl_adj + tran_e$runup_NoVeg

    # get remainder...
    # where did wave stop processing

    wav_min_xpos <- min(this_transect$Xpos, na.rm=TRUE)

    orig_tran <- orig_tran[which(orig_tran$Xpos < wav_min_xpos & orig_tran$elev <= total_wsl_adj_t),]

    # Determine if there is an island break
    CutSeq <- function(data,threshold) {
      cut <- which(c(1,diff(data)) > threshold)
      return(cut)
    }

    cs <- CutSeq(orig_tran$Xpos, 5)

    if(length(cs) > 0) {
      cs <- cs[length(cs)]
      # get tail
      orig_tran <- orig_tran[cs:nrow(orig_tran),]
    }


    # mapview(orig_tran) + mapview(this_transect['Etas'])
    # Add missing columns to match other object

    if( nrow(orig_tran) > 0) {

      orig_tran$height_array <- NA
      #orig_tran$Xpos_rev <- NA
      orig_tran$Eta <- 0
      orig_tran$Etas <- 0
      orig_tran$Ubot <- 0
      # Set waves to zero
      orig_tran$H_veg <- 0
      orig_tran$H_noveg <- 0
      orig_tran$Dis1 <- 0
      orig_tran$DisSimple1 <- 0

      orig_tran <- orig_tran[seq(dim(orig_tran)[1],1),]


      # Fix geometry
      obj <- this_transect
      objG <- sf::st_geometry(obj)
      sf::st_geometry(obj) <- NULL
      sf::st_geometry(obj) <- objG
      this_transect <- obj

      obj <- orig_tran
      objG <- sf::st_geometry(obj)
      sf::st_geometry(obj) <- NULL
      sf::st_geometry(obj) <- objG
      orig_tran <- obj

      # Merge objects
      this_transect <- rbind(this_transect, orig_tran)
      #colnames(this_transect)
      #colnames(orig_tran)


      #plot(this_transect$elev, type='l')
    }


    # Transform to latlong
    this_transect <- sf::st_transform(this_transect, 4326)

    #plot(this_transect$elev_smooth)
    #plot(this_transect$H_noveg)

    xData = seq(1, nrow(this_transect), by=1)

    # Parent
    build_obj = list()
    build_obj[["xData"]] = xData

    # Make individual series for high charts...
    child = list()
    child[["name"]] = 'Wave Height: Without Vegetation'
    child[["data"]] = round(this_transect$H_noveg,3)
    child[["unit"]] = 'm'
    child[["type"]] = 'line'
    child[["valueDecimals"]] = 3

    child2 = list()
    child2[["name"]] = 'Wave Height: With Vegetation'
    child2[["data"]] = round(this_transect$H_veg,3)
    child2[["unit"]] = 'm'
    child2[["type"]] = 'line'
    child2[["valueDecimals"]] = 3

    child3 = list()
    child3[["name"]] = 'Elevation (Chart Datum)'
    child3[["data"]] = this_transect$elev
    child3[["unit"]] = 'm'
    child3[["type"]] = 'area'
    child3[["valueDecimals"]] = 0

    this_transect$StemHeight <- round(this_transect$StemHeight, 1)
    child4 = list()
    child4[["name"]] = 'Vegetation Height'
    child4[["data"]] = ifelse(is.na(this_transect$StemHeight), 0, this_transect$StemHeight)
    child4[["unit"]] = 'm'
    child4[["type"]] = 'area'
    child4[["valueDecimals"]] = 1

    clist = list(child, child2, child3, child4)

    build_obj[["datasets"]] = clist

    # Add highcharts data to master dataset of all dat
    alldata[[as.character(this_id)]] <- build_obj


    #------------------------------------------
    # Export extra features for transect
    #------------------------------------------

    # Get start point and bearing - for map (seperate json file)
    start_coord <- utils::head(sf::st_coordinates(this_transect),1)
    end_coord <- utils::tail(sf::st_coordinates(this_transect),1)
    bearing <- geosphere::bearingRhumb( c(start_coord[1], start_coord[2]),c(end_coord[1], end_coord[2]))

    # Set map center
    mid_row <- nrow(this_transect)/2
    cent_coord <- sf::st_coordinates(this_transect[mid_row,])

    # All variable properties for current transect
    this_tran_prop <- list()
    this_tran_prop[['start_lat']] <- start_coord[2]
    this_tran_prop[['start_long']] <- start_coord[1]
    this_tran_prop[['end_lat']] <- end_coord[2]
    this_tran_prop[['end_long']] <- end_coord[1]
    this_tran_prop[['bearing']] <- bearing
    this_tran_prop[['mid_lat']] <- cent_coord[2]
    this_tran_prop[['mid_long']] <- cent_coord[1]
    this_tran_prop[['total_wsl_adj']] <- total_wsl_adj
    this_tran_prop[['retreat_NoVeg']] <- round(tran_e$retreat_NoVeg, 2)
    this_tran_prop[['retreat_Veg']] <- round(tran_e$retreat_Veg,2)
    this_tran_prop[['runup_NoVeg']] <- round(tran_e$runup_NoVeg,3)
    this_tran_prop[['runup_Veg']] <- round(tran_e$runup_Veg,3)
    this_tran_prop[['erosion_diff']] <- round(tran_e$erosion_diff,2)
    this_tran_prop[['shoreline_length']] <- round(tran_e$shoreline_length,0)

    this_tran_prop[['damage_NoVeg']] <- round(tran_e$damage_NoVeg,0)
    this_tran_prop[['damage_Veg']] <- round(tran_e$damage_Veg,0)
    this_tran_prop[['sed_size']] <- round(tran_e$sed_size,3)
    this_tran_prop[['berm_lengt']] <- round(tran_e$berm_lengt,2)
    this_tran_prop[['berm_heigh']] <- round(tran_e$berm_heigh,2)
    this_tran_prop[['dune_heigh']] <- round(tran_e$dune_heigh,2)
    this_tran_prop[['fore_slp']] <- round(tran_e$fore_slp,3)

    # Beach Retreat Percentage
    this_tran_prop[['retreat_percentage_veg']] <- round((tran_e$retreat_pct_Veg),0)
    this_tran_prop[['retreat_percentage_Noveg']] <- round((tran_e$retreat_pct_NoVeg),0)

    this_tran_prop[['retreat_index_Veg']] <- as.numeric(as.character((tran_e$retreat_index_Veg)))
    this_tran_prop[['retreat_index_NoVeg']] <- as.numeric(as.character((tran_e$retreat_index_NoVeg)))

    this_tran_prop[['area_loss_Veg']] <- round((tran_e$area_loss_Veg),0)
    this_tran_prop[['area_loss_NoVeg']] <- round((tran_e$area_loss_NoVeg),0)

    this_tran_prop[['vol_loss_Veg']] <- round((tran_e$vol_loss_Veg),0)
    this_tran_prop[['vol_loss_NoVeg']] <- round((tran_e$vol_loss_NoVeg),0)

    this_tran_prop[['damage_t_Veg_ss']] <- round((tran_e$damage_t_Veg_ss),0)
    this_tran_prop[['damage_t_NoVeg_ss']] <- round((tran_e$damage_t_NoVeg_ss),0)

    this_tran_prop[['damage_t_Veg']] <- round((tran_e$damage_Veg),0)
    this_tran_prop[['damage_t_NoVeg']] <- round((tran_e$damage_NoVeg),0)







    this_tran_prop[['id']] <- this_id




    # add to parent object
    transectfeatures[[as.character(this_id)]] <- this_tran_prop



    #---------------------------------------
    # Build final Geojson linestring feature
    ls <- sf::st_linestring(rbind(start_coord,end_coord))
    ls <- sf::st_sfc(ls)
    ls <- sf::st_as_sf(ls)
    ls$idd <- this_id
    linegeometry[[i]] <- ls
    #print(i)

  }


  # Merge spatial lines
  linegeom <- do.call("rbind", linegeometry)
  sf::st_crs(linegeom) <- 4326
  linegeom <- methods::as(linegeom, "Spatial")

  out_fname <- paste0(path_output,
                      'www/data/tran_lines.json'
  )

  BuildjsJSONobj(out_fname = out_fname, layer_name="tran_lines", exp_obj = linegeom)




  #================================================================
  # Export total erosion data
  #================================================================

  # Export total erosion object
  global_erosion <- ero_tot[2:length(ero_tot)]
  global_erosion <- jsonlite::toJSON(global_erosion)
  # var tot_erosion = { }
  out_fname <- paste0(path_output, 'www/data/tot_erosion.json')

  write(global_erosion, out_fname)

  # Merge to js object
  fConn <- file(out_fname, 'r+')
  Lines <- readLines(fConn)
  new_line <- paste0("var tot_erosion = ")
  writeLines(c(new_line, Lines), con = fConn)
  close(fConn)





  #================================================================
  # Export main transect data
  #================================================================

  outjson <- jsonlite::toJSON(alldata)
  out_fname <- paste0(path_output, 'www/data/tran_data.json')

  write(outjson, out_fname)
  # Merge to js object
  fConn <- file(out_fname, 'r+')
  Lines <- readLines(fConn)
  new_line <- paste0("var tran_data = ")
  writeLines(c(new_line, Lines), con = fConn)
  close(fConn)









  #================================================================
  # Export main transect features
  #================================================================

  outjson_features <- jsonlite::toJSON(transectfeatures)
  out_fname <- paste0(path_output, 'www/data/tran_feat.json')

  write(outjson_features, out_fname)
  # Merge to js object
  fConn <- file(out_fname, 'r+')
  Lines <- readLines(fConn)
  new_line <- paste0("var tran_feat = ")
  writeLines(c(new_line, Lines), con = fConn)
  close(fConn)





  #================================================================
  # Export map iframe of erosion points
  #================================================================

  erosion <- ero_tot$erosion_points
  erosion$retreat_Veg <- round(erosion$retreat_Veg, 1)
  erosion$retreat_NoVeg <- round(erosion$retreat_NoVeg, 1)
  erosion <- erosion[, c('retreat_Veg', 'retreat_NoVeg')]




}




