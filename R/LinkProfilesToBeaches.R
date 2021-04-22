#' @title Link Profiles To Beaches
#'
#' @description Merges the profiles returned from WaveModel and adds foreshore
#' beach attributes to them.
#'
#' @param BeachAttributes class sf and dataframe of foreshore beach attributes.
#' @param dat Cross-shore profile spatial points object returned from WaveModel.
#'
#' @return A data table of foreshore attributes for each cross shore profile.
#'
#' @export
LinkProfilesToBeaches <- function(
  BeachAttributes = NA,
  dat = NA) {

  # Load in foreshore
  fs <- BeachAttributes

  fs$fore_slp <- fs$slope
  fs$berm_lengt <- fs$W
  fs$berm_heigh <- fs$B
  fs$dune_heigh <- fs$D
  fs$sed_size <- fs$sediment
  fs$fore_slp <- fs$V

  # Loop through profiles
  uids <- unique(dat$line_id)

  # build output
  output <- list()

  for(i in seq_len(length(uids))){

    this_id <- uids[i]
    df <- dat[which(dat$line_id == this_id),]

    # Build simple line - faster intersection
    start_coord <- sf::st_coordinates(utils::head(df,1))
    end_coord <- sf::st_coordinates(utils::tail(df,1))
    ls <- sf::st_linestring(rbind(start_coord,end_coord))
    ls <- sf::st_sfc(ls)
    ls <- sf::st_as_sf(ls)
    ls$id <- this_id
    sf::st_crs(ls) <- sf::st_crs(df)

    # Intersect with foreshore

    ls <- sf::st_transform(ls, sf::st_crs(fs))
    myint <- suppressWarnings(sf::st_intersection(ls, fs))

    if (nrow(myint) == 0) {

      # take median if no int
      sed_size <- stats::median(fs$sed_size)
      berm_lengt <- stats::median(fs$berm_lengt)
      berm_heigh <- stats::median(fs$berm_heigh)
      dune_heigh <- stats::median(fs$dune_heigh)
      fore_slp <- stats::median(fs$fore_slp)

      add_row <- data.frame(line_id = this_id,
                            sed_size = sed_size,
                            berm_lengt = berm_lengt,
                            berm_heigh = berm_heigh,
                            dune_heigh = dune_heigh,
                            fore_slp = fore_slp)
      output[[i]] <- add_row
      next

    } else {

      if(nrow(myint) > 1) {
        myint <- myint[1, ]
      }

      # take median if no int
      sed_size <- stats::median(myint$sed_size)
      berm_lengt <- stats::median(myint$berm_lengt)
      berm_heigh <- stats::median(myint$berm_heigh)
      dune_heigh <- stats::median(myint$dune_heigh)
      fore_slp <- stats::median(myint$fore_slp)

      add_row <- data.frame(line_id = this_id,
                            sed_size = sed_size,
                            berm_lengt = berm_lengt,
                            berm_heigh = berm_heigh,
                            dune_heigh = dune_heigh,
                            fore_slp = fore_slp)
      output[[i]] <- add_row
      next

    } # end of int


  } # end of transect loop

  allo <- do.call("rbind", output)

  return(allo)

} # end of func

