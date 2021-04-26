#' @title Wave Evolution Model
#'
#' @description Wave attenuation model originally developed by Dr. Greg Guannel
#' for the Coastal Natural Capital InVEST project. This function models wave
#' attenuation along a cross-shore elevation profile.
#'
#' @param dat A sf and dataframe of cross-profiles returned from ExtractVeg.
#' @param total_wsl_adj Total water surface level above the chart datum. Recall
#' that the chart datum and TopoBathy DEM are referenced to have 0 at low
#' water. It is therefore suggested to set this value at the mean sea level
#' above chart datum or a specific tidal elevation of interest.
#' @param Ho Numeric. Initial offshore wave height in meters.
#' @param To Numeric. Initial offshore wave period in second.
#' @param tran_force Boolean TRUE/FALSE. should transect be forced even if there are
#' error codes.
#' @param print_debug Boolean TRUE/FALSE. Turn on function debugging mode.
#'
#' @details Wave attenuation model originally developed by Dr. Greg Guannel
#' for the Coastal Natural Capital InVEST project. This function models wave
#' attenuation along a cross-shore elevation profile. Input parameters include
#' a pre-processed cross shore profile dataset (dat) developed through the
#' sequence of functions show in the example, the total water surface level
#' of still water (total_wsl_adj) above the chart datum, the wave height (Ho)
#' and wave period (To). After running the function populates the original
#' input dataset (dat) with the follow data attributes: wave setup with
#' vegetation (Eta), wave setup without vegetation (Etas), bottom orbital
#' velocity (Ubot), wave height with vegetation (H_veg), wave height without
#' vegetation (H_noveg) and wave dissipation (Dis1). If  you receive a message
#' stating Transect failed - bad sort order, we suggest decreasing the
#' MaxOnshoreDist, adding a trim line or decreasing the water level.
#'
#' @return An object of class sf and data.frame updated with wave data
#' along each cross-shore profile.
#'
#' @references
#' InVEST: Wave Attenuation & Erosion Reduction: Coastal Protection (G. Guannel)
#'
#' Guannel et al. (2014) Integrated modeling framework to quantify the coastal
#' protection services supplied by vegetation. Journal of
#' Geophysical Research: Oceans. DOI: 10.1002/2014JC009821.
#'
#' @examples
#' \dontrun{
#' library(MNAI.CPBT)
#' data(Coastline)
#' # Generate cross-shore profile lines along the coastline.
#' crossshore_profiles <- samplePoints(
#'   Coastline = Coastline,
#'   ShorelinePointDist = 150,
#'   BufferDist = 50,
#'   RadLineDist = 1.5
#' )
#' crossshore_lines <- crossshore_profiles[[2]]
#'
#' # Extract elevation values along each profile
#' rpath <-  system.file("extdata", "TopoBathy.tif", package = "MNAI.CPBT")
#' TopoBathy <- raster::raster(rpath)
#' pt_elevs <- ExtractElev(crossshore_lines, TopoBathy)
#'
#' # Run SignalSmooth function to smooth elevation profiles
#' pt_elevs <- SignalSmooth(point_elev = pt_elevs,
#' SmoothParameter = 5)
#'
#' # Clean the cross-shore profiles with CleanTransect
#' data(Trimline)
#' cleantransect <- CleanTransect(
#'   point_elev = pt_elevs,
#'   RadLineDist = 1.5,
#'   MaxOnshoreDist = 0.01,
#'   trimline = Trimline
#' )
#'
#' # Merge vegetation onto lines
#' data(Vegetation)
#' dat_veg <- ExtractVeg(pt_exp = cleantransect, Vegetation = Vegetation)
#'
#' # Run the wave evolution model
#' wave_data <- WaveModel(dat = dat_veg,
#'   total_wsl_adj = 1.5,
#'   Ho = 2,
#'   To = 8
#' )
#'
#' # Preview individual transect
#' dsub <- wave_data[wave_data$line_id == 2, ]
#'
#' # Plot cross-shore profile
#' par(mfrow = c(2, 1))
#' plot(dsub$Xpos, dsub$elev, type = "l", xlab = "Cross-shore Distance (m)",
#' ylab = "Elevation (m) Chart Datum", main = "ELEVATION PROFILE")
#' points(dsub$Xpos, dsub$elev_smooth, col="red", type = "l")
#'
#' # Add MLLW water line
#' abline(h = 0, lty = 2, col = "blue")
#'
#' # Look at the wave height (without vegetation)
#' plot(dsub$Xpos, dsub$H_noveg, type = 'l', xlab = "Cross-shore Distance (m)",
#' ylab = "Wave Height (m)", main = "WAVE ATTENUATION")
#' # Add on wave height with vegetation
#' points(dsub$Xpos, dsub$H_veg, col="green", type = "l")
#'
#' }
#' @export
WaveModel <- function(
  dat = NA,
  total_wsl_adj = NA,
  Ho = NA,
  To = NA,
  tran_force = FALSE,
  print_debug = FALSE
) {

  math.pi <- pi

  # tmp func
  printDebug <- function(x) {
    if (print_debug) {
      print(x)
    }
  }

  wave_t_data <- list()
  ids <- unique(dat$line_id)


  #=============================================
  # Loop through transects
  #=============================================
  print(paste0("n Transects: ", length(ids)))

  for (ii in seq_len(length(ids))) {

    this_id <- ids[ii]
    this_transect <- dat[which(dat$line_id == this_id), ]

    # constants
    g <- 9.81
    rho <- 1024.0

    # simulated WSE is a combination of the mean high water
    # any storm surge elevation and any sea level rise

    height_array <- this_transect$elev_smooth
    this_transect$height_array <- height_array


    # adjust water level so that 0 is at MSL
    height_array <- height_array - total_wsl_adj
    this_transect$height_array <- height_array



    # ---------------------------------------------------------------------
    # If an island is present only take values - ISLANDS
    # ---------------------------------------------------------------------
    cross_zero <- rootSolve::uniroot.all(stats::approxfun(this_transect$Xpos,
                                        this_transect$height_array),
                              interval = range(this_transect$Xpos))

    if (length(cross_zero) > 1) {

      # if island only take first segment

      # Get cross zero line closest to main shoreline
      start_dip_i <- which.min(abs(cross_zero))
      # determine if really island
      if (start_dip_i == length(cross_zero)){
        end_dip <- Inf
      } else {
        end_dip <- cross_zero[start_dip_i + 1]
      }
      start_dip <- cross_zero[start_dip_i]


      sub1 <- this_transect[which(this_transect$Xpos >= start_dip &
                                    this_transect$Xpos <= end_dip), ]

      # Check validity of depth
      land_case <- stats::median(sub1$height_array, na.rm = TRUE) > 0

      # Check if wrong side of peninsula
      land_case <- stats::median(sub1$Xpos, na.rm = TRUE) > 0

      if (land_case) {


        # reverse subset direction
        sub1 <- this_transect[which(this_transect$Xpos <= start_dip &
                                      this_transect$Xpos >= cross_zero[start_dip_i-1]), ]

        if (tran_force) {

          sub1 <- this_transect[which(this_transect$Xpos >= start_dip),]
          #plot(sub1$Xpos, sub1$elev, type='l'); abline(h=0)

        } else {

          sub1$Xpos <- rev(sub1$Xpos)
          print("Transect failed - bad sort order")
          next

        }

      }



      # Filter onland portion
      mv <- min(sub1$height_array, na.rm=TRUE)
      mp <- which(sub1$height_array == mv)
      mp <- mp[length(mp)]
      sub1 <- sub1[1:mp, ]

      # Reset height array and initial object
      height_array <- sub1$height_array
      this_transect <- sub1
      # The remove second half

    }

    # END OF ISLAND...............................................................


    if(nrow(this_transect) < 10){
      next
    }



    # Only keep values that have negative depth
    keep <- which(height_array < -0.1)

    height_array <- height_array[keep]
    this_transect <- this_transect[keep,]

    # depth is now positive
    height_array <- -1*height_array
    this_transect$height_array <- height_array
    #plot(this_transect$height_array, type='l')

    # reverse order if profile starts at onshore
    height_array <- rev(height_array)
    this_transect <- this_transect[nrow(this_transect):1, ]


    ################################################
    ####### RUN MODEL FOR MANAGEMENT ACTION ########
    ################################################
    printDebug("run model for management action")

    # wave and setup at first grid point
    # wave height at first grid point if it's not a barrier reef

    fp <- 1.0 / To;
    sig <- 2.0 * math.pi * fp  # wave frequency and angular frequency
    ko <- iterativek(sig, height_array[1])  # wave number at 1st grid pt
    Lo <- 2.0 * math.pi / ko  # wave length @ 1st grid pt
    no <- 0.5 * (1 + (2.0 * ko * height_array[1] / math.sinh(2.0 * ko * height_array[1])))  # to compute Cg at 1st grid pt
    Cgo <- Lo / To * no  # phase and group velocity at 1st grid pt
    # w

    # wave height at first grid point
    if(Ho > 0.78 * (height_array[1])){ # check that the wave height is supported by the starting water depth (according to depth limited breaking)
      Ho1 <- 0.78 * (height_array[1])
      log.warning("...Water depth too shallow for input wave height; that wave broke somewhere in deeper water.  We will assume that H = 0.78h");
      #shal <- 1
    } else { # wave not broken; heck if it's in intermediate water
      #shal <- 0
      if(height_array[1] > 0.5 * Lo) {
        Ho1 <- Ho  # we are in deep water
      } else {
        Ho1 <- Ho * math.sqrt(0.5 * g * To / (
          2.0 * math.pi) / Cgo)  # we are in intermediate water; assume no brkg occurred
        # wave setup at first grid point
      }
    }


    # wave setup at first grid point
    Etao1 <- -0.125 * (Ho1 / math.sqrt(2)) ** 2.0 * ko / math.sinh(2.0 * ko * height_array[1])


    Etao <- Etao1

    # initialize variables
    # Xn <- list()
    # ho <- list()
    # Retreat2 <- -9999
    # MErodeLen2 <- -9999
    # EqualRetreat <- "Null"
    # Xaxis <- list()
    # Depth <- list()
    # vector of X distances and depth will be filled with coral and oyster if applicable as we move along profile
    #Wave <- list()
    #WaveMA <- list()
    #Setup <- list()
    #SetupMA <- list()
    Dis1 <- list()
    DisSimple1 <- list()
    #DisMA <- list()
    #DisSimpleMA <- list()
    # vector of wave height and setup
    # will be filled as we move along the profile
    #VegLoc <- list()
    #VegLocMA <- list()
    # vector of vegetation presence/absence will be filled
    #BottVelo <- list()
    #BottVeloMA <- list()
    # vector of bottom velocity to be used if muddy substrate
    #Hshortwave <- Ho
    #HshortwaveMA <- Ho
    # short wave value to use in runup equation


    #-------------------------------------------------
    # Wave Transformation Model
    #-------------------------------------------------
    printDebug("Wave Transformation Model")

    #def WaveModel(X, h, Ho, To, Etao, Roots, Trunk, Canop, VegXloc, TrigRange):
    #log.info('Beginning wave transformation model.')

    # #  wave_height_array, Eta, Hs, Etas, DissAtn1, Ubot = WaveModel(Xtrig
    # htrig
    # Ho1
    # To = To
    Etao <- Etao1 # , Roots, Trunk, Canop, vegetation_x_location, TrigRange)
    # X=Xtrig
    # h=htrig


    # X: Vector of consecutive cross-shore positions going shoreward
    X <- this_transect$Xpos

    if(X[1]>X[length(X)]){
      # decreasing
      X <- rev(this_transect$Xpos)
      this_transect$Xpos_rev <- rev(this_transect$Xpos)
    }

    # h: Vector of water depths at the corresponding cross-shore position
    htrig <- this_transect$height_array
    h <- htrig

    # Ho: Initial wave height to be applied at the first cross-shore position
    # To: Wave Period
    # Etao: Mean Water Level increase due wave set-up at the first cross-shore position

    # Roots: An array of physical properties (density, diameter, and height) of mangrove roots at the corresponding cross-shore position (all zeros if mangroves don't exist at that location)
    #Roots <- rep(0, nrow(this_transect))

    # Trunk: An array of physical properties (density, diameter, and height) of mangrove trunks or the marsh or seagrass plants at the corresponding cross-shore position (all zeros if vegetation don't exist at that location)
    Trunk <- list()
    Trunk[[1]] <- ifelse(is.na(this_transect$StemDensty), 0, this_transect$StemDensty) # Stem Density
    Trunk[[2]] <- ifelse(is.na(this_transect$StemDiam),   0, this_transect$StemDiam) # Stem Diameter
    Trunk[[3]] <- ifelse(is.na(this_transect$StemHeight), 0, this_transect$StemHeight) # Stem Height
    #unique(Trunk[[3]]);

    # Canop: An array of physical properties (density, diameter, and height) of the mangrove canopy at the corresponding cross-shore position (all zeros if mangroves don't exist at that location)
    #Canop <- rep(0, nrow(this_transect))

    # VegXloc: A vector with a numeric code indicate what (if any) natural habitat is present at the cross-shore location.  0 = No Habitat, 1 = Mangrove, 2 = Marsh, 3 = Seagrass, 4 = Coral, 5 = Oyster Reef
    vegetation_x_location <- this_transect$Type
    vegetation_x_location <- as.numeric(as.factor(vegetation_x_location))
    vegetation_x_location <- ifelse(is.na(vegetation_x_location), 0, vegetation_x_location)
    Vegxloc <- vegetation_x_location
    # unique(Vegxloc)

    # TrigRange: Defines the segment of the cross-shore domain where a given model is valid.  Where VegXloc is 0, 1, 2, or 3 the same wave model is applicable.  If a coral or oyster reef is present the model is interupted, the reef model is run and that output is carried onto the next segment of the cross-shore domain.
    #TrigRange <- c(1, nrow(this_transect))


    # constants
    g <- 9.81
    rho <- 1024.0
    B <- 1.0
    Beta <- 0.05
    Cf <- 0.01

    # extract vegetation characteristics
    NRoots <- rep(0, nrow(this_transect))
    dRoots <- rep(0, nrow(this_transect))
    hRoots <- rep(0, nrow(this_transect))

    NTrunk <- Trunk[1][[1]] # Density
    dTrunk <- Trunk[2][[1]] # Diameter
    hTrunk <- Trunk[3][[1]] # Height

    NCanop <- rep(0, nrow(this_transect))
    dCanop <- rep(0, nrow(this_transect))
    hCanop <- rep(0, nrow(this_transect))

    lx <- length(X)
    dx <- 1 #abs(X[2] - X[1])


    # create relative depth values for roots, trunk and canopy
    alphr <- hRoots / h
    alpht <- hTrunk / h
    alphc <- hCanop / h
    for(kk in 1:lx) {
      if(alphr[kk] > 1) {
        alphr[kk] <- 1
        alpht[kk] <- 0
        alphc[kk] <- 0  # roots only
      } else if((alphr[kk] + alpht[kk]) > 1) {
        alpht[kk] <- 1 - alphr[kk]
        alphc[kk] <- 0  # roots and trunk
      } else if((alphr[kk] + alpht[kk] + alphc[kk]) > 1) {
        alphc[kk] <- 1 - alphr[kk] - alpht[kk]  # roots, trunk and canopy
      }
    }


    # drag coefficent for vegetation; mangrove and marsh win over seagrass if they overlap
    CdVeg <- this_transect$Cd
    CdVeg <- ifelse(is.na(CdVeg), 0, CdVeg)
    unique(CdVeg)
    # Run after with signal smooth
    CdVeg <- SignalSmooth_smooth(x=CdVeg, window_len=length(CdVeg) * 0.01)
    #summary(CdVeg)

    printDebug("End of veg initialize")



    # initialize vectors for wave model
    H <- rep(0, lx)  # RMS Wave Height
    Db <- rep(0, lx)
    Df <- rep(0, lx)
    Dveg <- rep(0, lx)
    k <- rep(0, lx)
    L <- rep(0, lx)  # wave number; wave length
    C <- rep(0, lx)
    n <- rep(0, lx)
    Cg <- rep(0, lx)  # wave celerity; shoaling factor group velocity (C*n)
    Er <- rep(0, lx)
    Ef <- rep(0, lx)
    Br <- rep(0, lx)  # roller energy; energy flux; roller flux
    Hs <- rep(0, lx)
    Etas <- rep(0, lx)  # wave height; setup in the absence of vegetation
    Dbs <- rep(0, lx)
    Dfs <- rep(0, lx)
    Ers <- rep(0, lx)  # dissipation due to breaking; dissipation due to bottom friction; roller energy
    Efs <- rep(0, lx)
    Brs <- rep(0, lx)  # energy flux in the absence of veg; roller flux in the absence of vegetation

    # wave parameter at 1st grid pt
    ash <- h  # ash is same as h, but is now an independent variable
    fp <- 1.0 / To;
    sig <- 2.0 * math.pi * fp  # wave frequency and angular frequency
    k[1] <- iterativek(sig, h[1])  # wave number at 1st grid pt
    L[1] <- 2.0 * math.pi / k[1]  # wave length @ 1st grid pt
    n[1] <- 0.5 * (1 + (2.0 * k[1] * h[1] / math.sinh(2.0 * k[1] * h[1])))
    # to compute Cg at 1st grid pt
    C[1] <- L[1] / To
    Cg[1] <- C[1] * n[1]  # phase and group velocity at 1st grid pt
    So <- Ho / L[1]  # deep water wave steepness
    Gam <- 0.5 + 0.4 * math.tanh(33.0 * So)
    # Gam from Battjes and Stive 85 (as per Alsina & Baldock)


    # RMS wave height at first grid point; assume no dissipation occurs
    H[1] <- Ho / math.sqrt(2.0)
    # transform significant wave height to rms wave height
    Ef[1] <- 0.125 * rho * g * H[1] ** 2 * Cg[1]
    Efs[1] <- Ef[1]
    Hs[1] <- H[1]  # energy flux @ 1st grid pt




    #-----------------------------------------------------------
    # begin wave mode;
    printDebug("begin wave mode")

    for(xx in 1:(lx-1)) {
      # transform waves,take MWL into account

      # wave in presence of veg.
      Ef[xx] <- 0.125 * rho * g * (H[xx] ** 2.0) * Cg[xx]
      # Ef at (xx)
      Ef[xx + 1] <- Ef[xx] - dx * (Db[xx] + Df[xx] + Dveg[xx])
      # Ef at [xx+1] (subtract dissipation due to: breaking,
      #bottom friction, and vegetation)

      # phase and group velocity
      k[xx + 1] <- iterativek(sig, h[xx + 1])
      # compute wave number
      n[xx + 1] <- 0.5 * (1.0 + (2.0 * k[xx + 1] * h[xx + 1] /
                                  math.sinh(2.0 * k[xx + 1] * h[xx + 1])))
      C[xx + 1] <- sig / k[xx + 1];
      Cg[xx + 1] <- C[xx + 1] * n[xx + 1]  # phase and group velocity

      # roller info
      H[xx + 1] <- math.sqrt(8.0 * Ef[xx + 1] / (rho * g * Cg[xx + 1]))
      # wave height at [xx+1]
      Br[xx + 1] <- Br[xx] - dx * (g * Er[xx] * math.sin(Beta) /
                                     C[xx] - 0.5 * Db[xx])
      # roller flux
      Er[xx + 1] <- Br[xx + 1] / (C[xx + 1])  # roller energy

      # dissipation due to brkg
      Var <- 0.25 * rho * g * fp * B
      Hb <- 0.88 / k[xx + 1] * math.tanh(Gam * k[xx + 1] * h[xx + 1] / 0.88)
      temp1 <- ((Hb / H[xx + 1]) ** 3.0 + 1.5 * Hb / H[xx + 1]) *
        exp(-(Hb / H[xx + 1]) ** 2.0)

      if(!(is.na(H[xx + 1]))) {
        temp2 <- 0.75 * math.sqrt(math.pi) * (1 - pracma::erf(Hb / H[xx + 1]))
      } else {
        temp2 <- 0
      }


      # dissipation due to brkg
      Db[xx + 1] <- Var * H[xx + 1] ** 3 / h[xx + 1] *
        (temp1 + temp2)

      # dissipation due to bot friction
      Df[xx + 1] <- rho * Cf / (12.0 * math.pi) *
        (2.0 * math.pi * fp * H[xx + 1] /
           math.sinh(k[xx + 1] * h[xx + 1])) ** 3.0


      # dissipation due to vegetation
      # Roots
      V1 <- 3 * math.sinh(k[xx + 1] * alphr[xx + 1] * h[xx + 1]) +
        math.sinh(k[xx + 1] * alphr[xx + 1] * h[xx + 1]) ** 3  # roots

      # Trunks
      V2 <- (3 * math.sinh(k[xx + 1] * (alphr[xx + 1] + alpht[xx + 1]) *
                            h[xx + 1]) - 3 * math.sinh(
        k[xx + 1] * alphr[xx + 1] * h[xx + 1]) +
          math.sinh(k[xx + 1] * (alphr[xx + 1] + alpht[xx + 1]) *
                      h[xx + 1]) ** 3 -
          math.sinh(k[xx + 1] * alphr[xx + 1] * h[xx + 1]) ** 3)
      # trunk

      # Canopy
      V3 <- (3 * math.sinh(k[xx + 1] * (alphr[xx + 1] + alpht[xx + 1] +
                                         alphc[xx + 1]) * h[xx + 1])
            - 3 * math.sinh(k[xx + 1] * (alphr[xx + 1] + alpht[xx + 1]) *
                              h[xx + 1]) +
              math.sinh(k[xx + 1] * (alphr[xx + 1] + alpht[xx + 1] +
                                       alphc[xx + 1]) * h[xx + 1]) ** 3 -
              math.sinh(k[xx + 1] * (alphr[xx + 1] + alpht[xx + 1]) *
                          h[xx + 1]) ** 3)  # canopy


      CdDN <- CdVeg[xx + 1] * (
        dRoots[xx + 1] * NRoots[xx + 1] * V1 +
          dTrunk[xx + 1] * NTrunk[xx + 1] * V2 +
          dCanop[xx + 1] * NCanop[xx + 1] * V3)


      temp1 <- rho * CdDN * (k[xx + 1] * g / (2.0 * sig)) ** 3.0 /
        (2.0 * math.sqrt(math.pi))
      temp3 <- (3.0 * k[xx + 1] * math.cosh(k[xx + 1] *
                                             h[xx + 1]) ** 3)

      Dveg[xx + 1] <- temp1 / temp3 * H[xx + 1] ** 3
      # dissipation due to vegetation

      # wave in absence of vegetation
      Hs[xx + 1] <- H[xx + 1]


      #============================================
      # compute if there's vegetation (time saver)

      if(sum(Vegxloc) != 0){

        Efs[xx] <- 0.125 * rho * g * (Hs[xx] ** 2.0) * Cg[xx]  # Ef at (xx)
        Efs[xx + 1] <- Efs[xx] - dx * (Dbs[xx] + Dfs[xx])  # Ef at [xx+1]

        Hs[xx + 1] <- math.sqrt(8.0 * Efs[xx + 1] / (rho * g * Cg[xx + 1]))
        # wave height at [xx+1]
        Brs[xx + 1] <- Brs[xx] - dx * (g * Ers[xx] * math.sin(Beta) /
                                        C[xx] - 0.5 * Dbs[xx])  # roller flux
        Ers[xx + 1] <- Brs[xx + 1] / (C[xx + 1])
        # roller energy

        temp1 <- ((Hb / Hs[xx + 1]) ** 3.0 + 1.5 * Hb / Hs[xx + 1]) *
          exp(-(Hb / Hs[xx + 1]) ** 2.0)


        if(!(is.na(Hs[xx + 1]))) {
          temp2 <- 0.75 * math.sqrt(math.pi) * (1 - pracma::erf(Hb / Hs[xx + 1]))
        } else {
          temp2 <- 0
        }

        # dissipation due to brkg
        Dbs[xx + 1] <- Var * Hs[xx + 1] ** 3 / h[xx + 1] * (temp1 + temp2)

        # dissipation due to bottom friction
        Dfs[xx + 1] <- rho * Cf / (12.0 * math.pi) *
          (2.0 * math.pi * fp * Hs[xx + 1] / math.sinh(
          k[xx + 1] * h[xx + 1])) ** 3.0

      } # end of compute if veg time saver

    } # end of for(xx in 1:lx)

    printDebug("end of wave model")
    # end of wave model
    #--------------------------------------------

    # energy density
    Ew <- rep(0, lx)
    Ew <- 0.125 * rho * g * (H ** 2.0)

    # energy density in the absence of vegetation
    Ews <- rep(0, lx)
    Ews <- 0.125 * rho * g * (Hs ** 2.0)


    # force on plants if they were emergent
    # take a portion if plants occupy only portion of wc

    Fxgr <- (rho * g * CdVeg * dRoots * NRoots * H ** 3.0 * k) /
      (12.0 * math.pi * math.tanh(k * ash))

    Fxgt <- (rho * g * CdVeg * dTrunk * NTrunk * H ** 3.0 * k) /
      (12.0 * math.pi * math.tanh(k * ash))

    Fxgc <- (rho * g * CdVeg * dCanop * NCanop * H ** 3.0 * k) /
      (12.0 * math.pi * math.tanh(k * ash))

    # scale by height of indiv. elements
    fx <- (-alphr*Fxgr) - (alpht * Fxgt) - (alphc * Fxgc)
    #fx <- ifelse(is.na(fx), NA, fx)


    # estimate MWS
    dx <- 1
    Xi <- seq(from=X[1], to=utils::tail(X, 1), by=dx)


    if(length(Xi) < 10){
      print("Transect failed - all back length or islands...")
      next
    }



    # use smaller dx to get smoother result
    lxi <- length(Xi)

    #F <- interp1d(X, ash);

    Ff <- stats::approx(X, ash, n=length(X))
    ashi <- Ff$y

    Ff <- stats::approx(X, k, n=length(X))
    ki <- Ff$y

    Ff <- stats::approx(X, Ew, n=length(X))
    Ewi <- Ff$y

    Ff <- stats::approx(X, Er, n=length(X))
    Eri <- Ff$y


    if(all(is.na(fx))){
      print("Transect failed - all NaN 065")
      next
    }
    Ff <- stats::approx(X, fx, n=length(X))
    fxi <- Ff$y

    Sxx <- rep(0, length(X))
    Rxx <- rep(0, length(X))
    Eta <- rep(0, length(X))
    O <- 0


    printDebug("iterate until convergence of water level")
    # iterate until convergence of water level
    while(O < 8){

      # water depth
      hi <- ashi + Eta

      # wave radiation stress
      Sxx <- (0.5 * Ewi * (4.0 * ki * hi / math.sinh(2.0 * ki * hi) + 1.0))

      # roller radiation stress
      Rxx <- 2.0 * Eri

      # estimate MWL along Xshore transect
      temp1 <- Sxx + Rxx
      temp2 <- pracma::gradient(temp1, dx)

      Integr <- (-temp2 + fxi) / (rho * g * hi)

      Eta[1] <- Etao
      Eta[2] <- Eta[1] + Integr[1] * dx


      for(i in 2:(lxi - 2)){
        Eta[i + 1] <- Eta[i - 1] + Integr[i] * 2 * dx
        #print(i)
      }

      Eta[lxi] <- Eta[lxi - 1] + Integr[lxi] * dx

      O <- O + 1
      # End of while O < 8

    }


    printDebug("approx functions")
    # MJB Added for ESRI tool..
    if(length(Xi) != length(Eta)) {
      Xi <- seq_len(length(Eta))
    }


    Ff <- stats::approxfun(Xi, Eta, na.rm=FALSE)

    Eta <- Ff(X)

    Etas <- Ff(X)

    # compute if there's vegetation (time saver)
    if(sum(Vegxloc) != 0){

      Sxxs <- rep(0, lx)
      Rxxs <- rep(0, lx)
      Etas <- rep(0, lx)
      dx <- 1
      O <- 0

      # iterate until convergence of water level
      while(O < 5) {
        # water depth
        h <- ash + Etas
        # wave radiation stress
        Sxxs <- 0.5 * Ews * (4.0 * k * h / math.sinh(2.0 * k * h) + 1.0)
        # roller radiation stress
        Rxxs <- 2.0 * Ers
        # estimate MWL along Xshore transect
        temp1 <- Sxxs + Rxxs
        temp2 <- pracma::gradient(temp1, dx)

        Integr <- (-temp2) / (rho * g * h)
        Etas[1] <- Etao
        Etas[2] <- Eta[1] + Integr[1] * dx


        for(i in 2:(lx - 2)){
          Etas[i + 1] <- Etas[i - 1] + Integr[i] * 2 * dx
          #print(i)
        }

        Etas[lx] <- Etas[lx - 1] + Integr[lx] * dx

        O <- O + 1
        # End of while O < 8
      }

    }

    # bottom velocity (with veg)
    Ubot <- math.pi * H / (To * math.sinh(k * h))

    #plot(Ubot, type='l')
    printDebug("bottom velocity")




    H <- H * math.sqrt(2)
    Hs <- Hs * math.sqrt(2)

    # Ds1=num.array(H); Ds2=num.array(Hs)
    Ds1 <- pracma::gradient(Ef, dx);
    Ds1 <- -Ds1

    # compute if there's vegetation (time saver)
    if(sum(Vegxloc) != 0){
      Ds2 <- pracma::gradient(Efs, dx)
      Ds2 <- -Ds2
    } else {
      Ds2 <- Ds1
    }

    # dissipation difference
    #Diss <- list(Ds1, Ds2) # dissipation difference

    npm <- mean(c(mean(Ds1), mean(Ds2)))

    if(is.na(npm)) {
      Diss <- list(Ds1 * 0.0, Ds1 * 0.0)
    }

    # Complete Dis1 dissipation
    temp1 <- H ** 3
    Dis1 <- temp1
    temp2 <- Hs ** 3
    DisSimple1 <- temp2
    #temp <- Dis1 / DisSimple1
    printDebug("Complete Dis1 dissipation")

    # H = wave height,
    # Eta = wave setup
    # Hs = wave height w/o veg.
    # Etas = wave setup w/o veg.
    # Diss = wave dissipation,
    # Ubot =  bottom wave orbital velocity over the cross-shore domain

    printDebug("Finalizae format")
    # add other columns
    this_transect$Eta <- Eta
    this_transect$Etas <- Etas
    #this_transect$Diss <- Diss
    this_transect$Ubot <- Ubot

    # bottom velocity (veg)


    # rebuild export objects
    this_transect$H_veg    <- round(H, 3)
    this_transect$H_noveg  <- round(Hs, 3)


    this_transect$Dis1 <- Dis1
    this_transect$DisSimple1 <- DisSimple1

    # Remove this column if present
    this_transect$Xpos_rev <- NULL


    # add to list
    wave_t_data[[ii]] <- this_transect

    #print(ii)


  } # end of loop through transects

  export_this <- do.call("rbind", wave_t_data)


  return(export_this)



} # end of function


