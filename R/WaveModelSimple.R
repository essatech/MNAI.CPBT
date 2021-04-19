#' Custom WaveModelSimple from InVEST tool
#' @param X a number, distance
#' @param h a number, depth
#' @param Ho a number, wave height
#' @param To a number, wave period
#' @param Cf a number, coef
#' @keywords internal
WaveModelSimple <- function(
    X, h, Ho, To, Cf = 0.01
) {

    math.pi = pi

    # constants
    g = 9.81
    rho = 1024.0;
    B = 1.0
    Beta = 0.05
    lx = length(X)
    dx = X[2] - X[1]

    flat = 0

    # initialize vectors for wave model
    H = rep(0, lx)
    Eta = rep(0, lx)
    L = rep(0, lx)
    Db = rep(0, lx)
    Df = rep(0, lx)
    Er = rep(0, lx)
    Ef = rep(0, lx)
    Br = rep(0, lx)
    C = rep(0, lx)
    n = rep(0, lx)
    Cg = rep(0, lx)
    k = rep(0, lx)

    # wave parameter at 1st grid pt
    ash = h  # ash is same as h, but is now an independent variable
    fp = 1.0 / To
    sig = 2.0 * math.pi * fp
    H[1] = Ho / math.sqrt(2.0)  # transform significant wave height to rms wave height
    Ef[1] = 0.125 * rho * g * H[1] ** 2 * Cg[1]  # energy flux @ 1st grid pt
    k[1] = iterativek(sig, h[1])  # wave number at 1st grid pt
    L[1] = 2.0 * math.pi / k[1]  # wave length @ 1st grid pt
    n[1] = 0.5 * (1 + (2.0 * k[1] * h[1] / math.sinh(2.0 * k[1] * h[1])))  # to compute Cg at 1st grid pt
    C[1] = L[1] / To;
    Cg[1] = C[1] * n[1]  # phase and group velocity at 1st grid pt

    So = Ho / L[1]  # deep water wave steepness
    Gam = 0.5 + 0.4 * math.tanh(33.0 * So)  # Gam from Battjes and Stive 85 (as per Alsina & Baldock)


    # begin wave model
    # transform waves,take MWL into account
    for(xx in 1:(lx-1)){

      Ef[xx] = 0.125 * rho * g * (H[xx] ** 2.0) * Cg[xx]  # Ef at (xx)
      Ef[xx + 1] = Ef[xx] - dx * (Db[xx] + Df[xx])  # Ef at [xx+1]

      k[xx + 1] = iterativek(sig, h[xx + 1])
      n[xx + 1] = 0.5 * (1.0 + (2.0 * k[xx + 1] * h[xx + 1] / math.sinh(2.0 * k[xx + 1] * h[xx + 1])))
      C[xx + 1] = sig / k[xx + 1];
      Cg[xx + 1] = C[xx + 1] * n[xx + 1]  # phase and group velocity

      H[xx + 1] = math.sqrt(8.0 * Ef[xx + 1] / (rho * g * Cg[xx + 1]))  # wave height at [xx+1]
      Br[xx + 1] = Br[xx] - dx * (g * Er[xx] * math.sin(Beta) / C[xx] - 0.5 * Db[xx])  # roller flux
      Er[xx + 1] = Br[xx + 1] / (C[xx + 1])  # roller energy

      Var = 0.25 * rho * g * fp * B
      Hb = 0.88 / k[xx + 1] * math.tanh(Gam * k[xx + 1] * h[xx + 1] / 0.88)

      temp1 = ((Hb / H[xx + 1]) ** 3.0 + 1.5 * Hb / H[xx + 1]) * exp(-(Hb / H[xx + 1]) ** 2.0)

      if(!(is.na(H[xx + 1]))){
        temp2 = 0.75 * math.sqrt(math.pi) * (1 - pracma::erf(Hb / H[xx + 1]))
      } else {
        temp2 = 0
      }

      # dissipation due to brkg
      Db[xx + 1] = Var * H[xx + 1] ** 3 / h[xx + 1] * (temp1 + temp2)

      # dissipation due to bottom friction
      Df[xx + 1] = 1.0 * rho * Cf / (12.0 * math.pi) * (2.0 * math.pi * fp * H[xx + 1] / math.sinh(
        k[xx + 1] * h[xx + 1])) ** 3.0

    } # end of loop through xx

    Ew = rep(0, lx)
    Ew = 0.125*rho*g*(H**2.0)


    # estimate MWS
    val = 1.0;
    dell = 1.0;
    O = 0.0  # for setup calculations
    Sxx = rep(0, lx)
    Rxx = rep(0, lx)

    countr <- 1
    while(dell > 1e-10){

      # added escape
      countr <- countr+1
      if(countr > 1000){
        break
      }

      val1 = val
      h = ash + Eta  # water depth

      Sxx = (0.5 * Ew * (4.0 * k * h / math.sinh(2.0 * k * h) + 1.0))  # wave radiation stress
      Rxx = 2.0 * Er # roller radiation stress
      # estimate MWL along Xshore transect
      Tem = Sxx + Rxx
      Temp = pracma::gradient(Tem, dx)
      Terms = -Temp
      Integr = Terms / (rho * g * h)

      Eta[1] = -0.125 * H[1] ** 2.0 * k[1] / math.sinh(2.0 * k[1] * h[1])
      Eta[2] = Eta[1] + Integr[1] * dx

      end <- lx-2
      for(i in 2:end){
        Eta[i + 1] = Eta[i - 1] + Integr[i] * 2 * dx
      }

      Eta[lx - 1] = Eta[lx - 2] + Integr[lx - 1] * dx


      Temp_eta = pracma::gradient(Eta, dx)
      valn = Temp + rho * g * h * Temp_eta
      valn[lx - 3] = 0
      valn[lx - 2] = 0
      valn[lx - 1] = 0
      dell1 = valn - val1
      dell1me = sum(dell1) / len(dell1)
      dell1mx = max(dell1)

      O = O + 1
      if (O < 5){
        dell = 13
      } else {
        dell = max(dell1me, dell1mx)
      }



    }


    H = H * math.sqrt(2)

    retobj <- list()

    retobj[['H']] <- H
    retobj[['Eta']] <- Eta


    return(retobj)



}
