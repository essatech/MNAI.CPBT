#' Custom ErosionKD
#' @keywords internal
ErosionKD <- function(A, Ho, TWL, B, D, W, m, count, To, storm_duration){
  # A: Shape factor for the Equilibrium Beach Profile associated with the sediment size
  # Ho: Deep Water Wave Height
  # TWL: Total Water Level (Surge plus Setup)
  # B: Berm Elevation
  # D: Dune Height
  # W: Dune Width
  # m: Foreshore Slope

  # For testing:
  # TWL = Rnp1
  # B = B1
  # D = D1
  # W = W1
  # m = mo
  # count = inundationcount
  if(FALSE){
    # Testing only
    A = 0.202 # 0.173
    Ho = 2.5
    TWL = 0.7454
    B = 1
    D = 2
    W = 10
    m = 0.04
    count = 0
    To = 7
    storm_duration = 3
  }

  math.pi = pi

  # constants
  g = 9.81
  BD = D + B;
  msg = 0  # tracks whether or not certain messages should be displayed
  B_adj1 = 0  # The berm may need to be elevated if TWL value is too great (first method)
  B_adj2 = 0  # The berm may need to be elevated if TWL value is too great (second method)
  inundation = 0  # tracks whether or not the backshore is inundated

  # Erosion model 1
  hb = (((Ho ** 2.0) * g * To / (2 * math.pi)) / 2.0) ** (2.0 / 5.0) / (
    g ** (1.0 / 5.0) * 0.73 ** (4.0 / 5.0))  # breaking depth
  xb = (hb / A) ** 1.5;
  Hb = 0.78 * hb  # cross-shore breaking location


  if(B + hb <= TWL / 2){
    testval = -9999
    while(testval < .1){
      B_adj1 = B_adj1 + 0.5
      testval = B_adj1 + B + hb - TWL / 2
    }
    BD1 = B + B_adj1 + D
    B1 = B + B_adj1
  } else {
    BD1 = BD
    B1 = B
  }


  # erosion without taking width into account
  Term1 = xb - hb / m
  Rinf1 = (TWL * Term1) / (B1 + hb - TWL / 2.)

  if(D > 0){
    # potential erosion distance
    Rinf = (TWL * Term1) / (BD1 + hb - TWL / 2.) - W * (B1 + hb - 0.5 * TWL) / (
      BD1 + hb - 0.5 * TWL)

    if(Rinf < 0){
      Rinf = (TWL * Term1) / (B1 + hb - TWL / 2.)
      msg = 1 + msg
    }

  } else {
    Rinf = Rinf1
  }

  # response time scale
  TS = (320.0 * (Hb ** (3.0 / 2.0) / (g ** .5 * A ** 3.0)) * (
    1.0 / (1.0 + hb / BD1 + (m * xb) / hb))) / 3600.0

  BetaKD = 2.0 * math.pi * (TS / storm_duration)

  # solve this numerically
  fn = function(x){
    exp(-2.0*x/BetaKD)-cos(2.0*x)+(1.0/BetaKD)*math.sin(2.0*x)
  }

  FindRootKD <- function(fun, a, b, tol=1e-16){
    c = (a + b) / 2
    while(abs(fun(c)) > tol){
      if(a == c | b == c){
        break
      }
      if(sign(fun(c)) == sign(fun(b))){
        b = c
      } else {
        a = c
      }
      c = (a+b)/2
    }
    return(c)
  }

  # find zero in function,initial guess from K&D
  z = FindRootKD(fn, math.pi, math.pi / 2)

  # final erosion distance
  R01 = 0.5 * Rinf * (1.0 - cos(2.0 * z))




  # 2nd method
  x = seq(0, 9999, 1)
  y = A * x ** (2.0 / 3)
  y = rev(y)
  y = y[which(y >= 0.5)]
  x = x[which(y >= 0.5)]

  out = WaveModelSimple(X=x, h=y, Ho, To, Cf=0.01)

  # Hs = out$H
  Etas = out$Eta

  hb = y[which(Etas == min(Etas))]
  xb = (hb / A) ** 1.5  # surf zone width

  # if this condition is true, the K&D model with blow up or given erroneous values
  if(B + hb <= TWL / 2){
    testval = -9999
    while(testval < 1){
      B_adj2 = 0.5 + B_adj2
      testval = B_adj2 + B + hb - TWL / 2
    }
    BD2 = B + B_adj2 + D
    B2 = B + B_adj2
  } else {
    BD2 = BD
    B2 = B
  }

  Term1 = xb - hb / m
  Rinf1 = (TWL * Term1) / (B2 + hb - TWL / 2)  # erosion without taking width into account

  if(D > 0){
    Rinf = (TWL * Term1) / (BD2 + hb - TWL / 2) - W * (B2 + hb - 0.5 * TWL) / (
      BD2 + hb - 0.5 * TWL)  # potential erosion distance

    if(Rinf < 0){
      Rinf = (TWL * Term1) / (B2 + hb - TWL / 2);
      msg = 1 + msg
    }
  } else {
    Rinf = Rinf1

  }


  TS = (320.0 * (Hb ** (3.0 / 2.0) / (g ** .5 * A ** 3.0)) * (
    1.0 / (1.0 + hb / BD2 + (m * xb) / hb))) / 3600.0  # response time scale

  BetaKD = 2.0 * math.pi * (TS / storm_duration)


  # solve this numerically
  # find zero in function,initial guess from K&D
  fn = function(x){
    exp(-2.0*x/BetaKD)-cos(2.0*x)+(1.0/BetaKD)*math.sin(2.0*x)
  }
  z = FindRootKD(fn, math.pi, math.pi / 2)

  R02 = 0.5 * Rinf * (1.0 - cos(2.0 * z))  # final erosion distance


  R0 = max(c(R01, R02))


  if(R0 < 0){
    R0 = 0
  }

  B_adj = min(c(B_adj1, B_adj2))
  # return the minimum of the required berm elevation adjustments
  #('0' if one or both methods did not require adjusting
  # returns:

  # returns:
  # R0 = the average retreat value,
  # R01 = the retreat values by method 1,
  # R02 = by method 2,
  # B_adj = berm increase value,
  # inundation = whether or not inundation occured,
  # count =  and a variable tracking what message to display later.

  if(FALSE){
    print("===============================")
    print("===============================")
    print(paste0("A: ", A))
    print(paste0("R0: ", R0))
    print(paste0("R01: ", R01))
    print(paste0("R02: ", R02))
    print(paste0("B_adj: ", B_adj))
    print(paste0("inundation: ", inundation))
    print(paste0("count: ", count))
    print("===============================")
    print("===============================")
  }



  out <- list()
  out['R0']<- R0
  out['R01']<- R01
  out['R02']<- R02
  out['B_adj']<- B_adj
  out['inundation']<- inundation
  out['count']   <- count


  return(out)

}
