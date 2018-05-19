#### Utility functions for package esg2 ####
############################################

## Compute the price of a zero coupon bond

#' Zero Coupon Price
#'
#' \code{rate2zcp} computes the price of a zero coupon bond for given maturities and rates.
#'
#' @param mat Vector. Maturities of the bound.
#' @param rate Vector. Panel of rates for which the price is to be computed.
#' @param method Character. Computation method:
#' \itemize{
#'   \item{"continuous": continuous rate. Default value.}
#'   \item{"actuarial": actuarial rate.}
#'   \item{"libor": libor rate.}}
#'
#' @return The zero coupon prices associated to the maturity.
#'
#' @examples rate2zcp(1, 0.01, "continuous")
#'
#' @export
rate2zcp <- function(mat, rate, method = "continuous"){
  if(!(method %in% c("continuous", "actuarial", "libor"))) stop("Invalid method choice")
  if(method == "continuous"){
    return(exp( - mat * rate ))
  } else if(method == "actuarial"){
    return((1 + rate) ^ ( - mat))
  } else{
    return(1 / (1 + rate * mat))
  }
}

##############################
##############################

## Rate computation

#' Rate computation
#'
#' \code{zcp2rate} computes the rate equivalent to a zero coupon price for a maturity.
#'
#' @param mat Vector. Maturities of the bound.
#' @param zcp Vector. Panel of zero coupon bonds prices for which the rates are to be computed.
#' @param method Character. Computation method:
#' \itemize{
#'   \item{"continuous": continuous rate. Default value.}
#'   \item{"actuarial": actuarial rate.}
#'   \item{"libor": libor rate.}}
#'
#' @return The rates associated to the maturities.
#'
#' @examples zcp2rate(1, 0.9900498, "continuous")
#'
#' @export
zcp2rate <- function(mat, zcp, method = "continuous"){
  if(!(method %in% c("continuous", "actuarial", "libor"))) stop("Invalid method choice")
  if(method == "continuous"){
    return(-log( zcp)/mat)
  } else if(method == "actuarial"){
    return(zcp ^ (- 1 / mat) - 1)
  } else{
    return((1 / zcp -1) / mat)
  }
}

##############################
##############################

# Projection formulas for G2++

B_g2 <- function(z, t, T){
  return((1-exp(-z*(T-t)))/z)
}

V_g2 <- function(a, b, sigma, eta, rho, t, T){
  ta <- T - t - 2 * B_g2(a, t, T) + B_g2(2*a, t, T)
  tb <- T - t - 2 * B_g2(b, t, T) + B_g2(2*b, t, T)
  tab <- T - t - B_g2(a, t, T) - B_g2(b, t, T) + B_g2(a+b, t, T)
  return(sigma^2/a^2*ta + eta^2/b^2*tb + 2*rho*sigma*eta/(a*b)*tab)
}

A_g2 <- function(a, b, sigma, eta, rho, t, T, xt, yt){
  return(1/2*(V_g2(a, b, sigma, eta, rho, t, T) - V_g2(a, b, sigma, eta, rho, 0, T) + V_g2(a, b, sigma, eta, rho, 0, t)) - B_g2(a, t, T) * xt - B_g2(b, t, T) * yt )
}

phi_g2 <- function(f0, a, b, sigma, eta, rho, T){
  return(f0 + sigma^2/2*B_g2(a, 0, T)^2 + eta^2/2*B_g2(b, 0, T)^2 + rho*sigma*eta*B_g2(a, 0, T)*B_g2(b, 0, T))
}

next_step_g2 <- function(xt, a, sigma, Wt){
  return(xt*exp(-a) + Wt * sigma * sqrt(B_g2(2*a, 0, 1)))
}

M_g2 <- function(a, b, sigma, eta, rho, T){
  r <- (sigma^2/a^2 + rho * sigma * eta / (a*b))*(1-exp(-a*T)) - sigma^2/(2*a^2)*(1 - exp(-2*a*T)) - rho*sigma*eta/(b*(a+b)) *(1-exp(-(a+b)*T))
  return(r)
}

##############################
##############################

## Generation of correlated variables

#' Generate correlated variables
#'
#' \code{genW} generates different sets of centered normal distribution tables with specified correlations.
#'
#'
#' @param correl Matrix. Correlation matrix for the inputs.
#' @param s Numeric. Number of simulation. Default to 1000.
#' @param p Number of periods. Default to 50.
#' @return An array of dimension s * p * dim(correl) corresponding to the simulations.
#'
#' @examples
#' correl <- cbind(c(1,-0.9, 0.25, 0.25),c(-0.9,1, 0, 0), c(0.25, 0, 1, 0.25), c(0.25, 0, 0.25, 1))
#' varCor <- genW(correl, 1000, 50)
#'
#' @export
genW=function(correl, s = 1000, p = 50)
{
  # Number of assets
  n=nrow(correl)
  # Generate random vectors with normal centered distribution
  V_norm=rnorm(s*p*n,mean=0,sd=1)
  V_norm=array(V_norm,dim=c(s,p,n))
  # Cholesky decomposition
  R=chol(correl)
  # Correlate the values
  V=array(0,dim=c(s,p,n))
  for(i in 1:s)
  {
    V[i,,]=V_norm[i,,] %*% R
  }
  return(V)
}

##############################
##############################

## Validity check for class ZCCurve

check_zc <- function(object){
  if(!(object@method %in% c("continuous", "actuarial", "libor"))) stop("Invalid method for class ZCCurve.")
}

## Validity check for class swaptions

check_swaptions <- function(object){
  if(!(object@type %in% c("lognormal", "normal"))) stop("Invalid type choice. Should be either normal or lognormal.")
  if(object@freq %% 1 != 0 | object@freq <=0) stop("Invalid frequence. Should be an integer superior to 0.")
  if(!all(c(length(object@tenor), length(object@vol)) == length(object@mat))) stop("Inconsistent lengths between maturities, tenors, and volatilities vectors.")
}

## Validity check for class BS

check_bs <- function(object){
  if(object@s0 <= 0) stop("Invalid start value for the projection.")
  if(object@vol < 0) stop("Invalid volatility value for the projection.")
  if(object@div < 0) stop("Invalid dividend rate value for the projection.")
  if(object@rho < -1 | object@rho > 1) stop("Invalid correlation value for the projection.")
}

## Validity check for class G2

check_g2 <- function(object){
  if(object@horizon %% 1 != 0 | object@horizon < 0) stop("Invalid horizon. Should be an integer superior to 0.")
  if(object@nsimul %% 1 != 0 | object@nsimul < 0) stop("Invalid number of simulation. Should be an integer superior to 0.")
}

## Validity check for class VSK

check_vsk <- function(object){
  if(object@sigma < 0) stop("Invalid volatility value for the projection.")
}
