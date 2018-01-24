#' Swaptions
#'
#' An S4 class to represent ATM swaptions for calibration of a G2++ model.
#'
#' @slot type Character. Type of volatility:
#' \itemize{
#'   \item{"normal".}
#'   \item{"lognormal". Default value}}
#' @slot curve ZCCurve. Zero coupon curve to use for pricing.
#' @slot mat Vector. List of maturities.
#' @slot tenor Vector. List of tenors.
#' @slot vol Vector. List of volatilities corresponding to maturities and tenors.
#' @slot freq Numeric. Number of payments by period.
#' @slot price Vector. Swaptions prices.
#'
#' @include utils.R ZCCurve-class.R
#' @exportClass Swaptions
setClass(Class = "Swaptions",
         representation = representation(
           type = "character",
           curve = "ZCCurve",
           mat = "vector",
           tenor = "vector",
           vol = "vector",
           freq = "numeric",
           price = "vector"
         )
)

############################################
############################################

# Initializator for class Swaptions
setMethod(
  f = "initialize",
  signature = "Swaptions",
  definition = function(.Object, type, curve, mat, tenor, vol, freq){
    # Assignation of objects
    .Object@type <- type
    .Object@curve <- curve
    .Object@mat <- mat
    .Object@tenor <- tenor
    .Object@vol <- vol
    .Object@freq <- freq
    # Computation of swaptions prices
    zcp <- curve@zcp
    n <- length(mat)
    p <- rep(NA, n)
    for(i in 1:n){
      matSwap <- mat[i]
      tenorSwap <- tenor[i]
      volSwap <- vol[i]
      times_payments <- seq(matSwap + 1/freq, matSwap + tenorSwap, by = 1 / freq)
      amounts_payments <- approx(x = 1:length(zcp), y = zcp, xout = times_payments)$y
      payments <- 1/freq * sum(amounts_payments)
      s0 <- (approx(x = 1:length(zcp), y = zcp, xout = matSwap)$y - approx(x = 1:length(zcp), y = zcp, xout = matSwap + tenorSwap)$y) / payments
      if(type == "lognormal"){
        d1 <- volSwap * sqrt(matSwap) / 2
        d2 <- d1 - volSwap * sqrt(matSwap)
        p[i] <- s0 * (pnorm(d1) - pnorm(d2)) * payments
      } else{
        d <- 0
        p[i] <- volSwap * (dnorm(d)) * payments
      }
    }
    .Object@price <- p
    # Return of the object
    return(.Object)
  }
)

############################################
############################################

# User constructor function

#' Swaptions: constructor
#'
#' @param type Character. Type of volatility:
#' \itemize{
#'   \item{"normal".}
#'   \item{"lognormal". Default value}}
#' @param curve ZCCurve. Zero coupon curve to use for pricing.
#' @param mat Vector. List of maturities.
#' @param tenor Vector. List of tenors.
#' @param vol Vector. List of volatilities corresponding to the maturities and tenors.
#' @param freq Numeric. Number of payments by period. Default value to 1.
#'
#' @return A Swaptions object.
#'
#' @examples rates <- c(-0.00316,-0.00269,-0.00203,-0.00122,-0.00022,0.00092,0.00215,0.00342,0.00465,0.00581,0.00684,0.00777,0.00861,0.00933,0.00989,0.0103,0.01061,0.01092,0.01127,0.0117,0.01222,0.01281,0.01345,0.01411,0.01478,0.01546,0.01613,0.01679,0.01743,0.01806,0.01867,0.01926,0.01983,0.02038,0.02092,0.02143,0.02192,0.02239,0.02285,0.02329,0.02371,0.02411,0.0245,0.02488,0.02524,0.02558,0.02592,0.02624,0.02655,0.02685)
#' @examples curve <-curve(rates, "continuous")
#' @examples swaptions <- swaptions("lognormal", curve, 1, 1, 0.016735, 1)
#'
#' @export
swaptions <- function(type = "lognormal", curve, mat, tenor, vol, freq = 1){
  if(!(type %in% c("lognormal", "normal"))) stop("Invalid type choice")
  new(Class = "Swaptions",
      type = type,
      curve = curve,
      mat = mat,
      tenor = tenor,
      vol = vol,
      freq = freq
  )
}

############################################
############################################

# Plot method for class Swaptions
#' @importFrom graphics plot abline layout legend lines mtext par points title matplot
#' @import grDevices
#' @export
setMethod(
  f = "plot",
  signature = signature(x = "Swaptions", y = "missing"),
  definition = function(x, y, ...){
    # Construction of the plot surface
    mat <- min(x@mat):max(x@mat)
    ten <- min(x@tenor):max(x@tenor)
    z <- matrix(NA, nrow = length(mat), ncol = length(ten))
    for(i in 1:length(x@mat)){
      xpos <- which(mat == x@mat[i])
      ypos <- which(ten == x@tenor[i])
      z[xpos,ypos] <- x@vol[i]
    }
    z2 <- z
    for(i in 1:nrow(z)){
      for(j in 1:ncol(z)){
        if(is.na(z2[i,j])){
          z2[i,j] <- mean(z[max(i-1,1):min(i+1,nrow(z)),max(j-1,ncol(z)):min(j+1,ncol(z))], na.rm = TRUE)
        }
      }
    }
    # Plot of the surface
    pal <- c('#440154FF','#481568FF','#482677FF','#453781FF','#3F4788FF','#39558CFF','#32648EFF','#2D718EFF','#287D8EFF','#238A8DFF','#1F968BFF','#20A386FF','#29AF7FFF','#3CBC75FF','#56C667FF','#74D055FF','#94D840FF','#B8DE29FF','#DCE318FF','#FDE725FF')
    image(mat, ten, z2, useRaster = F, breaks = quantile(x@vol, seq(0,1, by = 0.05)), col = pal, xlab = "maturities", ylab = "tenors")
    for(i in 1:length(x@mat)){
      text(x@mat[i], x@tenor[i], round(x@vol[i],3))
    }
    #filled.contour(mat, ten, z2, xlim = c(min(mat), max(mat)), ylim = c(min(ten), max(ten)), xlab = "maturities", ylab="tenors", key.title = title(main = "volatility"))
    contour(mat, ten, z2, add = T, xlim = c(min(mat), max(mat)), ylim = c(min(ten), max(ten)), drawlabels = FALSE)
  }
)

############################################
############################################

# Print method for class Swaptions
#' @export
setMethod(
  f = "print",
  signature = "Swaptions",
  definition = function(x){
    cat("\n")
    cat("Type of volatility:", x@type, "\n")
    cat("Frequency of payments:", x@freq, "\n")
    cat("Parameters: \n")
    data.frame(maturities = x@mat,
               tenors = x@tenor,
               volatilities= x@vol,
               prices = x@price
    )
  }
)

############################################
############################################

# Show method for class Swaptions
#' @export
setMethod(
  f = "show",
  signature = "Swaptions",
  definition = function(object){
    cat("\n")
    cat("Type of volatility:", object@type, "\n")
    cat("Frequency of payments:", object@freq, "\n")
    cat("Number of swaptions:", length(object@mat), "\n")
    cat("Maturity range:", min(object@mat), "->", max(object@mat), "\n")
    cat("Tenor range:", min(object@tenor), "->", max(object@tenor), "\n")
  }
)
