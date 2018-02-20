#### Zero Coupon Curve class ####
#################################

## Class definition

#' Zero Coupon Curve
#'
#' A S4 class to represent a zero coupon curve.
#'
#' @slot method Character. Method for discounting the curve:
#' \itemize{
#'   \item{"continuous": continuous rate. Default value.}
#'   \item{"actuarial": actuarial rate.}
#'   \item{"libor": libor rate}.}
#' @slot rates Vector. List of rates from maturity 1 to horizon.
#' @slot zcp Vector. List of corresponding zero coupon bonds prices.
#' @slot ifr Vector. Instant forward rates at time 0.
#'
#' @include utils.R
#' @exportClass ZCCurve
setClass(Class = "ZCCurve",
         representation = representation(
           method = "character",
           rates = "vector",
           zcp = "vector",
           ifr = "vector"
         ),
         validity = check_zc
)

############################################
############################################

# Initializator for class ZCCurve
setMethod(
  f = "initialize",
  signature = "ZCCurve",
  definition = function(.Object, method, rates){
    .Object@method <- method
    .Object@rates <- rates
    .Object@zcp <- rate2zcp(1:length(rates), rates, method)
    .Object@ifr <- c(rates[1],zcp2rate(1, .Object@zcp[2:length(rates)]/.Object@zcp[1:(length(rates)-1)], method))
    return(.Object)
  }
)

############################################
############################################

# User constructor function

#' Zero Coupon Curve: constructor
#'
#' \code{curvezc} creates a zero coupon curve object.
#'
#' @param rates Vector. List of rates from maturity 1 to horizon continuously.
#' @param method Character. Computation method:
#' \itemize{
#'   \item{"continuous": continuous rate. Default value.}
#'   \item{"actuarial": actuarial rate.}
#'   \item{"libor": libor rate.}}
#'
#' @return The curve object.
#'
#' @details Rates are assumed to go from period time 1 to period time equal to the length of the vector. For example, for a rate curve equal to c(0.01, 0.015, 0.02), the period considered are c(1,2,3).
#'
#' @examples
#'  rates <- c(-0.00316,-0.00269,-0.00203,-0.00122,-0.00022,0.00092,0.00215,0.00342,0.00465,0.00581,0.00684,0.00777,0.00861,0.00933,0.00989,0.0103,0.01061,0.01092,0.01127,0.0117,0.01222,0.01281,0.01345,0.01411,0.01478,0.01546,0.01613,0.01679,0.01743,0.01806,0.01867,0.01926,0.01983,0.02038,0.02092,0.02143,0.02192,0.02239,0.02285,0.02329,0.02371,0.02411,0.0245,0.02488,0.02524,0.02558,0.02592,0.02624,0.02655,0.02685)
#'  curve <-curvezc(rates, "continuous")
#'
#' @export
curvezc <- function(rates, method){
  if(!(method %in% c("continuous", "actuarial", "libor"))) stop("Invalid method choice")
  new(Class = "ZCCurve", rates = rates, method = method)
}

############################################
############################################

## Plot method

#' @describeIn ZCCurve plot method for ZCCurve
#' @param x the Zero Coupon Curve object to plot
#' @param y classical plot parameters. Useless here.
#' @import graphics grDevices
#' @export
setMethod(
  f = "plot",
  signature = signature(x = "ZCCurve", y = "missing"),
  definition = function(x, y){
    plot(x@rates * 100, xlab = "maturities", ylab = "rates in %")
    lines(x@rates * 100)
    title("Zero Coupon Curve")
  }
)

############################################
############################################

## Print method

#' @describeIn ZCCurve print method for ZCCurve
#' @export
setMethod(
  f = "print",
  signature = "ZCCurve",
  definition = function(x){
    cat("\n")
    cat("Type of curve:", x@method, "\n")
    cat("Parameters: \n")
    data.frame(maturities = 1:length(x@rates),
               rates = x@rates,
               `zero coupon prices` = x@zcp,
               `instant forward rate` = x@ifr)
  }
)

############################################
############################################

#' @describeIn ZCCurve show method for ZCCurve
#' @param object the Zero Coupon Curve object to show
#' @export
setMethod(
  f = "show",
  signature = "ZCCurve",
  definition = function(object){
    cat("\n")
    cat("Type of curve:", object@method, "\n\n")
    cat("Horizon:", length(object@rates), "\n\n")
    cat("rates:", object@rates)
  }
)
