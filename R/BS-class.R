#### Black & Scholes asset class ####
#####################################

#' Black & Scholes
#'
#' An S4 class to represent a Black & Scholes assets built upon G2 class
#'
#' @slot g2model G2. Trained and projected G2 model
#' @slot s0 Numeric. Initial value of the asset.
#' @slot vol Numeric. Periodic volatility.
#' @slot div Numeric. Percentage of divividend each year.
#' @slot rho Numeric. Correlation with the rate model.
#' @slot s Matrix. Projection of the asset.
#'
#' @include utils.R ZCCurve-class.R G2-class.R
#' @exportClass BS
setClass(Class = "BS",
         representation = representation(
           g2model = "G2",
           s0 = "numeric",
           vol = "numeric",
           div = "numeric",
           rho = "numeric",
           s = "matrix"
         ),
         validity = check_bs
)

############################################
############################################

# Initializator for class BS
setMethod(
  f = "initialize",
  signature = "BS",
  definition = function(.Object, g2model, s0, vol, div, rho, W){
    # Assignations
    .Object@g2model <- g2model
    .Object@s0 <- s0
    .Object@vol <- vol
    .Object@div <- div
    .Object@rho <- rho
    # Computation of W if not exist
    if(W[1] == "auto"){
      W_nc <- matrix(rnorm(g2model@horizon * g2model@nsimul), nrow = g2model@nsimul)
      W <- g2model@Wx * rho + sqrt((1-rho^2)) * W_nc
    }
    # Projection
    s = matrix(s0, nrow = g2model@nsimul, ncol = g2model@horizon + 1)
    for(i in 2:ncol(s)){
      s[, i] = s[, i-1]*exp(g2model@r[, i-1] - div - vol^2/2 + vol * W[,i-1])
    }
    s <- s[,-1]
    colnames(s) <- 1:g2model@horizon
    .Object@s <- s
    # Return object
    validObject(.Object)
    return(.Object)
  }
)

############################################
############################################

# User constructor function

#' BS: constructor
#'
#' \code{bs} creates a Black & Scholes model object.
#'
#' @param g2model G2. Trained and projected G2 model
#' @param s0 Numeric. Initial value of the asset. Default to 1.
#' @param vol Numeric. Periodic volatility. Default to 0.2.
#' @param div Numeric. Percentage of divividend each year. Default to 0.02.
#' @param rho Numeric. Correlation with the rate model. Default to -0.5. Useless if W is given.
#' @param W Matrix. Projection of the asset via a normal distribution. Optional.
#'
#' @return The model as a class.
#'
#' @examples rates <- c(-0.00316,-0.00269,-0.00203,-0.00122,-0.00022,
#' 0.00092,0.00215,0.00342,0.00465,0.00581,0.00684,0.00777,0.00861,
#' 0.00933,0.00989,0.0103,0.01061,0.01092,0.01127,0.0117,0.01222,
#' 0.01281,0.01345,0.01411,0.01478,0.01546,0.01613,0.01679,0.01743,
#' 0.01806,0.01867,0.01926,0.01983,0.02038,0.02092,0.02143,0.02192,
#' 0.02239,0.02285,0.02329,0.02371,0.02411,0.0245,0.02488,0.02524,
#' 0.02558,0.02592,0.02624,0.02655,0.02685)
#' @examples curve <-curvezc(rates, "continuous")
#' @examples g2model <- g2(curve, a=0.773511777, b=0.082013014,
#' sigma=0.022284644, eta=0.010382461, rho=-0.701985206)
#' @examples g2model <- project(g2model)
#' @examples action <- bs(g2model, 1, 0.2, 0.02, -0.5)
#'
#' @export
bs <- function(g2model, s0 = 1, vol = 0.2, div = 0.02, rho = -0.5, W = "auto"){
  new(Class = "BS",
      g2model = g2model,
      s0 = s0,
      vol = vol,
      div = div,
      rho = rho,
      W = W
  )
}

############################################
############################################

## Plot method

#' @describeIn BS plot method for BS
#' @param x the BS object to plot
#' @param y classical plot parameters. Useless here.
#' @import graphics grDevices
#' @export
setMethod(
  f = "plot",
  signature = signature(x = "BS", y = "missing"),
  definition = function(x, y){
    tr <- x@s
    tr <- cbind(x@s0, tr)
    colnames(tr)[1] <- 0
    tr2 <- tr
    if(nrow(tr) > 100){
      ord <- order(tr[, ncol(tr)])
      sel <- ord[round(seq(1, nrow(tr), length.out = 100), 0)]
      tr2 <- tr2[sel,]
    }
    matplot(colnames(tr),t(tr2), type = "l", xlab = "time", ylab = "Value", ylim = c(quantile(tr,0.05), quantile(tr, 0.95)))
    title("Trajectories with mean \n and quantile at 10 and 90%")
    lines(colnames(tr),colMeans(tr), lwd = 3, col = rgb(99/255, 24/255, 66/255))
    lines(colnames(tr),apply(tr, 2, function(x) quantile(x, 0.1)), lwd = 3, col = "#1de9b6")
    lines(colnames(tr),apply(tr, 2, function(x) quantile(x, 0.9)), lwd = 3, col = "#1de9b6")
  }
)

############################################
############################################

## Print method for class BS

#' @describeIn BS print method for BS
#'
#' @export
setMethod(
  f = "print",
  signature = "BS",
  definition = function(x){
    cat("\nInitial value: ", x@s0, '\n \n')
    cat("Volatility: ", x@vol, '\n \n')
    cat("Correlation with rate: ", x@rho, '\n \n')
    cat("Dividends: ", x@div, '\n \n')
    cat("horizon: ", x@g2model@horizon, '\n \n')
    cat("number of simulation: ", x@g2model@nsimul, '\n')
  }
)

############################################
############################################

## Show method for class BS

#' @describeIn BS show method for BS
#' @param object the BS object to show
#' @export
setMethod(
  f = "show",
  signature = "BS",
  definition = function(object){
    cat("\nInitial value: ", object@s0, '\n \n')
    cat("Volatility: ", object@vol, '\n \n')
    cat("Correlation with rate: ", object@rho, '\n \n')
    cat("Dividends: ", object@div, '\n \n')
    cat("horizon: ", object@g2model@horizon, '\n \n')
    cat("number of simulation: ", object@g2model@nsimul, '\n')
  }
)

############################################
############################################

#' BS: get trajectories
#'
#' \code{traj} returns the trajectories for the Black & Sholes model.
#'
#' @param .Object BS Object whose trajectories are wanted.
#'
#' @return The trajectories as a matrix.
#'
#' @examples rates <- c(-0.00316,-0.00269,-0.00203,-0.00122,
#' -0.00022,0.00092,0.00215,0.00342,0.00465,0.00581,0.00684,
#' 0.00777,0.00861,0.00933,0.00989,0.0103,0.01061,0.01092,
#' 0.01127,0.0117,0.01222,0.01281,0.01345,0.01411,0.01478,
#' 0.01546,0.01613,0.01679,0.01743,0.01806,0.01867,0.01926,
#' 0.01983,0.02038,0.02092,0.02143,0.02192,0.02239,0.02285,
#' 0.02329,0.02371,0.02411,0.0245,0.02488,0.02524,0.02558,
#' 0.02592,0.02624,0.02655,0.02685)
#' @examples curve <-curvezc(rates, "continuous")
#' @examples g2model <- g2(curve, a=0.773511777, b=0.082013014,
#' sigma=0.022284644, eta=0.010382461, rho=-0.701985206)
#' @examples g2model <- project(g2model)
#' @examples action <- bs(g2model, 1, 0.2, 0.02, -0.5)
#' @examples trajAction <- traj(action)
#'
#' @export
setGeneric(
  name="traj",
  def = function(.Object)
  {
    standardGeneric("traj")
  }
)

setMethod(
  f="traj",
  signature="BS",
  definition=function(.Object)
  {
    return(.Object@s)
  }
)

############################################
############################################

#' BS: Martingal test
#'
#' \code{test_martingal} tests the martingality of the projections.
#'
#' @param .Object BS Object on which the test is to be applied.
#'
#' @examples rates <- c(-0.00316,-0.00269,-0.00203,-0.00122,-0.00022,
#' 0.00092,0.00215,0.00342,0.00465,0.00581,0.00684,0.00777,0.00861,
#' 0.00933,0.00989,0.0103,0.01061,0.01092,0.01127,0.0117,0.01222,
#' 0.01281,0.01345,0.01411,0.01478,0.01546,0.01613,0.01679,0.01743,
#' 0.01806,0.01867,0.01926,0.01983,0.02038,0.02092,0.02143,0.02192,
#' 0.02239,0.02285,0.02329,0.02371,0.02411,0.0245,0.02488,0.02524,
#' 0.02558,0.02592,0.02624,0.02655,0.02685)
#' @examples curve <-curvezc(rates, "continuous")
#' @examples g2model <- g2(curve, a=0.773511777, b=0.082013014,
#' sigma=0.022284644, eta=0.010382461, rho=-0.701985206)
#' @examples g2model <- project(g2model)
#' @examples action <- bs(g2model, 1, 0.2, 0.02, -0.5)
#' @examples test_martingal(action)
#'
#' @export
setGeneric(
  name="test_martingal",
  def = function(.Object)
  {
    standardGeneric("test_martingal")
  }
)

setMethod(
  f="test_martingal",
  signature="BS",
  definition=function(.Object)
  {
    d <- deflator(.Object@g2model)
    t <- .Object@s * d
    t_mean <- c(.Object@s0, colMeans(t)*exp(.Object@div* 1:.Object@g2model@horizon))
    t_sd <- c(0, apply(t,2,sd)*exp(.Object@div* 1:.Object@g2model@horizon))
    plot(0:.Object@g2model@horizon,t_mean, type = "l", lwd = 3, col = rgb(99/255, 24/255, 66/255), xlab = "time", ylab = "deflated value", ylim = c(min(.Object@s0 - 1.96 * t_sd / sqrt(.Object@g2model@nsimul)), max(.Object@s0 + 1.96 * t_sd / sqrt(.Object@g2model@nsimul))))
    lines(0:.Object@g2model@horizon,.Object@s0 + 1.96 * t_sd / sqrt(.Object@g2model@nsimul), lwd = 3, col = "#1de9b6")
    lines(0:.Object@g2model@horizon,.Object@s0 - 1.96 * t_sd / sqrt(.Object@g2model@nsimul), lwd = 3, col = "#1de9b6")
    title("Mean deflated trajectory with \n 95% confidence interval")
    cat("Differences in percent by time: \n")
    dif <- paste0(round(abs(t_mean - .Object@s0)/.Object@s0 *100, 2), "%")
    names(dif) <- 0:.Object@g2model@horizon
    print(dif)
  }
)
