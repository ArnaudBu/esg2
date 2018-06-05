#### Vasicek asset class for inflation ####
###########################################

#' Vasicek model
#'
#' An S4 class to represent a value modelled by a Vasicek projection model. In the context of this esg, this model is thought for inflation modelling.
#'
#' @slot q0 Numeric. Initial value of rate.
#' @slot a Numeric. Speed of reversion.
#' @slot b Numeric. Long-term mean for the model.
#' @slot sigma Numeric. Periodic volatility.
#' @slot q Matrix. Projection of the asset.
#'
#' @include utils.R
#' @exportClass VSK
setClass(Class = "VSK",
         representation = representation(
           q0 = "numeric",
           a = "numeric",
           b = "numeric",
           sigma = "numeric",
           q = "matrix"
         ),
         validity = check_vsk
)

############################################
############################################

# Initializator for class VSK
setMethod(
  f = "initialize",
  signature = "VSK",
  definition = function(.Object, q0, a, b, sigma, q){
    # Assignations
    .Object@q0 <- q0
    .Object@a <- a
    .Object@b <- b
    .Object@sigma <- sigma
    .Object@q <- q
    return(.Object)
  }
)

############################################
############################################

# User constructor function

#' VSK: constructor
#'
#' \code{vsk} creates a vasicek model object.
#'
#' @param q0 Numeric. Initial value of rate. Optional if index_histo or q_histo is given.
#' @param a Numeric. Speed of reversion. Optional if index_histo or q_histo is given.
#' @param b Numeric. Long-term mean for the model. Optional if index_histo or q_histo is given.
#' @param horizon Numeric. Projection horizon. Default to 50.
#' @param nsimul Numeric. Number of simulations. Default to 1000.
#' @param sigma Numeric. Periodic volatility. Optional if index_histo or q_histo is given.
#' @param index_histo Vector. History of the index to modelize. Optional if q0, a, b, sigma or q_histo is given. All values from the intervall must be given. Any missing value should be set to NA.
#' @param q_histo Vector. History of rates to modelize. Optional if q0, a, b, sigma or index_histo is given. All values from the intervall must be given. Any missing value should be set to NA.
#' @param W Matrix. Projection of the asset via a normal distribution. Optional.
#'
#' @return The model as a class.
#'
#' @details The rate at time t is computed as log(Index_t/Index_t-1)
#'
#' @examples index <- c(100,102.32,104.47,106.23,108.21,110.36,
#' 111.66,112.4,112.97,114.84,116.7,119.03,121.46,124.01,126.33,
#' 128.37,130.29,133.98,134.09,136.13,139.01,141.73,142.92,
#' 143.66,143.71,144,145.41)
#' @examples inflation <- vsk(index_histo = index)
#'
#' @importFrom stats approx dnorm integrate lm optim pnorm quantile rnorm sd uniroot
#' @export
vsk <- function(q0 = NULL, a = NULL, b = NULL, horizon = 50, nsimul = 1000, sigma = NULL, index_histo = NULL, q_histo = NULL,  W = NULL){
  # Validation
  if(is.null(W)) W <- matrix(rnorm(horizon * nsimul), nrow = nsimul)
  if(horizon > ncol(W)) warning(paste0("Impossible to go until horizon with W. Limiting to ", ncol(W)))
  if(nsimul > nrow(W)) warning(paste0("Impossible to have this number of simulation with W. Limiting to ", nrow(W)))
  horizon <- min(ncol(W), horizon)
  nsimul <- min(nrow(W), nsimul)
  W <- W[1:nsimul, 1:horizon]
  if((is.null(q0) | is.null(a) | is.null(b) | is.null(sigma)) & is.null(index_histo) & is.null(q_histo)) stop("Too many missing parameters for projection. We need either q0, a, b and sigma or index_histo or q_histo.")
  if(is.null(q0) | is.null(a) | is.null(b) | is.null(sigma)){
    if(is.null(q_histo) & !is.null(index_histo)) q_histo <- log(index_histo[-1]/index_histo[-length(index_histo)])
    if(is.null(q0)) q0 <- q_histo[length(q_histo)]
    if(is.null(a) | is.null(b) | is.null(sigma)){
      dflearn <- data.frame(x = q_histo[-length(q_histo)], y = q_histo[-1])
      mdl <- lm(y~x, data = dflearn)
      if(is.null(sigma)) sigma <- sigma(mdl)
      prms <- mdl$coefficients
      if(is.null(a)) a <- as.numeric(1 - prms[2])
      if(is.null(b)) b <- as.numeric(prms[1]/(1 - prms[2]))
    }
  }
  # Projection
  q = matrix(q0, nrow = nsimul, ncol = horizon + 1)
  for(i in 2:ncol(q)){
    q[, i] = q[, i-1] + a * ( b - q[, i-1]) + sigma * W[,i-1]
  }
  q <- q[,-1]
  colnames(q) <- 1:horizon
  # Creation of the object
  new(Class = "VSK",
      q0 = q0,
      a = a,
      b = b,
      sigma = sigma,
      q = q
  )
}

############################################
############################################

#' Draw method for VSK
#'
#' Plot the trajectories of the model.
#'
#' @param x the VSK object to plot
#' @param type Type of plot. "index" or "rate"
#' @import graphics grDevices
#' @export
setGeneric(
  name = "draw",
  def = function(x , type = "index")
  {
    standardGeneric("draw")
  }
)

setMethod(
  f = "draw",
  signature = signature(x = "VSK"),
  definition = function(x,  type = "index"){
    tr <- x@q
    tr <- cbind(x@q0, tr)
    nshow = 10
    if(type == "index"){
      tr <- exp(tr)
      tr[,1] <- 100
      tr <- t(apply(tr, 1, cumprod))
      nshow = 100
    }
    colnames(tr)[1] <- 0
    tr2 <- tr
    if(nrow(tr) > nshow){
      ord <- order(tr[, ncol(tr)])
      sel <- ord[round(seq(1, nrow(tr), length.out = nshow), 0)]
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

## Print method for class VSK

#' @describeIn VSK print method for VSK
#'
#' @param x the VSK object to print
#'
#' @export
setMethod(
  f = "print",
  signature = "VSK",
  definition = function(x){
    cat("\nInitial value: ", x@q0, '\n \n')
    cat("Volatility: ", x@sigma * 100, ' %\n \n')
    cat("Reversion speed: ", x@a, '\n \n')
    cat("Mean reversion: ", x@b * 100, ' %\n \n')
    cat("horizon: ", ncol(x@q), '\n \n')
    cat("number of simulation: ", nrow(x@q), '\n')
  }
)

############################################
############################################

## Show method for class VSK

#' @describeIn VSK show method for VSK
#' @param object the VSK object to show
#' @export
setMethod(
  f = "show",
  signature = "VSK",
  definition = function(object){
    cat("\nInitial value: ", object@q0, '\n \n')
    cat("Volatility: ", object@sigma * 100, ' %\n \n')
    cat("Reversion speed: ", object@a, '\n \n')
    cat("Mean reversion: ", object@b * 100, ' %\n \n')
    cat("horizon: ", ncol(object@q), '\n \n')
    cat("number of simulation: ", nrow(object@q), '\n')
  }
)

############################################
############################################

#' VSK: get trajectories
#'
#' \code{trajv} returns the trajectories for the Vasicek model.
#'
#' @param .Object VSK Object whose trajectories are wanted.
#' @param type character. Type of trajectories: "index" or "rate"
#'
#' @return The trajectories as a matrix.
#'
#' @examples index <- c(100,102.32,104.47,106.23,108.21,110.36,
#' 111.66,112.4,112.97,114.84,116.7,119.03,121.46,124.01,126.33,
#' 128.37,130.29,133.98,134.09,136.13,139.01,141.73,142.92,
#' 143.66,143.71,144,145.41)
#' @examples inflation <- vsk(index_histo = index)
#' @examples inf_traj <- trajv(inflation)
#'
#' @export
setGeneric(
  name="trajv",
  def = function(.Object, type = "index")
  {
    standardGeneric("trajv")
  }
)

setMethod(
  f="trajv",
  signature="VSK",
  definition=function(.Object, type = "index")
  {
    tr <- .Object@q
    if(type == "index"){
      tr <- exp(tr)
      tr <- t(apply(tr, 1, cumprod))
    } else{
      tr <- .Object@q
    }
    return(tr)
  }
)
