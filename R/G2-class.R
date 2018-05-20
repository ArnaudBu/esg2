#### G2 Class ####
##################


#' G2 Model
#'
#' An S4 class to represent a G2 ++ model.
#'
#' @slot curve ZCCurve. Zero coupon curve as basis for projection.
#' @slot a Numeric. G2++ parameter.
#' @slot b Numeric. G2++ parameter.
#' @slot sigma Numeric. G2++ parameter.
#' @slot eta Numeric. G2++ parameter.
#' @slot rho Numeric. G2++ parameter.
#' @slot horizon Numeric. Projection horizon.
#' @slot nsimul Numeric. Number of simulations.
#' @slot x Matrix Projection of the first component.
#' @slot y Matrix. Projection of the second component.
#' @slot r Matrix. Short rate projection.
#' @slot Wx Matrix. Generated normal distribution for x projection.
#'
#' @include utils.R ZCCurve-class.R Swaptions-class.R
#' @exportClass G2
setClass(Class = "G2",
         representation = representation(
           curve = "ZCCurve",
           a = "numeric",
           b = "numeric",
           sigma = "numeric",
           eta = "numeric",
           rho = "numeric",
           horizon = "numeric",
           nsimul = "numeric",
           x = "matrix",
           y = "matrix",
           r = "matrix",
           Wx = "matrix"
         ),
         validity = check_g2
)

############################################
############################################

## Initializator for class G2

setMethod(
  f = "initialize",
  signature = "G2",
  definition = function(.Object, curve, a, b, sigma, eta, rho, horizon, nsimul){
    # Assignations
    .Object@curve <- curve
    .Object@a <- a
    .Object@b <- b
    .Object@sigma <- sigma
    .Object@eta <- eta
    .Object@rho <- rho
    .Object@horizon <- horizon
    .Object@nsimul <- nsimul
    # Return object
    validObject(.Object)
    return(.Object)
  }
)

############################################
############################################

## User constructor function

#' G2 ++ model: constructor
#'
#' \code{g2} creates a g2++ model object.
#'
#' @param curve ZCCurve. Zero coupon curve as basis for projection.
#' @param horizon Numeric. Projection horizon. Default to 50.
#' @param nsimul Numeric. Number of simulation. Default to 1000.
#' @param a Numeric. G2++ parameter. Optional.
#' @param b Numeric. G2++ parameter. Optional.
#' @param sigma Numeric. G2++ parameter. Optional.
#' @param eta Numeric. G2++ parameter. Optional.
#' @param rho Numeric. G2++ parameter. Optional.
#'
#' @return The model as a class
#'
#' @examples rates <- c(-0.00316,-0.00269,-0.00203,-0.00122,
#' -0.00022,0.00092,0.00215,0.00342,0.00465,0.00581,
#' 0.00684,0.00777,0.00861,0.00933,0.00989,0.0103,
#' 0.01061,0.01092,0.01127,0.0117,0.01222,0.01281,
#' 0.01345,0.01411,0.01478,0.01546,0.01613,0.01679,
#' 0.01743,0.01806,0.01867,0.01926,0.01983,0.02038,
#' 0.02092,0.02143,0.02192,0.02239,0.02285,0.02329,
#' 0.02371,0.02411,0.0245,0.02488,0.02524,0.02558,
#' 0.02592,0.02624,0.02655,0.02685)
#' @examples curve <-curvezc(rates, "continuous")
#' @examples g2model <- g2(curve, a=0.773511777, b=0.082013014,
#' sigma=0.022284644, eta=0.010382461, rho=-0.701985206)
#'
#' @export
g2 <- function(curve, horizon = 50, nsimul = 1000, a = numeric(0), b = numeric(0), sigma = numeric(0), eta = numeric(0), rho = numeric(0)){
  new(Class = "G2",
      curve = curve,
      a = a,
      b = b,
      sigma = sigma,
      eta = eta,
      rho = rho,
      horizon = horizon,
      nsimul = nsimul
  )
}

############################################
############################################

## Plot method for class G2

#' @describeIn G2 plot method for G2
#' @param x the G2 object to plot
#' @param y classical plot parameters. Useless here.
#' @import graphics grDevices
#' @export
setMethod(
  f = "plot",
  signature = signature(x = "G2", y = "missing"),
  definition = function(x, y){
    tr <- x@r
    tr2 <- tr
    if(nrow(tr) > 100){
      ord <- order(tr[, ncol(tr)])
      sel <- ord[round(seq(1, nrow(tr), length.out = 100), 0)]
      tr2 <- tr2[sel,]
    }
    matplot(colnames(tr),t(tr2), type = "l", xlab = "time", ylab = "Short Rate")
    lines(colnames(tr),colMeans(tr), lwd = 3, col = rgb(99/255, 24/255, 66/255))
    lines(colnames(tr),colMeans(tr) + apply(tr, 2, sd), lwd = 3, col = "#1de9b6")
    lines(colnames(tr),colMeans(tr) - apply(tr, 2, sd), lwd = 3, col = "#1de9b6")
  }
)

############################################
############################################

## Print method for class G2

#' @describeIn G2 print method for G2
#' @export
setMethod(
  f = "print",
  signature = "G2",
  definition = function(x){
    cat("\na: ", x@a, '\n \n')
    cat("b: ", x@b, '\n \n')
    cat("sigma: ", x@sigma, '\n \n')
    cat("eta: ", x@eta, '\n \n')
    cat("rho: ", x@rho, '\n \n')
    cat("horizon: ", x@horizon, '\n \n')
    cat("number of simulation: ", x@nsimul, '\n')
  }
)

############################################
############################################

## Show method for class G2

#' @describeIn G2 show method for G2
#' @param object the G2 object to show
#' @export
setMethod(
  f = "show",
  signature = "G2",
  definition = function(object){
    cat("\na: ", object@a, '\n \n')
    cat("b: ", object@b, '\n \n')
    cat("sigma: ", object@sigma, '\n \n')
    cat("eta: ", object@eta, '\n \n')
    cat("rho: ", object@rho, '\n \n')
    cat("horizon: ", object@horizon, '\n \n')
    cat("number of simulation: ", object@nsimul, '\n')
  }
)

############################################
############################################

## Projection function

#' G2 ++ model: projection
#'
#' \code{project} projects the rate model for the object G2.
#'
#' @param .Object G2 object. The object whose model is to be projected.
#' @param Wx Matrix. Matrix of dimension Number of simulations x Horizon representing the normal distribution for projection of the x component.
#' @param Wy Matrix. Matrix of dimension Number of simulations x Horizon representing the normal distribution for projection of the y component. Correlation rho with Wx.
#'
#' @return The updated object.
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
#'
#' @export
setGeneric(
  name="project",
  def = function(.Object, Wx = "auto", Wy = "auto")
  {
    standardGeneric("project")
  }
)

setMethod(
  f="project",
  signature="G2",
  definition=function(.Object, Wx = "auto", Wy = "auto")
  {
    # Extraction of useful parameters
    a = .Object@a
    b = .Object@b
    sigma = .Object@sigma
    eta = .Object@eta
    rho = .Object@rho
    # Construction of the normal laws if they do not exist
    if(Wx[1] == "auto" || Wy[1] == "auto"){
      correl <- cbind(c(1,rho), c(rho, 1))
      V_norm=rnorm(.Object@nsimul * .Object@horizon * 2,mean=0,sd=1)
      V_norm=array(V_norm,dim=c(.Object@nsimul , .Object@horizon, 2))
      R=chol(correl)
      V=array(0,dim=c(.Object@nsimul, .Object@horizon , 2))
      for(i in 1:.Object@nsimul)
      {
        V[i,,]=V_norm[i,,] %*% R
      }
      Wx = V[,,1]
      Wy = V[,,2]
    }
    # More parameters for projection
    NP = ncol(Wx)
    NS = nrow(Wx)
    zcp = .Object@curve@zcp
    f0 = .Object@curve@ifr
    #Initialization x(0)=0 y(0)=0 r(0)=r0
    x=array(0,dim=c(NS,NP))
    y=array(0,dim=c(NS,NP))
    r=array(0,dim=c(NS,NP))
    colnames(x) <- 0:(NP-1)
    colnames(y) <- 0:(NP-1)
    colnames(r) <- 0:(NP-1)
    r[,1] <- f0[1]
    #Iterations
    for(i in 2:(NP))
    {
      # Simulation of x factor
      x[,i] = next_step_g2(x[,i-1], a, sigma, Wx[,i-1])
      # Simulation of y factor
      y[,i] = next_step_g2(y[,i-1], b, eta, Wy[,i-1])
      # Short rate
      r[,i]=x[,i]+y[,i]+phi_g2(f0[i], a, b, sigma, eta, rho, i-1)
    }
    # Assignation in object
    .Object@x = x
    .Object@y = y
    .Object@r = r
    .Object@Wx = Wx
    # Return object
    return(.Object)
  }
)

############################################
############################################

## Zero coupon table

#' G2 ++ model: construction of a zero coupon prices table.
#'
#' \code{zctable} computes the zero coupon prices table at time t under the t-forward measure.
#'
#' @param .Object G2 object.
#' @param t Numeric. Time for computation.
#' @param length Numeric. Length of the curve, in time steps. If not given, the curve is given until the horizon of projection.
#'
#' @return The zero-coupon table at time t.
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
#' @examples zc10 <- zctable(g2model, 10)
#'
#' @export
setGeneric(
  name="zctable",
  def = function(.Object, t = 0, length = NULL)
  {
    standardGeneric("zctable")
  }
)

setMethod(
  f="zctable",
  signature="G2",
  definition=function(.Object, t = 0, length = NULL)
  {
    # Definition of the objects useful to computation
    x = .Object@x
    y = .Object@y
    if(is.null(length)){
      NP = .Object@horizon
    } else {
      NP = t + length
    }
    NS = .Object@nsimul
    a =  .Object@a
    b = .Object@b
    sigma = .Object@sigma
    eta = .Object@eta
    rho = .Object@rho
    zcp = .Object@curve@zcp
    p=array(1,dim=c(NS,NP+1 - t))
    colnames(p) <- t:NP
    #Iterations
    for(i in (t+1):NP){
      if(t == 0){
        p[, i+1-t] <- zcp[i] * exp(A_g2(a, b, sigma, eta, rho, t, i+2, x[,1], y[,1]))
      } else{
        p[, i+1-t] <- zcp[i]/zcp[t] * exp(A_g2(a, b, sigma, eta, rho, t, i, x[,t+1], y[,t+1]))
      }
    }
    # Return
    return(p)
  }
)

############################################
############################################

## Deflator table

#' G2 ++ model: deflator
#'
#' \code{deflator} computes the deflator table.
#'
#' @param .Object G2 object.
#'
#' @return The deflator table.
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
#' @examples def <- deflator(g2model)
#'
#' @export
setGeneric(
  name="deflator",
  def = function(.Object)
  {
    standardGeneric("deflator")
  }
)

setMethod(
  f="deflator",
  signature="G2",
  definition=function(.Object)
  {
    r = .Object@r
    p <- exp(-t(apply(r, 1, cumsum)))
    colnames(p) <- 1:.Object@horizon
    return(p)
  }
)

############################################
############################################

## Deflator test

#' Deflator test
#'
#' \code{test_deflator} compares the initial zero coupon rate curve with the one computed from the deflator.
#'
#' @param .Object G2 object. The object whose model is to be projected.
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
#' @examples zc10 <- test_deflator(g2model)
#'
#' @export
setGeneric(
  name="test_deflator",
  def = function(.Object)
  {
    standardGeneric("test_deflator")
  }
)

setMethod(
  f="test_deflator",
  signature="G2",
  definition=function(.Object)
  {
    def_table <- colMeans(deflator(.Object))
    r_table <- zcp2rate(1:length(def_table), def_table, method = .Object@curve@method)
    plot(r_table, type = "l", lwd = 3, col = rgb(99/255, 24/255, 66/255), xlab = "maturity", ylab = "rate")
    lines(.Object@curve@rates, lwd = 3, col = "#1de9b6")
    cat("Differences in percent by maturity: \n")
    dif <- paste0(round(abs(.Object@curve@rates[1:.Object@horizon] -r_table) *100, 2), "%")
    names(dif) <- 1:length(r_table)
    print(dif)
  }
)

############################################
############################################

## Compute swaption prices

#' Swaptions prices
#'
#' \code{price_swaption} prices a swaption according to the g2++ model.
#'
#' @param .Object G2 object. The object whose model is to be projected.
#' @param freqS Numeric. Swaption frequence, as the number of payments per period.
#' @param matS Numeric. Maturity of the swaption.
#' @param tenorS Numeric. Tenor of the swaption.
#'
#' @return The price of the swaption
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
#' sigma=0.022284644, eta =0.010382461, rho =-0.701985206)
#' @examples g2model <- project(g2model)
#' @examples price_swaption <- price_swaption(g2model, 2, 3, 3)
#'
#' @export
setGeneric(
  name="price_swaption",
  def = function(.Object, freqS, matS, tenorS)
  {
    standardGeneric("price_swaption")
  }
)

setMethod(
  f="price_swaption",
  signature="G2",
  definition=function(.Object, freqS, matS, tenorS)
  {
    # Define the relevant elements
    a =  .Object@a
    b = .Object@b
    sigma = .Object@sigma
    eta = .Object@eta
    rho = .Object@rho
    L <- 1
    mat <- 1:length(.Object@curve@zcp)
    zcp <- .Object@curve@zcp
    #Compute the price
    times_payments <- seq(matS + 1/freqS, matS + tenorS, by = 1 / freqS)
    amounts_payments <- approx(x = 1:length(zcp), y = zcp, xout = times_payments)$y
    payments <- 1/freqS * sum(amounts_payments)
    s0 <- (approx(x = 1:length(zcp), y = zcp, xout = matS)$y - approx(x = 1:length(zcp), y = zcp, xout = matS + tenorS)$y) / payments
    strike <- s0
    indices = seq(matS + 1 / freqS, matS + tenorS,by= 1 / freqS)
    B_a=(1-exp(-a*(indices-matS)))/a
    B_b=(1-exp(-b*(indices-matS)))/b
    mux=-M_g2(a,b,sigma,eta ,rho,matS)
    muy=-M_g2(b,a,eta,sigma,rho,matS)
    sigmax=sigma*sqrt((1-exp(-2*a*matS))/(2*a))
    sigmay=eta*sqrt((1-exp(-2*b*matS))/(2*b))
    rhoxy=rho*sigma*eta*(1-exp(-(a+b)*matS))/((a+b)*sigmax*sigmay)
    c=rep(strike * 1 / freqS, length(indices))
    c[length(c)]=c[length(c)]+1
    A = approx(x = mat, y = zcp, xout = indices)$y/approx(x = mat, y = zcp, xout = matS)$y * exp(A_g2(a, b, sigma, eta, rho, matS, indices, 0, 0))
    g=function(x){ # function to integrate
      y=uniroot(f=function(z){(sum(c*A*exp(-B_a*x-B_b*z))-1)},c(-100,100))$root
      khi=-B_b*(muy-0.5*(1-rhoxy^2)*sigmay^2*B_b+rhoxy*sigmay*(x-mux)/sigmax)
      lambda=c*A*exp(-B_a*x)
      h1=((y-muy)/(sigmay*sqrt(1-rhoxy^2))) - (rhoxy*(x-mux)/(sigmax*sqrt(1-rhoxy^2)))
      h2=h1+B_b*sigmay*sqrt(1-rhoxy^2)
      density_x=exp(-0.5*((x-mux)/sigmax)^2)/(sigmax*sqrt(2*pi))
      T1=pnorm(-h1)
      T2=sum(lambda*exp(khi)*pnorm(-h2))
      g=density_x * (T1-T2)
    }
    # Vector version of g
    g_vect=function(z){
      return(mapply(function(x){g(x)},x=z))
    }
    PrixExact=approx(x = mat, y = zcp, xout = matS)$y*integrate(g_vect,lower=mux-5*sigmax,upper=mux+5*sigmax)$value
    # Return price
    return(PrixExact)
  }
)

############################################
############################################

## Calibration function

#' Rate model calibration
#'
#' \code{calibrate} calibrates the model on swaptions prices.
#'
#' @param .Object G2 object. The object whose model is to be projected.
#' @param swaptions Swaptions. List of swaptions under the format of the class Swaption.
#' @param maxIter Numeric. Max number of iterations for Nelder-Mead algorithm. Default to 1000.
#' @param input_param Data.frame. Input parameters for optimization. It must have 4 columns of 5 rows each:
#' #' \itemize{
#'   \item{"Parameter": always equals to c("a", "b", "sigma", "eta", "rho").}
#'   \item{"Initial.point": Initial point for optimization. Default to c(0.1, 0.25, 0.05, 0.025, 0).}
#'   \item{"Min": Minimum admissible value for parameters. Default to c(0.001, 0.0001, 0.0001, 0.001, -1).}
#'   \item{"Max": Maximimu admissible value for parameters. Default to c(0.2, 0.5, 0.1, 0.05, 1).}}
#' @param monitor Boolean. To display intermediary results. Default to TRUE.
#'
#' @return The calibrated model.
#'
#' @export
setGeneric(
  name="calibrate",
  def = function(.Object, swaptions, maxIter = 1000, input_param = "auto", monitor = TRUE)
  {
    standardGeneric("calibrate")
  }
)

setMethod(
  f="calibrate",
  signature="G2",
  definition=function(.Object, swaptions, maxIter = 1000, input_param = "auto", monitor = TRUE)
  {
    # Parameters for optimisation
    if(all(input_param == "auto")){
    input_param <- data.frame(Parameter = c("a", "b", "sigma", "eta", "rho"),
                              Initial.point = c(0.1, 0.25, 0.05, 0.025, 0),
                              Min = c(0.001, 0.0001, 0.0001, 0.001, -1),
                              Max = c(0.2, 0.5, 0.1, 0.05, 1))
    }
    freq = swaptions@freq
    priceSwaptions <- data.frame(mat = swaptions@mat, tenor = swaptions@tenor, price = swaptions@price)
    f0 <- .Object@curve@ifr[10]
    nswap <- nrow(priceSwaptions)
    # Probability of negative rate at 10 years horizon
    ptn <- function(param){
      a = param[1]
      b = param[2]
      sigma = param[3]
      eta = param[4]
      rho = param[5]
      n <- f0 + 1/2*(sigma/a*(1-exp(-10*a)))^2 + 1/2*(eta/b*(1-exp(-10*b)))^2 + rho*sigma*eta/(a*b)*(1-exp(-10*a))*(1-exp(-10*b))
      d <- sigma^2/(2*a)*(1-exp(-2*10*a)) + eta^2/(2*b)*(1-exp(-2*10*b)) + 2*sigma*rho*eta/(a+b)*(1-exp(-10*(a+b)))
      return(pnorm(-n/sqrt(d)))
    }
    # Function to optimize
    OptimFunction=function(param){
      model <- .Object
      model@a <- param[1]
      model@b <- param[2]
      model@sigma <- param[3]
      model@eta <- param[4]
      model@rho <- param[5]
      if(param[1]<input_param[1,3]||param[2]<input_param[2,3]||param[3]<input_param[3,3]||param[4]<input_param[4,3]||param[5]<input_param[5,3]||param[1]>input_param[1,4]||param[2]>input_param[2,4]||param[3]>input_param[3,4]||param[4]>input_param[4,4]||param[5]>input_param[5,4])
      {
        ecartRelatif=100000
      } else{
        priceSwaptions$valueCalc <- tryCatch({mapply(function(x,y){price_swaption(model, freq, x, y)}, x = priceSwaptions$mat, y = priceSwaptions$tenor)}, error = function(e){1000})
        ecartRelatif <- sum(abs((priceSwaptions$valueCalc - priceSwaptions$price) / priceSwaptions$price), na.rm = TRUE) + nswap * 0.3 *ptn(param)
      }
      return(ecartRelatif)
    }
    # Optimization loop
    paramIni <- input_param$Initial.point
    param = optim(paramIni ,function(param){OptimFunction(param)},
                  method="Nelder-Mead",
                  control = list(maxit = maxIter, trace = ifelse(monitor,1,0)))
    param = optim(param$par ,function(param){OptimFunction(param)},
                  method="BFGS",
                  control = list(maxit = maxIter, trace = ifelse(monitor,1,0)))
    # Save of the results
    .Object@a <- param$par[1]
    .Object@b <- param$par[2]
    .Object@sigma <- param$par[3]
    .Object@eta <- param$par[4]
    .Object@rho <- param$par[5]
    cat("\nRelative error: ", param$value / nrow(priceSwaptions) * 100, "% \n\n")
    priceSwaptions$valueCalc <- tryCatch({mapply(function(x,y){price_swaption(.Object, freq, x, y)}, x = priceSwaptions$mat, y = priceSwaptions$tenor)}, error = function(e){1000})
    priceSwaptions$diff <- abs(priceSwaptions$price - priceSwaptions$valueCalc)
    priceSwaptions$relDiff <- paste0(round(priceSwaptions$diff / priceSwaptions$price * 100, 3), "%")
    print(priceSwaptions)
    return(.Object)
  }
)
