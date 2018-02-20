# esg2

R package for risk neutral economic scenarios generation with a g2++ model

## Getting started

### Prerequisites

**esg2** only relies on *R* default libraries. The package `devtools` is required in order to install it directly from its GitHub repository.

```r
install.packages("devtools")
```
### Installation

The package is available from its GitHub repository.

```r
devtools::install_github("arnaudbu/esg2")
```

## Model

The G2++ model implemented in this package is described by the following equation:

<a href="https://www.codecogs.com/eqnedit.php?latex=\left\{\begin{matrix}&space;dx(t)&space;=&space;-ax(t)&space;&plus;&space;\sigma&space;dW_1(t)&space;\\&space;dy(t)&space;=&space;-by(t)&space;&plus;&space;\eta&space;dW_2(t)&space;\\&space;\rho&space;dt&space;=&space;dW_1(t)dW_2(t)&space;\\&space;\\&space;r(t)&space;=&space;x(t)&space;&plus;&space;y(t)&space;&plus;&space;\phi(t)&space;\end{matrix}\right." target="_blank"><img src="https://latex.codecogs.com/gif.latex?\left\{\begin{matrix}&space;dx(t)&space;=&space;-ax(t)&space;&plus;&space;\sigma&space;dW_1(t)&space;\\&space;dy(t)&space;=&space;-by(t)&space;&plus;&space;\eta&space;dW_2(t)&space;\\&space;\rho&space;dt&space;=&space;dW_1(t)dW_2(t)&space;\\&space;\\&space;r(t)&space;=&space;x(t)&space;&plus;&space;y(t)&space;&plus;&space;\phi(t)&space;\end{matrix}\right." title="\left\{\begin{matrix} dx(t) = -ax(t) + \sigma dW_1(t) \\ dy(t) = -by(t) + \eta dW_2(t) \\ \rho dt = dW_1(t)dW_2(t) \\ \\ r(t) = x(t) + y(t) + \phi(t) \end{matrix}\right." /></a>

The Black & Scholes model implemented for related assets follows the classical model, with a time-dependant interest rate.

<a href="https://www.codecogs.com/eqnedit.php?latex=dS(t)&space;=&space;r(t)S(t)dt&space;&plus;&space;\omega&space;S(t)dW_s(t)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?dS(t)&space;=&space;r(t)S(t)dt&space;&plus;&space;\omega&space;S(t)dW_s(t)" title="dS(t) = r(t)S(t)dt + \omega S(t)dW_s(t)" /></a>

## Documentation

The script file [test_esg2.R](https://github.com/arnaudbu/esg2/raw/master/test_esg2.R) contains all the usefull commands in order to run an end to end example with this package.

All the functions of the package are documented through *R*'s `help()` function.

### Zero Coupon Curve

The Zero Cupon curve is the basic brick for rate projection.

#### Import the curve

A zero coupon rate curve is imported through the function `curvezc`, which create a *Zero Coupon Curve* object from the rates given in standard unit.

Three discounting methods are available:

+ continuous;
+ actuarial;
+ libor.

> Only rates are given in the function. Maturities are assumed to go continuously from 1 to the length of the curve.

For example, importing a 10 periods curve is done with the command:

```r
?curvezc

rates <- c(-0.00316,-0.00269,-0.00203,-0.00122,-0.00022, 0.00092,0.00215,0.00342,0.00465,0.00581)

curve <- curvezc(method = "continuous",
             rates = rates
             )
```

#### Operations

It is possible to display, print or plot a *curve* object.

```r
curve
print(curve)
plot(curve)
```
<img src="https://github.com/arnaudbu/esg2/raw/master/img/curve.png" width="50%"/>

The plot gives the zero coupon rate for each maturity.

### Swaptions

The rate model uses swaptions for its calibration. Those instrument are stocked as a table in an object of class Swaptions.

#### Import

The *swaptions* initiator needs a curve object for the pricing of the swaptions, along with a list of maturities, tenors, and volatilities.

Two pricing method are available:
+ normal: Bachelier pricing;
+ lognormal: Black & Scholes Pricing.

The pricing model also use the frequency as the number of payments for each period.

```r
?swaptions

maturities = c(2,3,4,5)
tenors = c(1,1,1,1)
vols = c(0.016735,0.009841,0.007156,0.005425)

swaptions <- swaptions("normal", curve, maturities, tenors, vols, 2)
```

#### Operations

It is possible to display, print or plot the volatility surface of a swaptions object. The `print` function also displays the computed prices.

```r
swaptions
print(swaptions)
plot(swaptions)
```

<img src="https://github.com/arnaudbu/esg2/raw/master/img/swaptions.png" width="50%"/>

### Handling correlations

Correlations needs to be handled between at least the two components of the rate model, and for associated assets using the projected rates in a Black & Scholes context.

We thus need to generate W_1 and W_2 processes with a rho correlation, and take into account the correlations between the other Wiener's process.

A function, `genW` is thus implemented in order to generate as many correlated processes as needed:

This function takes as argument the correlation matrix between the different process, the horizon for projection and the number of desired simulations.

> In order to get the calibrated parameters of the rate model, this function should be run after the calibration process.

```r
?genW

correl <- cbind(c(1,g2model@rho, 0.25),c(g2model@rho,1, 0), c(0.25, 0, 1))
W <- genW(correl, g2model@nsimul, g2model@horizon)

```

### Rate Model

#### Initialization

The rate model is initialized via a zero coupon curve, a projection horizon and a number of simulation.

It is possible to directly pass the parameters of the model as arguments in order to skip the calibration step.

```r
?g2

g2model <- g2(curve, horizon = 50, nsimul = 1000)
```

#### Calibration

Calibration is performed on the model over a panel of swaptions defined in a *Swaption* object.

The optimized function for the process is a trade off between the differences between theoretical and observed prices and the probability to get negative rates. The optimal parameters are found with a Nelder-Mead algorithm followed by a Newton-Rhaphson method.

```r
?calibrate

g2model <- calibrate(g2model, swaptions, maxIter = 100)
```

#### Projection and Visualization

Once calibrated, the model is projected via the function `project`.

It is possible to pass Wx and Wy as optional arguments, that corresponds to W_1 and W_2 in the model equation and are generated to be correlated between themselves and with other assets. If not given, those two processes are computed with the rho parameter of the model, without taking into account any external process.

```r
?project

g2model <- project(g2model, Wx = W[,,1], Wy = W[,,2])
```

It is possible to display, print, and plot the model. For the later case, 100 scenarios are displayed, along with the mean trajectory and the standard deviation around the mean.

```r
g2model
print(g2model)
plot(g2model)
```

<img src="https://github.com/arnaudbu/esg2/raw/master/img/projectiong2.png" width="50%"/>

Once the projection are realized, it is possible to get the deflator or the projected zero-coupon prices table at a time t.

```r
# Get deflator table
?deflator
def <- deflator(g2model)

# Get zero coupon table at time 10
?zctable
zc10 <- zctable(g2model, 10)
```

#### Validation

The deflator test is also implemented in the package. This test aims to verify that the means of the deflator values match the zero coupon curve for every maturity.

The function `test_deflator` plot the zero coupon curve against the mean of the deflator for each maturity, along with a table displaying the absolute differences in points. 

```r
?test_deflator
test_deflator(g2model)
```

<img src="https://github.com/arnaudbu/esg2/raw/master/img/test_deflator.png" width="50%"/>

### Black & Scholes Model

The library allows to use a Black & Scholes model for asset projection with the projected rate model.

#### Initialization

Initializing a Black & Scholes modelized asset requires:

+ a calibrated and projected rate model;
+ an initial value for the projection;
+ a volatility for the asset;
+ a dividend rate, as the proportion of the value of the asset that is paid each year as dividends or rent;
+ the correlation with the rate model;
+ the generated distribution for W_s.

The last value is optional. If not provided, a distribution is computed with the given correlation.

```r
?bs

action <- bs(g2model,
             s0 = 100,
             vol = 0.2,
             div = 0.02,
             rho = -0.5,
             W = W[,,3])
```

#### Projection and Visualization

All the traditional display functions are available, and the trajectories can be extracted with the function `trajAction`. 

```r
?traj
trajAction <- traj(action)

action
print(action)
plot(action)
```

<img src="https://github.com/arnaudbu/esg2/raw/master/img/action_traj.png" width="50%"/>

#### Validation

The martingality test is implemented.

The means of the discounted value of the asset is ploted with the 95% confidence interval of its values.

```r
?test_martingal
test_martingal(action)
```

<img src="https://github.com/arnaudbu/esg2/raw/master/img/mart.png" width="50%"/>

## Authors

* **Arnaud Buzzi** - *esg2 package* - [View on GitHub](https://github.com/ArnaudBu)

## License

This project is licensed under GPL Licensing.
