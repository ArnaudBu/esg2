# esg2

R package for risk neutral economic scenarios generation with a g2++ model

## Getting started

### Prerequisites

**esg2** only relies on *R* default libraries, but is not available on *CRAN*, which means the package `devtools` is required in order to install it directly from its GitHub repository.

```r
install.packages("devtools")
```
### Installation

The package is available from its GitHub repository.

```r
devtools::install_github("arnaudbu/esg2")
```

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

### Rate Model

<img src="https://github.com/arnaudbu/esg2/raw/master/img/projectiong2.png" width="50%"/>

<img src="https://github.com/arnaudbu/esg2/raw/master/img/test_deflator.png" width="50%"/>

### Black & Scholes Model

<img src="https://github.com/arnaudbu/esg2/raw/master/img/action_traj.png" width="50%"/>

<img src="https://github.com/arnaudbu/esg2/raw/master/img/mart.png" width="50%"/>

## Authors

* **Arnaud Buzzi** - *esg2 package* - [View on GitHub](https://github.com/ArnaudBu)

## Acknowledgement

* A big thank to **Hamza Ghrib** who is the initiator of this project.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
