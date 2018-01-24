# esg2

R package for risk neutral economic scenarios generation with a g2++ model

## Getting started

### Prerequisites

**esg2** only relies on *R* default libraries, but is not available on *CRAN*, which means the package `devtools` is required in order to install it directly from its GitHub repository.

```R
install.packages("devtools")
```

### Installation

The package is available from its GitHub repository.


```R
devtools::install_github("arnaudbu/esg2")
```

## Documentation

The script file [test_esg2.R](https://github.com/arnaudbu/esg2/raw/master/test_esg2.R) contains all the usefull commands in order to run an end to end example with this package.

All the functions of the package are documented through *R*'s `help()` function.

### Zero Coupon Curve

![Zero Coupon Rates](https://github.com/arnaudbu/esg2/raw/master/img/curve.png "Zero Coupon Rates")

### Swaptions

![Swaptions volatility surface](https://github.com/arnaudbu/esg2/raw/master/img/swaptions.png "Swaptions volatility surface")

### Handling correlations

### Rate Model

![Rate model projections](https://github.com/arnaudbu/esg2/raw/master/img/projectiong2.png "Rate model projections")

![Rate model deflator test](https://github.com/arnaudbu/esg2/raw/master/img/test_deflator.png "Rate model deflator test")

### Black & Scholes Model

![B&S model projections](https://github.com/arnaudbu/esg2/raw/master/img/action_traj.png "B&S model projections")

![B&S model martingality test](https://github.com/arnaudbu/esg2/raw/master/img/mart.png "B&S model martingality test")

## Authors

* **Arnaud Buzzi** - *esg2 package* - [View on GitHub](https://github.com/ArnaudBu)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
