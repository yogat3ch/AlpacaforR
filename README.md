# AlpacaforR
Connecting to [Alpacas](https://alpaca.markets) API and navigating it using R.

## Installation

**AlpacaforR** isn't yet available on CRAN, but you may install the development versions of the package from Github.

Install the [devtools](https://cran.r-project.org/web/packages/devtools/readme/README.html) package if you have yet to do so, and then load it in.

```r
install.packages("devtools")
library(devtools)
```

Now, install **AlpacaforR** using devtools `install_github` and then load it in.

```r
install_github("jagg19/AlpacaforR")
library(AlpacaforR)
```

You're all set! See the help calls for details on each function. E.g `?get_account`