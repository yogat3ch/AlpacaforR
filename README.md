# AlpacaforR
Connecting to [Alpacas](https://alpaca.markets) API and navigating it using R.

## Installation

**AlpacaforR** isn't yet available on CRAN, but you may install the development versions of the package from Github.

Install the [devtools](https://cran.r-project.org/web/packages/devtools/readme/README.html) package if you have yet to do so, and then load it in:

```r
install.packages("devtools")
library(devtools)
```

Now, install **AlpacaforR** using devtools `install_github` and then load it in:

```r
install_github("jagg19/AlpacaforR")
library(AlpacaforR)
```




## URL & User Keys

#### Live or Paper URL?
The user does not have to specify the URL, as the functions handle that on its own. The user only needs to specify whether or not they are interacting with a paper account. E.g:

```r
#If live account; user does not need to do anything since paper = FALSE is the default.
get_account()

#If paper account; user needs to set paper = TRUE
get_account(paper = TRUE)
```

Not all functions require this since some functions use the same URL regardless of the account type. These functions are `get_assets`, `get_calendar`, `get_clock`, and `get_bars` since the same URL is used for each user.

<br> 

#### KEY-ID and SECRET-KEY

You must set your KEY-ID and SECRET-KEY as specifically named environment variables. They *must* be named as the following:
```r
Sys.setenv('APCA-API-KEY-ID' = VALUE)
Sys.setenv('APCA-API-SECRET-KEY' = VALUE)
```

You can test that it has been properly set by calling:
```r
Sys.getenv('APCA-API-KEY-ID')
Sys.getenv('APCA-API-SECRET-KEY')
```

The output should be the key values you've entered. 

## Start trading in R!
You're all set! Now your ready to begin using **AlpacaforR** functions to send and recieve [Alpaca](https://alpaca.markets) API requests using R! See the help calls for details on each function. E.g `?get_account` 