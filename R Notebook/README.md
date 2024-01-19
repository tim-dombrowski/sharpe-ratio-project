Sharpe Ratio Comparisons
================
Last updated: 2024-01-19

## R Packages

Below is a list of R packages that will be used throughout this R
Notebook.

- The [fredr package](https://cran.r-project.org/package=fredr) is an R
  package that wraps the FRED API for easy importing of FRED data into
  R.
- The [quantmod package](https://cran.r-project.org/package=quantmod)
  contains tools for importing and analyzing financial data.
- The [xts package](https://cran.r-project.org/package=xts) allows for
  some additional time series functionality.
- The [ggplot2 package](https://cran.r-project.org/package=ggplot2)
  includes tools for generating graphics and visuals.
- The [rmarkdown package](https://cran.r-project.org/package=rmarkdown)
  is used to generate this R Notebook.

The first three lines in the setup chunk below will automatically
install any R packages that you may be missing. Another observation to
make about the code chunk is that it is labeled as ‘setup’, which is a
special name that the R Notebook will recognize and automatically run
prior to running any other code chunk. This is useful for loading in
packages and setting up other global options that will be used
throughout the notebook.

``` r
list.of.packages = c("fredr",
                     "quantmod", 
                     "xts", 
                     "ggplot2", 
                     "rmarkdown")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(fredr)
library(quantmod)
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: TTR

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(xts)
library(ggplot2)
```

## FRED Data Import

To access the FRED API, you must first create an account and [request an
API key](https://fred.stlouisfed.org/docs/api/api_key.html). If you wish
to run the code and replicate the results, you’ll need to make an
account, generate your own API key, and run this command un-commented
with your key in place of the placeholder text.

``` r
#fredr_set_key("<YOUR-FRED-API-KEY>")
```

Using the `fredr()` function, we will import the 10-year Treasury note
yields. This is a typical proxy for the risk-free return when applying
CAPM and calculating stock betas. The Sys.Date function is simply using
the computer’s current time to get the most up-to-date data. *We could
also import BTC prices via FRED, but they only use data from one
exchange, whereas CoinMarketCap averages across many.*

``` r
RFraw = fredr(
  series_id = "DGS10",
  observation_start = as.Date("2011-04-30"),
  observation_end = as.Date(Sys.Date()),
  frequency = "m"
)
INFraw = fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2011-04-30"),
  observation_end = as.Date(Sys.Date()),
  frequency = "m"
)
```

We will then create a `xts` time series object, `ALL`, to collect all
the asset returns. This effectively imposes the directed nature of time
into the data frame’s properties and opens the door to additional
functions from the xts package. The second line of code renames the
Treasury yields as `rf`, and the last line removes the extra variables
that are no longer needed.

``` r
ALL = xts(RFraw,order.by=RFraw$date)
colnames(ALL)[colnames(ALL)=="value"] <- "rf"
ALL = subset(ALL,select=-c(date,series_id,realtime_start,realtime_end))
```

Before we attach the inflation rate to the `ALL` table, we must first
calculate it from the CPI levels that we downloaded. This will
effectively be the same process that we will follow to convert stock
prices to annualized returns.

First, we do the same three steps as above, which creates the `CPI`
variable in the `INF` data frame. Then we calculate the monthly log
returns (growth rates) by taking the successive difference between the
natural logarithms of the CPI. Lastly, we convert the monthly inflation
numbers to annualized percentages by multiplying by 12 and 100. *There
are some more details on this below in the stock return examples.*

``` r
INF = xts(INFraw,order.by=INFraw$date)
colnames(INF)[colnames(INF)=="value"] <- "CPI"
INF = subset(INF,select=-c(date,series_id,realtime_start,realtime_end))
INF$INFmonthly = log(as.numeric(INF$CPI)) - log(as.numeric(lag(INF$CPI)))
INF$inf = INF$INFmonthly*12*100
# Attach the annualized inflation numbers to the ALL data frame
ALL = merge(ALL,INF$inf)
```

## Yahoo Finance Data Import

The quantmod package contains tools for importing both stock and crypto
data from Yahoo! Finance. The `getSymbols()` function is used to import
the data. The first argument is a vector of ticker symbols, and the
`src` argument specifies the data source. The `from` and `to` arguments
specify the date range for the data. Then the `periodicity="monthly"`
requests a monthly series, rather than the default daily frequency.

``` r
tickers = c("SPY",
            "GOOG",
            "AAPL",
            "TSLA",
            "BTC-USD",
            "ETH-USD",
            "ADA-USD")
getSymbols(tickers,
           src="yahoo",
           from="2011-04-01",
           to=Sys.Date(),
           periodicity="monthly")
```

Rename the crypto data frames since the \$ indexing doesn’t work with
the hyphen in the name. Then covert the asset price series to annualized
returns and merge these return series to the `ALL` data frame.

``` r
BTC = `BTC-USD`
ETH = `ETH-USD`
ADA = `ADA-USD`
# Compute returns
SPY$Return = c(NA, diff(log(as.numeric(SPY$SPY.Adjusted))))
GOOG$Return = c(NA, diff(log(as.numeric(GOOG$GOOG.Adjusted))))
AAPL$Return = c(NA, diff(log(as.numeric(AAPL$AAPL.Adjusted))))
TSLA$Return = c(NA, diff(log(as.numeric(TSLA$TSLA.Adjusted))))
BTC$Return = c(NA, diff(log(as.numeric(BTC$`BTC-USD.Adjusted`))))
ETH$Return = c(NA, diff(log(as.numeric(ETH$`ETH-USD.Adjusted`))))
ADA$Return = c(NA, diff(log(as.numeric(ADA$`ADA-USD.Adjusted`))))
# Annualize returns
SPY$SPY = SPY$Return*12*100
GOOG$GOOG = GOOG$Return*12*100
AAPL$AAPL = AAPL$Return*12*100
TSLA$TSLA = TSLA$Return*12*100
BTC$BTC = BTC$Return*12*100
ETH$ETH = ETH$Return*12*100
ADA$ADA = ADA$Return*12*100
# Merge to ALL
ALL = merge(ALL, 
            SPY$SPY,
            GOOG$GOOG,
            AAPL$AAPL,
            TSLA$TSLA,
            BTC$`BTC`,
            ETH$`ETH`,
            ADA$`ADA`)
```

## Prepare Final Data

Now that we have all the annualized monthly returns for all the assets,
we will subset down to the 60 most recent full month observations. The
first line takes the `tail()` of the `ALL` data frame and counts how
many of the most recent observations are incomplete using
`complete.cases()`. Then the second line creates our `FINAL` data frame
where we trim off those extra, incomplete observations. Then the final
line trims off anything earlier than the most recent 60 months. Thus, we
end with a data frame with the 60 most recent monthly observations where
we have all data present (except for ADA, which doesn’t yet have a full
60 months of historical return data). This is the typical kind of data
for CAPM calculations; however, there are certainly many extensions and
expansions in the vast asset pricing literature.

``` r
ntrim = sum(!complete.cases(tail(ALL)))
FINAL = ALL[1:(nrow(ALL)-ntrim),]
FINAL = last(FINAL,60)
```

Then before we dive into analyzing our final dataset, let’s create a
couple portfolios between the S&P 500 and Bitcoin. For Portfolio 1,
we’ll do an equal-weight portfolio between the S&P 500 and Bitcoin. Then
Portfolio 2 will be 95% S&P 500 and 5% Bitcoin.

``` r
w = c(0.5,0.5)
FINAL$PORT5050 = w[1]*FINAL$SPY+w[2]*FINAL$BTC
w = c(0.95,0.05)
FINAL$PORT9505 = w[1]*FINAL$SPY+w[2]*FINAL$BTC
```

## Examine the Returns Series

### Visualize the Data

We will use the ggplot2 package for graphics. To start, let’s plot out a
bar chart for the least volatile return series, the risk-free rate
(10-year Treasury yield). For this plot, we load in our final data
frame, set the x input as `Index` (which is the index for our xts
object, *time*). Then we plot the risk-free return on the y-axis. The
`geom_col()` line indicates the type of plot, and the title is set with
`ggtitle()`. The `xlab()` function is used to remove the x-axis label
since it is not needed here.

``` r
ggplot(FINAL,aes(x=Index,y=rf))+
  geom_col()+
  xlab("")+
  ggtitle("Risk-Free Asset Returns (10-Year Treasury Yield)")
```

![](README_files/figure-gfm/rfplot-1.png)<!-- -->

Then we’ll generate the same chart for the inflation rates and asset
returns.

``` r
ggplot(FINAL,aes(x=Index,y=inf))+
  geom_col()+
  xlab("")+
  ggtitle("Annualized Inflation Rates")
```

![](README_files/figure-gfm/otherplots-1.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=SPY))+
  geom_col()+
  xlab("")+
  ggtitle("SPY Returns")
```

![](README_files/figure-gfm/otherplots-2.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=GOOG))+
  geom_col()+
  xlab("")+
  ggtitle("GOOG Returns")
```

![](README_files/figure-gfm/otherplots-3.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=AAPL))+
  geom_col()+
  xlab("")+
  ggtitle("AAPL Returns")
```

![](README_files/figure-gfm/otherplots-4.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=TSLA))+
  geom_col()+
  xlab("")+
  ggtitle("TSLA Returns")
```

![](README_files/figure-gfm/otherplots-5.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=BTC))+
  geom_col()+
  xlab("")+
  ggtitle("BTC Returns")
```

![](README_files/figure-gfm/otherplots-6.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=ETH))+
  geom_col()+
  xlab("")+
  ggtitle("ETH Returns")
```

![](README_files/figure-gfm/otherplots-7.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=ADA))+
  geom_col()+
  xlab("")+
  ggtitle("ADA Returns")
```

![](README_files/figure-gfm/otherplots-8.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=PORT5050))+
  geom_col()+
  xlab("")+
  ggtitle("Portfolio 1 Returns (50% S&P 500, 50% BTC)")
```

![](README_files/figure-gfm/otherplots-9.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=PORT9505))+
  geom_col()+
  xlab("")+
  ggtitle("Portfolio 2 Returns (95% S&P 500, 5% BTC)")
```

![](README_files/figure-gfm/otherplots-10.png)<!-- -->

### The Multivariate Return Distribution

Now let’s go beyond just visualizing the individual return series and
look at some statistics for the multivariate return distribution. If we
consider a multivariate normal distribution, we can think of the mean
vector as the expected returns for each asset, and the covariance matrix
as the variance of each asset’s returns along the diagonal, and the
covariances between each pair of assets in the off-diagonal elements.

The `colMeans()` function calculates the average of each column
(i.e. the average return for each asset).*Note: the `round()` function
is applied to reduce the number of decimals when displaying the
calculations in the output.*

``` r
Er = colMeans(FINAL,na.rm=TRUE)
Er |> round(digits=2)
```

    ##       rf      inf      SPY     GOOG     AAPL     TSLA      BTC      ETH 
    ##     2.28     4.01    14.53    20.02    32.53    48.32    48.48    56.79 
    ##      ADA PORT5050 PORT9505 
    ##    53.44    31.50    16.22

Now let’s calculate the volatility (standard deviation of returns) for
each of these assets. This is done by simply calculating the standard
deviation `sd()` of each return series (reduced to just one line of code
using the `apply()` function).

``` r
sigma = apply(FINAL,2,sd,na.rm=TRUE)
sigma |> round(digits=2)
```

    ##       rf      inf      SPY     GOOG     AAPL     TSLA      BTC      ETH 
    ##     1.18     3.94    65.38    93.36   101.82   241.47   236.54   295.17 
    ##      ADA PORT5050 PORT9505 
    ##   390.57   135.87    68.15

Then beyond the individual asset volatilities, another critical
component of portfolio-level risk is the correlations across the
individual assets. The starting point for this is the covariance matrix,
which we can compute using the `cov()` function. *Note: the
`use="pairwise.complete.obs"` parameter prevents missing values in the
event that any of the assets have less than 60 months of data available.
This is no longer necessary; however, that was not the case back when I
started this.*

``` r
cov(FINAL, use="pairwise.complete.obs") |> round(digits=0)
```

    ##           rf  inf   SPY  GOOG  AAPL  TSLA   BTC   ETH    ADA PORT5050 PORT9505
    ## rf         1    0    -5   -12   -21   -82   -28   -62    -75      -16       -6
    ## inf        0   15   -25   -22   -32   -47  -240  -223   -213     -133      -36
    ## SPY       -5  -25  4275  4604  5286  8846  6806  9646  10178     5540     4401
    ## GOOG     -12  -22  4604  8717  5606 11338  8015 11557  17337     6309     4774
    ## AAPL     -21  -32  5286  5606 10368 17192  7367 11165   7728     6327     5390
    ## TSLA     -82  -47  8846 11338 17192 58308 20693 34056  24325    14770     9439
    ## BTC      -28 -240  6806  8015  7367 20693 55952 53988  50521    31379     9263
    ## ETH      -62 -223  9646 11557 11165 34056 53988 87123  71008    31817    11864
    ## ADA      -75 -213 10178 17337  7728 24325 50521 71008 152544    30349    12195
    ## PORT5050 -16 -133  5540  6309  6327 14770 31379 31817  30349    18459     6832
    ## PORT9505  -6  -36  4401  4774  5390  9439  9263 11864  12195     6832     4644

The covariance matrix above captures not just the degree of variation in
each asset return series, but also the degree to which each pair
co-moves with each other. This is often normalized by individual asset
volatilities to give a correlation matrix:

``` r
Rho = cor(FINAL,use="pairwise.complete.obs")
Rho |> round(digits=2)
```

    ##             rf   inf   SPY  GOOG  AAPL  TSLA   BTC   ETH   ADA PORT5050
    ## rf        1.00  0.03 -0.06 -0.11 -0.18 -0.29 -0.10 -0.18 -0.16    -0.10
    ## inf       0.03  1.00 -0.10 -0.06 -0.08 -0.05 -0.26 -0.19 -0.14    -0.25
    ## SPY      -0.06 -0.10  1.00  0.75  0.79  0.56  0.44  0.50  0.40     0.62
    ## GOOG     -0.11 -0.06  0.75  1.00  0.59  0.50  0.36  0.42  0.48     0.50
    ## AAPL     -0.18 -0.08  0.79  0.59  1.00  0.70  0.31  0.37  0.19     0.46
    ## TSLA     -0.29 -0.05  0.56  0.50  0.70  1.00  0.36  0.48  0.26     0.45
    ## BTC      -0.10 -0.26  0.44  0.36  0.31  0.36  1.00  0.77  0.55     0.98
    ## ETH      -0.18 -0.19  0.50  0.42  0.37  0.48  0.77  1.00  0.62     0.79
    ## ADA      -0.16 -0.14  0.40  0.48  0.19  0.26  0.55  0.62  1.00     0.57
    ## PORT5050 -0.10 -0.25  0.62  0.50  0.46  0.45  0.98  0.79  0.57     1.00
    ## PORT9505 -0.07 -0.13  0.99  0.75  0.78  0.57  0.57  0.59  0.46     0.74
    ##          PORT9505
    ## rf          -0.07
    ## inf         -0.13
    ## SPY          0.99
    ## GOOG         0.75
    ## AAPL         0.78
    ## TSLA         0.57
    ## BTC          0.57
    ## ETH          0.59
    ## ADA          0.46
    ## PORT5050     0.74
    ## PORT9505     1.00

### Real Returns

Before we start calculating Sharpe ratios and betas, we first should
adjust our asset returns for inflation to convert from ‘nominal’ returns
to ‘real’ returns.

``` r
REAL = xts(order.by=index(FINAL))
REAL$rf = (FINAL$rf-FINAL$inf)/(1+(FINAL$inf/100))
REAL$SPY = (FINAL$SPY-FINAL$inf)/(1+(FINAL$inf/100))
REAL$GOOG = (FINAL$GOOG-FINAL$inf)/(1+(FINAL$inf/100))
REAL$AAPL = (FINAL$AAPL-FINAL$inf)/(1+(FINAL$inf/100))
REAL$TSLA = (FINAL$TSLA-FINAL$inf)/(1+(FINAL$inf/100))
REAL$BTC = (FINAL$BTC-FINAL$inf)/(1+(FINAL$inf/100))
REAL$ETH = (FINAL$ETH-FINAL$inf)/(1+(FINAL$inf/100))
REAL$ADA = (FINAL$ADA-FINAL$inf)/(1+(FINAL$inf/100))
REAL$PORT5050 = (FINAL$PORT5050-FINAL$inf)/(1+(FINAL$inf/100))
REAL$PORT9505 = (FINAL$PORT9505-FINAL$inf)/(1+(FINAL$inf/100))
```

If we examine the same summary statistics for the real returns, we can
see that the risk-free rate is actually negative due to high inflation.
Then the rest of the results are qualitatively similar to before with
just minor changes to some numbers due to the inflation adjustment.

``` r
RealEr = colMeans(REAL,na.rm=TRUE)
RealEr |> round(digits=2)
```

    ##       rf      SPY     GOOG     AAPL     TSLA      BTC      ETH      ADA 
    ##    -1.52    10.51    15.79    27.92    43.31    45.10    52.95    49.65 
    ## PORT5050 PORT9505 
    ##    27.81    12.24

``` r
Realsigma = apply(REAL,2,sd,na.rm=TRUE)
Realsigma |> round(digits=2)
```

    ##       rf      SPY     GOOG     AAPL     TSLA      BTC      ETH      ADA 
    ##     3.89    64.96    91.66    99.31   235.46   227.86   286.19   378.40 
    ## PORT5050 PORT9505 
    ##   131.89    67.65

``` r
RealRho = cor(REAL,use="pairwise.complete.obs")
RealRho |> round(digits=2)
```

    ##            rf  SPY GOOG AAPL TSLA  BTC  ETH  ADA PORT5050 PORT9505
    ## rf       1.00 0.15 0.09 0.08 0.00 0.23 0.15 0.09     0.24     0.18
    ## SPY      0.15 1.00 0.76 0.80 0.57 0.45 0.51 0.42     0.64     0.99
    ## GOOG     0.09 0.76 1.00 0.60 0.52 0.38 0.43 0.49     0.51     0.76
    ## AAPL     0.08 0.80 0.60 1.00 0.71 0.32 0.38 0.21     0.47     0.78
    ## TSLA     0.00 0.57 0.52 0.71 1.00 0.37 0.49 0.27     0.46     0.59
    ## BTC      0.23 0.45 0.38 0.32 0.37 1.00 0.78 0.56     0.98     0.58
    ## ETH      0.15 0.51 0.43 0.38 0.49 0.78 1.00 0.63     0.80     0.60
    ## ADA      0.09 0.42 0.49 0.21 0.27 0.56 0.63 1.00     0.59     0.47
    ## PORT5050 0.24 0.64 0.51 0.47 0.46 0.98 0.80 0.59     1.00     0.75
    ## PORT9505 0.18 0.99 0.76 0.78 0.59 0.58 0.60 0.47     0.75     1.00

### Excess Returns

The last step before we calculate the Sharpe ratios is converting the
real asset returns into ‘excess returns’. This involves subtracting the
risk-free returns from each asset’s return series. Since our average
real risk-free return was negative, this actually increases the expected
returns. One way to think about what this does is that the theoretical
risk-free asset should have zero variance. Since our T-bill risk-free
asset does not have a zero variance, this adjustment normalizes the
scenario to where the ‘excess return adjustment’ (subtracting rf) would
force the risk-free returns to have zero variance, and thus be more
appropriate for modeling. Interestingly, even after the inflation and
risk-free adjustments, Cardano’s ADA is still more highly correlated
with Alphabet’s stock, than it is with its primary smart contract
competitor, ETH.

``` r
XS = xts(order.by=index(FINAL))
XS$SPY = REAL$SPY-REAL$rf
XS$GOOG = REAL$GOOG-REAL$rf
XS$AAPL = REAL$AAPL-REAL$rf
XS$TSLA = REAL$TSLA-REAL$rf
XS$BTC = REAL$BTC-REAL$rf
XS$ETH = REAL$ETH-REAL$rf
XS$ADA = REAL$ADA-REAL$rf
XS$PORT5050 = REAL$PORT5050-REAL$rf
XS$PORT9505 = REAL$PORT9505-REAL$rf
```

``` r
xsEr = colMeans(XS,na.rm=TRUE)
xsEr |> round(digits=2)
```

    ##      SPY     GOOG     AAPL     TSLA      BTC      ETH      ADA PORT5050 
    ##    12.04    17.32    29.44    44.83    46.63    54.47    51.17    29.33 
    ## PORT9505 
    ##    13.77

``` r
xssigma = apply(XS,2,sd,na.rm=TRUE)
xssigma |> round(digits=2)
```

    ##      SPY     GOOG     AAPL     TSLA      BTC      ETH      ADA PORT5050 
    ##    64.50    91.40    99.05   235.48   226.99   285.65   378.05   131.02 
    ## PORT9505 
    ##    67.08

``` r
xsRho = cor(XS,use="pairwise.complete.obs")
xsRho |> round(digits=2)
```

    ##           SPY GOOG AAPL TSLA  BTC  ETH  ADA PORT5050 PORT9505
    ## SPY      1.00 0.76 0.80 0.58 0.44 0.50 0.41     0.63     0.99
    ## GOOG     0.76 1.00 0.60 0.52 0.37 0.43 0.49     0.51     0.76
    ## AAPL     0.80 0.60 1.00 0.71 0.31 0.37 0.21     0.46     0.78
    ## TSLA     0.58 0.52 0.71 1.00 0.36 0.49 0.27     0.46     0.59
    ## BTC      0.44 0.37 0.31 0.36 1.00 0.78 0.56     0.98     0.57
    ## ETH      0.50 0.43 0.37 0.49 0.78 1.00 0.63     0.80     0.59
    ## ADA      0.41 0.49 0.21 0.27 0.56 0.63 1.00     0.59     0.47
    ## PORT5050 0.63 0.51 0.46 0.46 0.98 0.80 0.59     1.00     0.74
    ## PORT9505 0.99 0.76 0.78 0.59 0.57 0.59 0.47     0.74     1.00

## Sharpe Ratios

The Sharpe ratio compares the ratio of the excess return of portfolio
with its volatility. For simplicity, we will calculate this for
portfolios invested 100% in each individual asset, followed by the two
portfolios that we constructed.

The formula: $Sharpe$ $Ratio = \dfrac{R_p-R_f}{\sigma_p}$ divides the
excess return by the standard deviation of the excess returns.

``` r
Sharpes_5yr = xsEr/xssigma
Sharpes_5yr |> round(digits=2)
```

    ##      SPY     GOOG     AAPL     TSLA      BTC      ETH      ADA PORT5050 
    ##     0.19     0.19     0.30     0.19     0.21     0.19     0.14     0.22 
    ## PORT9505 
    ##     0.21

## Betas

Another important distinction that is often made in finance is
decomposing risk (volatility) into systematic piece and an idiosyncratic
piece. In the Capital Asset Pricing Model, systematic risk is
effectively the portion of an asset/portfolio’s return that can be
explained by a single market factor. This concept has branched off a
vast literature on asset pricing, which includes multifactor models. We
will stick with the single-factor CAPM where the S&P 500 returns are a
fairly typical measure of broader market movements.

$r_i - r_f = \alpha + \beta*(r_m - r_f) +\epsilon$

In the above equation, $r_i$ is the real excess return for company $i$,
$r_f$ is the real risk-free rate, and $r_m$ is the real market return.
We then use our data to estimate $\alpha$ and $\beta$, and then our
model errors are represented by $\epsilon$.

To fit the CAPM relationship, we estimate a linear regression of the
GOOG excess returns on the market (S&P500) excess returns. The summary
function prints the regression estimates/results, and then ggplot lets
us easily plot the relationship. For GOOG, we get a beta estimate
reasonably close to 1, which suggests that Alphabet has roughly an equal
level of systematic risk as the market as a whole (or at least as
measured by the S&P 500). Thus, the interpretation is that if the market
goes up or down by 1%, we would expect that GOOG would also increase or
decrease by the same amount (percentage-wise).

Then let’s make note of the strength of the fit with the $R^2>0.5$.
Theoretically, this would indicate the percent of variation in GOOG that
is explained by broader market movements, but since GOOG is a major
component of the S&P 500, that might not be the best interpretation for
this particular example. In other words, it shouldn’t be surprising that
the beta for GOOG is close to 1.

The last bit of info that we will make note of from these results is the
`(Intercept)` coefficient. In CAPM, this estimate of ‘alpha’ represents
the average annual outperformance of the asset over the market. Thus,
GOOG has outperformed the market on average over the past 5 years.

``` r
GOOGfit = lm(GOOG~SPY,data=XS)
summary(GOOGfit)
```

    ## 
    ## Call:
    ## lm(formula = GOOG ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -122.259  -37.167   -5.904   29.474  144.981 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.3429     7.8619   0.552    0.583    
    ## SPY           1.0779     0.1208   8.923 1.78e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 59.85 on 58 degrees of freedom
    ## Multiple R-squared:  0.5785, Adjusted R-squared:  0.5713 
    ## F-statistic: 79.61 on 1 and 58 DF,  p-value: 1.78e-12

``` r
ggplot(XS,aes(x=SPY,y=GOOG))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/googfit-1.png)<!-- -->

Now with Apple, we get a slightly larger beta, which suggests that a 1%
increase in the market is expected to correspond with an amplified
increase in the price of AAPL. Similarly, a 1% decline in the market
would be expected to correlate with a larger decrease in the price of
AAPL. Along with this larger systematic risk, the alpha estimate shows
that AAPL has compensated for this higher risk with a higher average
return over the market.

``` r
AAPLfit = lm(AAPL~SPY,data=XS)
summary(AAPLfit)
```

    ## 
    ## Call:
    ## lm(formula = AAPL ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -150.143  -42.547   -6.899   44.113  126.897 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  14.6668     7.8896   1.859   0.0681 .  
    ## SPY           1.2273     0.1212  10.124 1.97e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 60.06 on 58 degrees of freedom
    ## Multiple R-squared:  0.6386, Adjusted R-squared:  0.6324 
    ## F-statistic: 102.5 on 1 and 58 DF,  p-value: 1.965e-14

``` r
ggplot(XS,aes(x=SPY,y=AAPL))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/aaplfit-1.png)<!-- -->

With Tesla, we see there is a much larger beta estimate. This also
corresponds with a larger alpha suggesting this increased systematic
risk has been compensated well. However, the strength of this
relationship is weaker than the previous two examples. Although Tesla is
still a fairly recent addition to the S&P 500 (late-2020), it has
quickly jumped into one of the top 10 spots in terms of market
capitalization (and thus, weight in the index). However, it remains
smaller than both Alphabet and Apple, so the weaker relationship is not
surprising.

``` r
TSLAfit = lm(TSLA~SPY,data=XS)
summary(TSLAfit)
```

    ## 
    ## Call:
    ## lm(formula = TSLA ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -397.23  -87.89  -13.56  152.19  491.03 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.4671    25.4803   0.764    0.448    
    ## SPY           2.1071     0.3915   5.382 1.39e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 194 on 58 degrees of freedom
    ## Multiple R-squared:  0.333,  Adjusted R-squared:  0.3215 
    ## F-statistic: 28.96 on 1 and 58 DF,  p-value: 1.388e-06

``` r
ggplot(XS,aes(x=SPY,y=TSLA))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/tslafit-1.png)<!-- -->

Similarly, we can calculate the beta for our other assets and
portfolios.

``` r
BTCfit = lm(BTC~SPY,data=XS)
summary(BTCfit)
```

    ## 
    ## Call:
    ## lm(formula = BTC ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -523.28 -127.85   -4.65  124.35  660.17 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  27.8481    26.9582   1.033  0.30589    
    ## SPY           1.5600     0.4142   3.766  0.00039 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 205.2 on 58 degrees of freedom
    ## Multiple R-squared:  0.1965, Adjusted R-squared:  0.1826 
    ## F-statistic: 14.18 on 1 and 58 DF,  p-value: 0.0003896

``` r
ggplot(XS,aes(y=BTC,x=SPY))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/btcfit-1.png)<!-- -->

``` r
ETHfit = lm(ETH~SPY,data=XS)
summary(ETHfit)
```

    ## 
    ## Call:
    ## lm(formula = ETH ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -511.13 -121.71   -7.83  128.86  752.51 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  27.5780    32.6778   0.844    0.402    
    ## SPY           2.2343     0.5021   4.450 3.96e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 248.8 on 58 degrees of freedom
    ## Multiple R-squared:  0.2545, Adjusted R-squared:  0.2416 
    ## F-statistic:  19.8 on 1 and 58 DF,  p-value: 3.958e-05

``` r
ggplot(XS,aes(y=ETH,x=SPY))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/ethfit-1.png)<!-- -->

``` r
ADAfit = lm(ADA~SPY,data=XS)
summary(ADAfit)
```

    ## 
    ## Call:
    ## lm(formula = ADA ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -496.88 -233.73  -73.99  134.53 1434.27 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  22.1158    45.6460   0.485  0.62985   
    ## SPY           2.4137     0.7014   3.441  0.00108 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 347.5 on 58 degrees of freedom
    ## Multiple R-squared:  0.1696, Adjusted R-squared:  0.1552 
    ## F-statistic: 11.84 on 1 and 58 DF,  p-value: 0.00108

``` r
ggplot(XS,aes(y=ADA,x=SPY))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/adafit-1.png)<!-- -->

Now let’s check out the portfolios. Since the S&P 500 is one of the two
weights, the beta will transition from bitcoin’s beta at $w_{BTC}=1$ to
a beta of 1 at $w_{BTC}=0$.

``` r
PORT5050fit = lm(PORT5050~SPY,data=XS)
summary(PORT5050fit)
```

    ## 
    ## Call:
    ## lm(formula = PORT5050 ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -261.64  -63.93   -2.33   62.18  330.09 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  13.9241    13.4791   1.033    0.306    
    ## SPY           1.2800     0.2071   6.180 6.87e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 102.6 on 58 degrees of freedom
    ## Multiple R-squared:  0.3971, Adjusted R-squared:  0.3867 
    ## F-statistic: 38.19 on 1 and 58 DF,  p-value: 6.866e-08

``` r
ggplot(XS,aes(y=PORT5050,x=SPY))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/port1fit-1.png)<!-- -->

``` r
PORT9505fit = lm(PORT9505~SPY,data=XS)
summary(PORT9505fit)
```

    ## 
    ## Call:
    ## lm(formula = PORT9505 ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -26.164  -6.393  -0.233   6.218  33.009 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.39241    1.34791   1.033    0.306    
    ## SPY          1.02800    0.02071  49.633   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.26 on 58 degrees of freedom
    ## Multiple R-squared:  0.977,  Adjusted R-squared:  0.9766 
    ## F-statistic:  2463 on 1 and 58 DF,  p-value: < 2.2e-16

``` r
ggplot(XS,aes(y=PORT9505,x=SPY))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/port2fit-1.png)<!-- -->

### Crypto-Betas

As an experiment, we can calculate the “beta” for some of the assets
using BTC as the market index, rather than the S&P 500. From this, we
can see from the $R^2$’s that the other cryptos achieve a closer fit
than they did with the S&P 500. Then the betas under 1 suggest that the
impact of BTC volatility is dampened when impacting the other cryptos.
The alphas show that after removing the volatility explained away by
BTC, both ETH and ADA generated a positive excess real return.

``` r
ETHBTCfit = lm(ETH~BTC,data=XS)
summary(ETHBTCfit)
```

    ## 
    ## Call:
    ## lm(formula = ETH ~ BTC, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -282.27 -139.48  -14.23   74.03  514.51 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.9890    23.9967   0.375    0.709    
    ## BTC           0.9755     0.1044   9.344  3.6e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 182 on 58 degrees of freedom
    ## Multiple R-squared:  0.6009, Adjusted R-squared:  0.594 
    ## F-statistic: 87.31 on 1 and 58 DF,  p-value: 3.603e-13

``` r
ggplot(XS,aes(y=ETH,x=BTC))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/ethbtcfit-1.png)<!-- -->

``` r
ADABTCfit = lm(ADA~BTC,data=XS)
summary(ADABTCfit)
```

    ## 
    ## Call:
    ## lm(formula = ADA ~ BTC, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -444.01 -227.24  -52.53  153.05 1191.99 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7.8032    41.7019   0.187    0.852    
    ## BTC           0.9301     0.1814   5.127 3.54e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 316.3 on 58 degrees of freedom
    ## Multiple R-squared:  0.3119, Adjusted R-squared:    0.3 
    ## F-statistic: 26.28 on 1 and 58 DF,  p-value: 3.543e-06

``` r
ggplot(XS,aes(y=ADA,x=BTC))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/adabtcfit-1.png)<!-- -->

We can also regress the ADA excess returns on the ETH excess returns.

``` r
ADAETHfit = lm(ADA~ETH,data=XS)
summary(ADAETHfit)
```

    ## 
    ## Call:
    ## lm(formula = ADA ~ ETH, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -405.53 -184.78  -62.28  133.35 1454.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   6.0248    39.0823   0.154    0.878    
    ## ETH           0.8288     0.1355   6.117 8.74e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 297.3 on 58 degrees of freedom
    ## Multiple R-squared:  0.3921, Adjusted R-squared:  0.3817 
    ## F-statistic: 37.42 on 1 and 58 DF,  p-value: 8.739e-08

``` r
ggplot(XS,aes(y=ADA,x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/adaethfit-1.png)<!-- -->

### Case Study: Tesla and Bitcoin

Next, we will model Tesla’s excess returns as a function of Bitcoin’s
excess returns. Since Tesla has been purchasing Bitcoin to hold on their
balance sheet, we might expect an interesting, strong result. While we
do find an interesting result, it is for the opposite reason. Both the
BTC “beta” and the regression $R^2$ are quite small.

``` r
TSLABTCfit = lm(TSLA~BTC,data=XS)
summary(TSLABTCfit)
```

    ## 
    ## Call:
    ## lm(formula = TSLA ~ BTC, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -553.83 -118.01   12.81  149.97  591.46 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  27.2069    29.1605   0.933  0.35469   
    ## BTC           0.3780     0.1269   2.979  0.00421 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 221.2 on 58 degrees of freedom
    ## Multiple R-squared:  0.1327, Adjusted R-squared:  0.1178 
    ## F-statistic: 8.877 on 1 and 58 DF,  p-value: 0.004212

``` r
ggplot(XS,aes(y=TSLA,x=BTC))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/tslabtcfit-1.png)<!-- -->

From the Tesla 10-K Filing for the Year Ended 12/31/2020:

“In January 2021, we updated our investment policy…including digital
assets…Thereafter, we invested an aggregate \$1.50 billion in bitcoin
under this policy and may acquire and hold digital assets from time to
time or long-term.”

*Note: During Q2 2022, Tesla sold roughly 80% of their bitcoin holdings.
Their balance sheet reported \$1.26B at the end of Q1 2022 and \$0.22B
at the end of Q2 2022.*

So let’s split the time series into two parts: before this updated
Bitcoin policy, and after. Then we compute/display the correlations of
the excess returns and estimate a linear regression for each window.

``` r
subXS1 = XS[index(XS)<"2021-01-01",]
subXS2 = XS[index(XS)>="2021-01-01",]
cor(subXS1$BTC,subXS1$TSLA)
```

    ##          TSLA
    ## BTC 0.2783652

``` r
cor(subXS2$BTC,subXS2$TSLA)
```

    ##          TSLA
    ## BTC 0.3905702

``` r
TSLABTCfit1 = lm(TSLA~BTC,data=subXS1)
TSLABTCfit2 = lm(TSLA~BTC,data=subXS2)
summary(TSLABTCfit1)
```

    ## 
    ## Call:
    ## lm(formula = TSLA ~ BTC, data = subXS1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -563.28 -171.42   29.35  194.38  538.11 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  82.9034    58.0915   1.427    0.168
    ## BTC           0.3106     0.2285   1.359    0.188
    ## 
    ## Residual standard error: 262.2 on 22 degrees of freedom
    ## Multiple R-squared:  0.07749,    Adjusted R-squared:  0.03555 
    ## F-statistic: 1.848 on 1 and 22 DF,  p-value: 0.1878

``` r
summary(TSLABTCfit2)
```

    ## 
    ## Call:
    ## lm(formula = TSLA ~ BTC, data = subXS2)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -521.6  -84.3    6.8  102.4  290.3 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  -5.3803    31.6610  -0.170   0.8661  
    ## BTC           0.3693     0.1493   2.474   0.0185 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 189.7 on 34 degrees of freedom
    ## Multiple R-squared:  0.1525, Adjusted R-squared:  0.1276 
    ## F-statistic:  6.12 on 1 and 34 DF,  p-value: 0.01852

``` r
ggplot(subXS1,aes(y=TSLA,x=BTC))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/tslabtcfit2-1.png)<!-- -->

``` r
ggplot(subXS2,aes(y=TSLA,x=BTC))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/tslabtcfit2-2.png)<!-- -->
