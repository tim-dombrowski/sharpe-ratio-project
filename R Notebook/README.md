Sharpe Ratio Comparisons
================

## R Packages

- The [fredr package](https://cran.r-project.org/package=fredr) is an R
  package that wraps the FRED API for easy importing of FRED data into
  R.
- The [quantmod package](https://cran.r-project.org/package=quantmod)
  contains tools for importing and analyzing financial data.
- The [xts package](https://cran.r-project.org/package=xts) allows for
  some additional time series functionality.
- The [reshape2 package](https://cran.r-project.org/package=reshape2) is
  used to prepare data frames for ggplot2 graphics.
- The [ggplot2 package](https://cran.r-project.org/package=ggplot2)
  includes tools for generating graphics and visuals.
- The [rmarkdown package](https://cran.r-project.org/package=rmarkdown)
  is used to generate this R Notebook.

The first three lines in this setup chunk automatically install any R
packages that you may be missing. One note regarding any code chunk
labeled ‘setup’ is that the R Notebook will automatically run it prior
to any other code chunk.

``` r
list.of.packages = c("fredr",
                     "quantmod", 
                     "xts", 
                     "reshape2", 
                     "ggplot2", 
                     "rmarkdown")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(fredr)
```

    ## Warning: package 'fredr' was built under R version 4.4.2

``` r
library(quantmod)
```

    ## Warning: package 'quantmod' was built under R version 4.4.2

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 4.4.2

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.4.2

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: TTR

    ## Warning: package 'TTR' was built under R version 4.4.2

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(xts)
library(reshape2)
```

    ## Warning: package 'reshape2' was built under R version 4.4.2

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.4.2

Set the width of the output boxes to be a bit wider than usual. \*This
will help when outputting matrices.\*

``` r
options(width=120)
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
            "NVDA",
            "TSLA",
            "MSTR",
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
NVDA$Return = c(NA, diff(log(as.numeric(NVDA$NVDA.Adjusted))))
TSLA$Return = c(NA, diff(log(as.numeric(TSLA$TSLA.Adjusted))))
MSTR$Return = c(NA, diff(log(as.numeric(MSTR$MSTR.Adjusted))))
BTC$Return = c(NA, diff(log(as.numeric(BTC$`BTC-USD.Adjusted`))))
ETH$Return = c(NA, diff(log(as.numeric(ETH$`ETH-USD.Adjusted`))))
ADA$Return = c(NA, diff(log(as.numeric(ADA$`ADA-USD.Adjusted`))))
# Annualize returns
SPY$SPY = SPY$Return*12*100
NVDA$NVDA = NVDA$Return*12*100
TSLA$TSLA = TSLA$Return*12*100
MSTR$MSTR = MSTR$Return*12*100
BTC$BTC = BTC$Return*12*100
ETH$ETH = ETH$Return*12*100
ADA$ADA = ADA$Return*12*100
# Merge to ALL
ALL = merge(ALL, 
            SPY$SPY,
            NVDA$NVDA,
            TSLA$TSLA,
            MSTR$MSTR,
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
ggplot(FINAL,aes(x=Index,y=NVDA))+
  geom_col()+
  xlab("")+
  ggtitle("NVDA Returns")
```

![](README_files/figure-gfm/otherplots-3.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=TSLA))+
  geom_col()+
  xlab("")+
  ggtitle("TSLA Returns")
```

![](README_files/figure-gfm/otherplots-4.png)<!-- -->

``` r
ggplot(FINAL,aes(x=Index,y=MSTR))+
  geom_col()+
  xlab("")+
  ggtitle("MSTR Returns")
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

    ##       rf      inf      SPY     NVDA     TSLA     MSTR      BTC      ETH      ADA PORT5050 PORT9505 
    ##     2.61     4.09    14.11    65.58    49.53    55.39    40.65    52.31    42.11    27.38    15.44

Now let’s calculate the volatility (standard deviation of returns) for
each of these assets. This is done by simply calculating the standard
deviation `sd()` of each return series (reduced to just one line of code
using the `apply()` function).

``` r
sigma = apply(FINAL,2,sd,na.rm=TRUE)
sigma |> round(digits=2)
```

    ##       rf      inf      SPY     NVDA     TSLA     MSTR      BTC      ETH      ADA PORT5050 PORT9505 
    ##     1.37     3.97    63.57   167.49   238.90   330.10   228.34   287.19   385.12   134.10    67.27

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

    ##            rf  inf   SPY  NVDA  TSLA   MSTR   BTC   ETH    ADA PORT5050 PORT9505
    ## rf          2    0    -3     9   -92     20   -26   -80   -123      -14       -4
    ## inf         0   16   -19   -44   -64   -179  -186  -195   -224     -102      -27
    ## SPY        -3  -19  4041  6641  8212  10964  7873 11292  11301     5957     4233
    ## NVDA        9  -44  6641 28054 17104  26241 11968 21184  16580     9305     6908
    ## TSLA      -92  -64  8212 17104 57075  32695 23666 37134  29632    15939     8985
    ## MSTR       20 -179 10964 26241 32695 108966 54516 62616  63942    32740    13142
    ## BTC       -26 -186  7873 11968 23666  54516 52141 51412  50698    30007    10086
    ## ETH       -80 -195 11292 21184 37134  62616 51412 82479  69459    31352    13298
    ## ADA      -123 -224 11301 16580 29632  63942 50698 69459 148317    30999    13271
    ## PORT5050  -14 -102  5957  9305 15939  32740 30007 31352  30999    17982     7160
    ## PORT9505   -4  -27  4233  6908  8985  13142 10086 13298  13271     7160     4525

The covariance matrix above captures not just the degree of variation in
each asset return series, but also the degree to which each pair
co-moves with each other. This is often normalized by individual asset
volatilities to give a correlation matrix:

``` r
Rho = cor(FINAL,use="pairwise.complete.obs")
Rho |> round(digits=2)
```

    ##             rf   inf   SPY  NVDA  TSLA  MSTR   BTC   ETH   ADA PORT5050 PORT9505
    ## rf        1.00 -0.07 -0.03  0.04 -0.28  0.04 -0.08 -0.20 -0.23    -0.08    -0.04
    ## inf      -0.07  1.00 -0.07 -0.07 -0.07 -0.14 -0.21 -0.17 -0.15    -0.19    -0.10
    ## SPY      -0.03 -0.07  1.00  0.62  0.54  0.52  0.54  0.62  0.46     0.70     0.99
    ## NVDA      0.04 -0.07  0.62  1.00  0.43  0.47  0.31  0.44  0.26     0.41     0.61
    ## TSLA     -0.28 -0.07  0.54  0.43  1.00  0.41  0.43  0.54  0.32     0.50     0.56
    ## MSTR      0.04 -0.14  0.52  0.47  0.41  1.00  0.72  0.66  0.50     0.74     0.59
    ## BTC      -0.08 -0.21  0.54  0.31  0.43  0.72  1.00  0.78  0.58     0.98     0.66
    ## ETH      -0.20 -0.17  0.62  0.44  0.54  0.66  0.78  1.00  0.63     0.81     0.69
    ## ADA      -0.23 -0.15  0.46  0.26  0.32  0.50  0.58  0.63  1.00     0.60     0.51
    ## PORT5050 -0.08 -0.19  0.70  0.41  0.50  0.74  0.98  0.81  0.60     1.00     0.79
    ## PORT9505 -0.04 -0.10  0.99  0.61  0.56  0.59  0.66  0.69  0.51     0.79     1.00

### Real Returns

Before we start calculating Sharpe ratios and betas, we first should
adjust our asset returns for inflation to convert from ‘nominal’ returns
to ‘real’ returns.

``` r
REAL = xts(order.by=index(FINAL))
REAL$rf = (FINAL$rf-FINAL$inf)/(1+(FINAL$inf/100))
REAL$SPY = (FINAL$SPY-FINAL$inf)/(1+(FINAL$inf/100))
REAL$NVDA = (FINAL$NVDA-FINAL$inf)/(1+(FINAL$inf/100))
REAL$TSLA = (FINAL$TSLA-FINAL$inf)/(1+(FINAL$inf/100))
REAL$MSTR = (FINAL$MSTR-FINAL$inf)/(1+(FINAL$inf/100))
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

    ##       rf      SPY     NVDA     TSLA     MSTR      BTC      ETH      ADA PORT5050 PORT9505 
    ##    -1.28     9.97    59.71    44.53    51.01    36.96    48.27    38.74    23.46    11.32

``` r
Realsigma = apply(REAL,2,sd,na.rm=TRUE)
Realsigma |> round(digits=2)
```

    ##       rf      SPY     NVDA     TSLA     MSTR      BTC      ETH      ADA PORT5050 PORT9505 
    ##     4.08    62.93   160.55   233.07   316.16   218.60   277.41   373.13   129.51    66.50

``` r
RealRho = cor(REAL,use="pairwise.complete.obs")
RealRho |> round(digits=2)
```

    ##            rf  SPY NVDA TSLA MSTR  BTC  ETH  ADA PORT5050 PORT9505
    ## rf       1.00 0.13 0.11 0.01 0.14 0.18 0.11 0.07     0.18     0.15
    ## SPY      0.13 1.00 0.62 0.56 0.52 0.56 0.63 0.48     0.71     0.99
    ## NVDA     0.11 0.62 1.00 0.43 0.47 0.31 0.43 0.26     0.41     0.61
    ## TSLA     0.01 0.56 0.43 1.00 0.41 0.44 0.55 0.34     0.51     0.57
    ## MSTR     0.14 0.52 0.47 0.41 1.00 0.72 0.66 0.51     0.73     0.58
    ## BTC      0.18 0.56 0.31 0.44 0.72 1.00 0.79 0.59     0.98     0.67
    ## ETH      0.11 0.63 0.43 0.55 0.66 0.79 1.00 0.64     0.82     0.70
    ## ADA      0.07 0.48 0.26 0.34 0.51 0.59 0.64 1.00     0.62     0.53
    ## PORT5050 0.18 0.71 0.41 0.51 0.73 0.98 0.82 0.62     1.00     0.80
    ## PORT9505 0.15 0.99 0.61 0.57 0.58 0.67 0.70 0.53     0.80     1.00

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
XS$NVDA = REAL$NVDA-REAL$rf
XS$TSLA = REAL$TSLA-REAL$rf
XS$MSTR = REAL$MSTR-REAL$rf
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

    ##      SPY     NVDA     TSLA     MSTR      BTC      ETH      ADA PORT5050 PORT9505 
    ##    11.25    60.98    45.81    52.29    38.24    49.55    40.02    24.74    12.60

``` r
xssigma = apply(XS,2,sd,na.rm=TRUE)
xssigma |> round(digits=2)
```

    ##      SPY     NVDA     TSLA     MSTR      BTC      ETH      ADA PORT5050 PORT9505 
    ##    62.53   160.14   233.06   315.61   217.92   277.01   372.87   128.84    66.03

``` r
xsRho = cor(XS,use="pairwise.complete.obs")
xsRho |> round(digits=2)
```

    ##           SPY NVDA TSLA MSTR  BTC  ETH  ADA PORT5050 PORT9505
    ## SPY      1.00 0.61 0.56 0.51 0.55 0.63 0.48     0.71     0.99
    ## NVDA     0.61 1.00 0.43 0.46 0.30 0.43 0.26     0.41     0.60
    ## TSLA     0.56 0.43 1.00 0.41 0.44 0.55 0.34     0.51     0.58
    ## MSTR     0.51 0.46 0.41 1.00 0.72 0.65 0.50     0.73     0.58
    ## BTC      0.55 0.30 0.44 0.72 1.00 0.79 0.59     0.98     0.66
    ## ETH      0.63 0.43 0.55 0.65 0.79 1.00 0.64     0.82     0.70
    ## ADA      0.48 0.26 0.34 0.50 0.59 0.64 1.00     0.61     0.53
    ## PORT5050 0.71 0.41 0.51 0.73 0.98 0.82 0.61     1.00     0.80
    ## PORT9505 0.99 0.60 0.58 0.58 0.66 0.70 0.53     0.80     1.00

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

    ##      SPY     NVDA     TSLA     MSTR      BTC      ETH      ADA PORT5050 PORT9505 
    ##     0.18     0.38     0.20     0.17     0.18     0.18     0.11     0.19     0.19

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
NVDA excess returns on the market (S&P500) excess returns. The summary
function prints the regression estimates/results, and then ggplot lets
us easily plot the relationship. For NVDA, we get a beta estimate
reasonably close to 1, which suggests that Alphabet has roughly an equal
level of systematic risk as the market as a whole (or at least as
measured by the S&P 500). Thus, the interpretation is that if the market
goes up or down by 1%, we would expect that NVDA would also increase or
decrease by the same amount (percentage-wise).

Then let’s make note of the strength of the fit with the $R^2>0.5$.
Theoretically, this would indicate the percent of variation in NVDA that
is explained by broader market movements, but since NVDA is a major
component of the S&P 500, that might not be the best interpretation for
this particular example. In other words, it shouldn’t be surprising that
the beta for NVDA is close to 1.

The last bit of info that we will make note of from these results is the
`(Intercept)` coefficient. In CAPM, this estimate of ‘alpha’ represents
the average annual outperformance of the asset over the market. Thus,
NVDA has outperformed the market on average over the past 5 years.

``` r
NVDAfit = lm(NVDA~SPY,data=XS)
summary(NVDAfit)
```

    ## 
    ## Call:
    ## lm(formula = NVDA ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -323.24  -85.07  -19.53   66.58  317.31 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  43.3059    16.7288   2.589   0.0122 *  
    ## SPY           1.5721     0.2654   5.922 1.83e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 127.5 on 58 degrees of freedom
    ## Multiple R-squared:  0.3768, Adjusted R-squared:  0.3661 
    ## F-statistic: 35.07 on 1 and 58 DF,  p-value: 1.83e-07

``` r
ggplot(XS,aes(x=SPY,y=NVDA))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/NVDAfit-1.png)<!-- -->

Now with Apple, we get a slightly larger beta, which suggests that a 1%
increase in the market is expected to correspond with an amplified
increase in the price of TSLA. Similarly, a 1% decline in the market
would be expected to correlate with a larger decrease in the price of
TSLA. Along with this larger systematic risk, the alpha estimate shows
that TSLA has compensated for this higher risk with a higher average
return over the market.

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
    ## -405.45  -90.34  -16.27  150.67  491.60 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  22.3749    25.5718   0.875    0.385    
    ## SPY           2.0835     0.4058   5.135 3.44e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 194.9 on 58 degrees of freedom
    ## Multiple R-squared:  0.3125, Adjusted R-squared:  0.3007 
    ## F-statistic: 26.37 on 1 and 58 DF,  p-value: 3.442e-06

``` r
ggplot(XS,aes(x=SPY,y=TSLA))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/TSLAfit-1.png)<!-- -->

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
MSTRfit = lm(MSTR~SPY,data=XS)
summary(MSTRfit)
```

    ## 
    ## Call:
    ## lm(formula = MSTR ~ SPY, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -529.72 -189.52    6.43  163.04  645.92 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  23.1505    35.8394   0.646    0.521    
    ## SPY           2.5914     0.5687   4.557 2.73e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 273.2 on 58 degrees of freedom
    ## Multiple R-squared:  0.2636, Adjusted R-squared:  0.2509 
    ## F-statistic: 20.76 on 1 and 58 DF,  p-value: 2.725e-05

``` r
ggplot(XS,aes(x=SPY,y=MSTR))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/MSTRfit-1.png)<!-- -->

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
    ## -515.70 -114.49  -14.25  127.21  359.69 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  16.6669    24.0777   0.692    0.492    
    ## SPY           1.9180     0.3821   5.020 5.22e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 183.5 on 58 degrees of freedom
    ## Multiple R-squared:  0.3029, Adjusted R-squared:  0.2909 
    ## F-statistic:  25.2 on 1 and 58 DF,  p-value: 5.225e-06

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
    ## -412.22 -114.95   -5.44  132.60  680.81 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  18.2215    28.4995   0.639    0.525    
    ## SPY           2.7860     0.4522   6.161 7.39e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 217.2 on 58 degrees of freedom
    ## Multiple R-squared:  0.3955, Adjusted R-squared:  0.3851 
    ## F-statistic: 37.95 on 1 and 58 DF,  p-value: 7.395e-08

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
    ## -509.68 -192.30  -62.58  117.14 1430.33 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   7.9627    43.3381   0.184 0.854863    
    ## SPY           2.8508     0.6877   4.146 0.000112 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 330.3 on 58 degrees of freedom
    ## Multiple R-squared:  0.2286, Adjusted R-squared:  0.2153 
    ## F-statistic: 17.19 on 1 and 58 DF,  p-value: 0.0001118

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
    ##      Min       1Q   Median       3Q      Max 
    ## -257.851  -57.244   -7.124   63.605  179.843 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    8.334     12.039   0.692    0.492    
    ## SPY            1.459      0.191   7.637 2.49e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 91.76 on 58 degrees of freedom
    ## Multiple R-squared:  0.5014, Adjusted R-squared:  0.4928 
    ## F-statistic: 58.33 on 1 and 58 DF,  p-value: 2.487e-10

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
    ##      Min       1Q   Median       3Q      Max 
    ## -25.7851  -5.7244  -0.7124   6.3605  17.9843 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.8334     1.2039   0.692    0.492    
    ## SPY           1.0459     0.0191  54.751   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9.176 on 58 degrees of freedom
    ## Multiple R-squared:  0.981,  Adjusted R-squared:  0.9807 
    ## F-statistic:  2998 on 1 and 58 DF,  p-value: < 2.2e-16

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
    ## -280.46 -110.06  -20.12   69.66  511.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  11.3889    22.6816   0.502    0.617    
    ## BTC           0.9981     0.1034   9.657 1.11e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 173 on 58 degrees of freedom
    ## Multiple R-squared:  0.6166, Adjusted R-squared:  0.6099 
    ## F-statistic: 93.26 on 1 and 58 DF,  p-value: 1.113e-13

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
    ## -447.56 -184.87  -55.55  143.03 1166.35 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.4304    39.8132   0.036    0.971    
    ## BTC           1.0093     0.1814   5.563 7.06e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 303.7 on 58 degrees of freedom
    ## Multiple R-squared:  0.348,  Adjusted R-squared:  0.3367 
    ## F-statistic: 30.95 on 1 and 58 DF,  p-value: 7.062e-07

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
    ## -408.39 -175.58  -46.23  124.37 1455.06 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -2.6442    37.9210  -0.070    0.945    
    ## ETH           0.8610     0.1359   6.338 3.76e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 289.1 on 58 degrees of freedom
    ## Multiple R-squared:  0.4092, Adjusted R-squared:  0.399 
    ## F-statistic: 40.17 on 1 and 58 DF,  p-value: 3.757e-08

``` r
ggplot(XS,aes(y=ADA,x=ETH))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/adaethfit-1.png)<!-- -->

### Case Study: Microstrategy and Bitcoin

Next, we will model Microstrategy’s excess returns as a function of
Bitcoin’s excess returns. Since Microstrategy has been purchasing
Bitcoin to hold on their balance sheet, we might expect an interesting,
strong result.

``` r
MSTRBTCfit = lm(MSTR~BTC,data=XS)
summary(MSTRBTCfit)
```

    ## 
    ## Call:
    ## lm(formula = MSTR ~ BTC, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -438.79 -149.12  -24.74  133.76  447.76 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  12.6460    29.1353   0.434    0.666    
    ## BTC           1.0369     0.1328   7.810 1.27e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 222.2 on 58 degrees of freedom
    ## Multiple R-squared:  0.5126, Adjusted R-squared:  0.5042 
    ## F-statistic:    61 on 1 and 58 DF,  p-value: 1.275e-10

``` r
ggplot(XS,aes(y=MSTR,x=BTC))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/MSTRbtcfit1-1.png)<!-- -->

``` r
MSTRBTCfit2 = lm(MSTR~SPY+BTC,data=XS)
summary(MSTRBTCfit2)
```

    ## 
    ## Call:
    ## lm(formula = MSTR ~ SPY + BTC, data = XS)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -432.2 -152.6  -34.6  124.8  415.7 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.1442    28.9072   0.282    0.779    
    ## SPY           0.8645     0.5471   1.580    0.120    
    ## BTC           0.9004     0.1570   5.735 3.89e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 219.4 on 57 degrees of freedom
    ## Multiple R-squared:  0.5331, Adjusted R-squared:  0.5167 
    ## F-statistic: 32.54 on 2 and 57 DF,  p-value: 3.75e-10

### Case Study: Tesla and Bitcoin

Tesla is another company that has made a significant investment in
Bitcoin.

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
    ## -554.83 -136.44    5.77  147.20  592.42 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  27.8137    27.6742   1.005 0.319052    
    ## BTC           0.4705     0.1261   3.731 0.000435 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 211.1 on 58 degrees of freedom
    ## Multiple R-squared:  0.1936, Adjusted R-squared:  0.1797 
    ## F-statistic: 13.92 on 1 and 58 DF,  p-value: 0.0004353

``` r
ggplot(XS,aes(y=TSLA,x=BTC))+
  geom_point()+
  geom_smooth(method="lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/TSLAbtcfit1-1.png)<!-- -->

``` r
TSLABTCfit2 = lm(TSLA~SPY+BTC,data=XS)
summary(TSLABTCfit2)
```

    ## 
    ## Call:
    ## lm(formula = TSLA ~ SPY + BTC, data = XS)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -423.50 -114.37    7.93  137.52  480.35 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  18.9916    25.4239   0.747 0.458136    
    ## SPY           1.6942     0.4812   3.521 0.000854 ***
    ## BTC           0.2030     0.1381   1.470 0.147017    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 193 on 57 degrees of freedom
    ## Multiple R-squared:  0.3376, Adjusted R-squared:  0.3144 
    ## F-statistic: 14.53 on 2 and 57 DF,  p-value: 7.968e-06
