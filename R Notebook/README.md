Bitcoin Price Time Series Analysis
================
Last updated: 2025-11-03

## Preliminary Work: Install/Load Packages

To try and ensure that this R Notebook will run successfully, we’ll use
the [renv
package](https://cran.r-project.org/web/packages/renv/index.html) to
create a project-specific library of packages. This will allow us to
install the packages that we need for this project without affecting any
other projects that we may be working on. Additionally, the project
library will track the specific versions of the dependency packages so
that any updates to those packages will not break this project.

The code chunk below will first install the renv package if it is not
already installed. Then we will load the package. Next, we’ll use the
`restore()` function to install any packages listed in the renv.lock
file. Once these packages are installed, we can load them into the R
session using the `library()` commands. Below the code chunk, we’ll list
out the packages that will be used in the project demo. And if you run
into any trouble using renv, then you can use the second code chunk
below and that should be an even more reliable approach to install the
required packages.

``` r
# Install renv package if not already installed
if(!"renv" %in% installed.packages()[,"Package"]) install.packages("renv")
# Load renv package
library(renv)
# Use restore() to install any packages listed in the renv.lock file
renv::restore(clean=TRUE, lockfile="../renv.lock")
# Load in the packages
library(quantmod)
library(tidyverse)
library(scales)
library(tseries)
library(seasonal)
library(seasonalview)
library(rmarkdown)
```

- The [quantmod package](https://cran.r-project.org/package=quantmod)
  contains tools for importing and analyzing financial data.
- The [tidyverse package](https://www.tidyverse.org/) contains a suite
  of packages for data manipulation and visualization.
- The [scales package](https://cran.r-project.org/package=scales) has
  tools for changing the format of axis labels in plots (e.g. dollar
  amounts, percentages, etc.)
- The [tseries package](https://cran.r-project.org/package=tseries)
  contains additional time series analysis functions that we will
  explore.
- The [seasonal package](https://cran.r-project.org/package=seasonal)
  contains an interface to the [Census X-13-ARIMA-SEATS
  model](https://www.census.gov/data/software/x13as.html).
- The [seasonalview
  package](https://cran.r-project.org/package=seasonalview) brings a
  graphical user interface to that Census model.
- The [rmarkdown package](https://cran.r-project.org/package=rmarkdown)
  is used to generate this R Notebook.

Since the rmarkdown functionality is built into RStudio, this last one
is automatically loaded when you open RStudio. So no need to use the
`library()` function for it. Another observation to make about the code
chunk above is that it is labeled as `setup`, which is a special name,
which the R Notebook will recognize and automatically run prior to
running any other code chunk. This is useful for loading in packages and
setting up other global options that will be used throughout the
notebook.

Then if you wish to try and update the versions of the various R
packages in the lock file, you can use the `renv::update()` function to
update the packages in the project library. However, it is possible that
these updates could break the code in this notebook. If so, you may need
to adapt the code to work with the updated packages.

My recommendation is to first run through the code using the versions of
the packages in the lock file. Then if you want to try and update the
packages, you can do so and then run through the code again to see if it
still works. If not, you can always revert back to the lock file
versions using the `renv::restore()` function.

If you update the packages and get everything working successfully, then
you can update the lock file using the `renv::snapshot()` function. This
will update the lock file with the versions of the packages that are
currently installed in the project library. Then you can commit the
updated lock file to the repository so that others can use the updated
versions of the packages.

### Alternative Package Installation Code

If you run into any trouble using renv in the code chunk above, then you
can use the code chunk below to install the required packages for this
analysis. This method will first check if you have already installed the
packages. If any are missing, it will then install them. Then it will
load the packages into the R session. A potential flaw in this approach
compared to using renv is that it will simply install the latest
versions of the packages, which could potentially break some of the code
in this notebook if any of the updates aren’t backwards compatible.

As long as you have downloaded the entire project repository, the renv
chunk above will likely be managing the packages. Thus, the `eval=FALSE`
option is used to prevent this chunk from running unless manually
executed. But if you only downloaded this one Rmd file, this code chunk
should take care of installing the packages for you.

``` r
# Create list of packages needed for this exercise, omit geckor since its not on CRAN
list.of.packages = c("quantmod","tidyverse","scales","tseries","seasonal","seasonalview","rmarkdown")
# Check if any have not yet been installed
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# If any need to be installed, install them
if(length(new.packages)) install.packages(new.packages)
# Load in the packages
library(quantmod)
library(tidyverse)
library(scales)
library(tseries)
library(seasonal)
library(seasonalview)
library(rmarkdown)
```

### Set Directories and Paths

Set up paths to save data files and other relevant directories:

``` r
# Create general data directory in project root
datadir = "../Data/"
if(!dir.exists(datadir)) dir.create(datadir)
# Create raw data directory within data directory
rawdir = paste0(datadir,"Raw/")
if(!dir.exists(rawdir)) dir.create(rawdir)
# Create a figures directory in project root
figdir = "../Figures/"
if(!dir.exists(figdir)) dir.create(figdir)
```

## Bitcoin Data Import and Cleaning

The `getSymbols()` function from the quantmod package let’s us import
the daily price data for bitcoin. The `src="yahoo"` argument specifies
that we want to pull the data from Yahoo Finance. The `from` and `to`
arguments specify the date range for the data, which goes back to
October 2014 for bitcoin. Rather that assigning a variable name to the
output of `getSymbols()`, the function creates a new variable for each
ticker in the list, named after the ticker. For bitcoin in particular,
the `"BTC-USD"` can be tedious to work with since the presence of a dash
requires one to wrap the variable name in backticks. This is resolved in
the last line of the code chunk below by creating a new variable `BTC`
to be used instead.

``` r
# Set parameters and download data
startdate = "2014-10-01"
tickers = c("BTC-USD")
getSymbols(tickers,
           src="yahoo",
           from=startdate,
           to=Sys.Date())
# Create a copy with a simpler name
BTC = `BTC-USD`
# Save the data to a csv file
write.csv(BTC, file=paste0(rawdir,"BTC_daily_all.csv"), row.names=FALSE)
# Delete old variable to minimize memory usage
rm(`BTC-USD`)
```

If the code chunk above generates an error about missing values, this
can occur due to the most recent observation having a missing value. To
resolve this, we can just filter the data with `complete.cases()`, which
removes any rows with any missing values.

``` r
BTC = BTC[complete.cases(BTC),]
```

Now that we have the daily bitcoin price series, let’s extract the close
prices into a univariate xts object. Then we can rename the column to
`"Close"` to keep variable names simple. After that, let’s aggregate the
daily prices to weekly and monthly series to compare the time series
properties at different frequencies. The xts package (which is a
dependency of quantmod) allows us to easily convert the daily price data
into weekly or monthly OHLC series using `to.weekly()` and
`to.monthly()`. The `name=NULL` option in those functions prevents the
variable names from including the name of the original object.

``` r
# Create daily series and rename column
BTCdaily = BTC$`BTC-USD.Close` 
names(BTCdaily) = "Close"
# Convert to weekly and monthly series
BTCweeksOHLC = to.weekly(BTCdaily$Close, name=NULL)
BTCweeks = BTCweeksOHLC$Close
write.csv(BTCweeks, file=paste0(rawdir,"BTC_weekly_all.csv"), row.names=FALSE)
BTCmonthOHLC = to.monthly(BTCdaily$Close, name=NULL)
BTCmonth = BTCmonthOHLC$Close
write.csv(BTCmonth, file=paste0(rawdir,"BTC_monthly_all.csv"), row.names=FALSE)
```

## Exploring the Full Bitcoin Price Series

### Basic Price Charts

We’ll start with some simple plots of the BTC price series that we
downloaded. Starting with the daily chart, then weekly and monthly. The
[ggplot2 package](https://ggplot2.tidyverse.org/) (part of tidyverse) is
used to generate the plots. The first element of the plot will be a line
chart (`geom_line()`) of the price series. Note that the `index()`
function is used to extract the date index from the xts object for the
plot. Then we’ll clear out the x-axis label with `xlab("")` since the
dates are self-explanatory. The y-axis labels are formatted as dollar
amounts using the `dollar_format()` function from the scales package,
and the `ggtitle()` function is used to add a title to each plot. After
each plot is created, it is displayed in the notebook and then saved
using `ggsave()`. *Note: For the monthly plot, the y input needed to be
formatted back to a Date since the scale package does not appear to be
compatible with the yearmon format.*

``` r
BTCdaily_plot_price = ggplot() +
  geom_line(aes(x=index(BTCdaily),y=BTCdaily)) +
  xlab("") +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(labels=dollar_format(), n.breaks=6) +
  ggtitle("Daily Bitcoin Price Series (Oct. 2014 - present)")
BTCdaily_plot_price
```

![](README_files/figure-gfm/btcplotraw-1.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCdaily_plot_price.pdf"), BTCdaily_plot_price, width=8, height=4)

BTCweeks_plot_price = ggplot() +
  geom_line(aes(x=index(BTCweeks),y=BTCweeks)) +
  xlab("") +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(labels=dollar_format(), n.breaks=6) +
  ggtitle("Weekly Bitcoin Price Series (Oct. 2014 - present)")
BTCweeks_plot_price
```

![](README_files/figure-gfm/btcplotraw-2.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCweeks_plot_price.pdf"), BTCweeks_plot_price, width=8, height=4)

BTCmonth_plot_price = ggplot() +
  geom_line(aes(x=as.Date(index(BTCmonth)),y=BTCmonth)) +
  xlab("") +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(labels=dollar_format(), n.breaks=6) +
  ggtitle("Monthly Bitcoin Price Series (Oct. 2014 - present)")
BTCmonth_plot_price
```

![](README_files/figure-gfm/btcplotraw-3.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCmonth_plot_price.pdf"), BTCmonth_plot_price, width=8, height=4)
```

### Log Price Charts

In regard to statistical analysis of time series data, one drawback of
the directly analyzing the price series above is that they are
*non-stationary*. This property will be explored in more detail below,
but it is visually depicted in the plots above through the observation
that the mean value (and variance) of the series is not constant over
time. To resolve this, we can convert the price series to a stationary
process by taking its derivative (or by differencing the observations
over time to measure price changes/returns). By using log returns, we
effectively assume continuous compounding of the price series, which
reflects the perpetual nature of cryptocurrency markets.

$$r_t = \ln(P_t) - \ln(P_{t-1}) = \ln\left(\dfrac{P_t}{P_{t-1}}\right)$$

The first step in calculating continuously compounded growth rates is to
take the natural logarithm of the prices. To visualize this
transformation, let’s apply two approaches for plotting out the log
price charts. The first approach is to simply compute the log(prices)
within the ggplot specifications. However, note that the scaling on the
y-axis is hard to interpret. The second approach will keep the original
dollar format with logarithmic scaling. Since the second version will be
more intuitive, we won’t bother saving these to files.

``` r
ggplot() +
  geom_line(aes(x=index(BTCdaily), y=log(BTCdaily))) +
  xlab("") +
  ggtitle("Daily Bitcoin Log Price Series (Oct. 2014 - present)")
```

    ## Don't know how to automatically pick scale for object of type <xts/zoo>.
    ## Defaulting to continuous.

![](README_files/figure-gfm/btcplotlog-1.png)<!-- -->

``` r
ggplot() +
  geom_line(aes(x=index(BTCweeks), y=log(BTCweeks))) +
  xlab("") +
  ggtitle("Weekly Bitcoin Log Price Series (Oct. 2014 - present)")
```

    ## Don't know how to automatically pick scale for object of type <xts/zoo>.
    ## Defaulting to continuous.

![](README_files/figure-gfm/btcplotlog-2.png)<!-- -->

``` r
ggplot() +
  geom_line(aes(x=index(BTCmonth), y=log(BTCmonth))) +
  xlab("") +
  ggtitle("Monthly Bitcoin Log Price Series (Oct. 2014 - present)")
```

    ## Don't know how to automatically pick scale for object of type <xts/zoo>.
    ## Defaulting to continuous.

![](README_files/figure-gfm/btcplotlog-3.png)<!-- -->

Alternatively, we can generate the same charts by instead plotting the
non-logged price series with a logarithmic y-axis. This is done in
`ggplot` with the `scale_y_continuous(transform='log10')` setting. Then
the `largest_with_cents=1` argument is added to the `dollar_format()`
function to remove decimals/cents from the y-axis labels.

``` r
BTCdaily_plot_logprice = ggplot() +
  geom_line(aes(x=index(BTCdaily), y=BTCdaily)) +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(transform='log10', labels=dollar_format(largest_with_cents=1)) +
  xlab("") +
  ggtitle("Daily Bitcoin Price Series with Logarithmic Y-axis Scaling")
BTCdaily_plot_logprice
```

![](README_files/figure-gfm/btcplotlog_alt-1.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCdaily_plot_logprice.pdf"), BTCdaily_plot_logprice, width=8, height=4)

BTCweeks_plot_logprice = ggplot() +
  geom_line(aes(x=index(BTCweeks), y=BTCweeks)) +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(transform='log10', labels=dollar_format(largest_with_cents=1)) +
  xlab("") +
  ggtitle("Weekly Bitcoin Price Series with Logarithmic Y-axis Scaling")
BTCweeks_plot_logprice
```

![](README_files/figure-gfm/btcplotlog_alt-2.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCweeks_plot_logprice.pdf"), BTCweeks_plot_logprice, width=8, height=4)

BTCmonth_plot_logprice = ggplot() +
  geom_line(aes(x=as.Date(index(BTCmonth)), y=BTCmonth)) +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(transform='log10', labels=dollar_format(largest_with_cents=1)) +
  xlab("") +
  ggtitle("Monthly Bitcoin Price Series with Logarithmic Y-axis Scaling")
BTCmonth_plot_logprice
```

![](README_files/figure-gfm/btcplotlog_alt-3.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCmonth_plot_logprice.pdf"), BTCmonth_plot_logprice, width=8, height=4)
```

### Log Return Charts

Lastly, we’ll finish the price to return conversion by differencing the
log prices. This can be done directly by composing the `diff()` and
`log()` functions. The `log()` function will take the natural logarithm
of the prices, and then the `diff()` function will subtract the previous
period’s value from the current period’s value. Then to make the series
more comparable, we’ll annualize the returns by multiplying by 365 (for
daily), 52 (for weekly), and 12 (for monthly). Lastly, we’ll adjust the
units to percentages and delete the first observations of each since
those are `NA` due to the differencing.

``` r
# Create data frames with date column
rBTCdaily = data.frame(Date=index(BTCdaily))
rBTCweeks = data.frame(Date=index(BTCweeks))
rBTCmonth = data.frame(Date=index(BTCmonth))
# Compute log returns
rBTCdaily$Return = diff(log(BTCdaily))
rBTCweeks$Return = diff(log(BTCweeks))
rBTCmonth$Return = diff(log(BTCmonth))
# Annualize returns and shift units to percentages
rBTCdaily$AnnRet = rBTCdaily$Return*100*365
rBTCweeks$AnnRet = rBTCweeks$Return*100*52
rBTCmonth$AnnRet = rBTCmonth$Return*100*12
# Remove first row of each data frame (missing due to differencing)
rBTCdaily = rBTCdaily[-1,]
rBTCweeks = rBTCweeks[-1,]
rBTCmonth = rBTCmonth[-1,]
```

With these return series, we resolve much of the non-stationarity
present in the price series. With a more stationary process like
returns, a better way to plot the data is with a bar chart. The average
bar height (red dashed line) represents the average annual return. The
`percent_format()` function from the scales package is used to convert
the y-axis labels to percentages, and the `scale=1` parameter prevents
it from treating the percentage unit variables as decimals.

``` r
BTCdaily_plot_logret = ggplot(rBTCdaily,aes(x=Date,y=AnnRet))+
  geom_col()+
  geom_hline(yintercept=mean(rBTCdaily$AnnRet), linetype='dashed', col='red')+
  xlab("") +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(labels=percent_format(scale=1), n.breaks=6) +
  ggtitle("BTC Annualized Daily Returns")
BTCdaily_plot_logret
```

![](README_files/figure-gfm/btcplotret-1.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCdaily_plot_logret.pdf"), BTCdaily_plot_logret, width=8, height=4)

BTCweeks_plot_logret = ggplot(rBTCweeks,aes(x=Date,y=AnnRet))+
  geom_col()+
  geom_hline(yintercept=mean(rBTCdaily$AnnRet), linetype='dashed', col='red')+
  xlab("") +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(labels=percent_format(scale=1), n.breaks=6) +
  ggtitle("BTC Annualized Weekly Returns")
BTCweeks_plot_logret
```

![](README_files/figure-gfm/btcplotret-2.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCweeks_plot_logret.pdf"), BTCweeks_plot_logret, width=8, height=4)

BTCmonth_plot_logret = ggplot(rBTCmonth,aes(x=as.Date(Date),y=AnnRet)) +
  geom_col()+
  geom_hline(yintercept=mean(rBTCmonth$AnnRet), linetype='dashed', col='red')+
  xlab("") +
  scale_x_date(date_labels="%Y", date_breaks="2 year") +
  scale_y_continuous(labels=percent_format(scale=1), n.breaks=6) +
  ggtitle("BTC Annualized Monthly Returns")
BTCmonth_plot_logret
```

![](README_files/figure-gfm/btcplotret-3.png)<!-- -->

``` r
ggsave(paste0(figdir,"BTCmonth_plot_logret.pdf"), BTCmonth_plot_logret, width=8, height=4)
```

### Summary Statistics

In summarizing the return series, we can compare across the various
frequencies. Since we annualized each series for its respective
frequency, the means represent the average annual returns for BTC.

``` r
meanAnnRets = data.frame(BTCdaily=mean(rBTCdaily$AnnRet), 
                         BTCweeks=mean(rBTCweeks$AnnRet), 
                         BTCmonth=mean(rBTCmonth$AnnRet))
meanAnnRets |> round(digits=2)
```

    ##   BTCdaily BTCweeks BTCmonth
    ## 1    51.05    52.58    52.24

From the volatilities (as measured by standard deviation of the return
series), we can see that the higher frequencies produce larger standard
deviations.

``` r
meanRetVols = data.frame(BTCdaily=sd(rBTCdaily$AnnRet), 
                         BTCweeks=sd(rBTCweeks$AnnRet), 
                         BTCmonth=sd(rBTCmonth$AnnRet))
meanRetVols |> round(digits=2)
```

    ##   BTCdaily BTCweeks BTCmonth
    ## 1  1297.13   490.04   234.35

One way to visualize the empirical distribution summarized by the mean
and standard deviation is to plot a frequency histogram of the return
series overlayed with a normal bell curve transformed by the empirical
mean and standard deviation. The below code chunk first selects a
reasonable bin width (`bw`) for each frequency (trial and error), then
saves a count of non-missing observations, and generates the plot
described above.

``` r
# Set desired binwidth and number of non-missing obs
bw = 500
n_daily = nrow(rBTCdaily)
# Plot histogram of annualized returns and overlay graph of normal bell curve with same means and standard deviation
ggplot(rBTCdaily,aes(AnnRet)) +
  geom_histogram(binwidth=bw) +
  stat_function(fun=function(x) dnorm(x, mean=mean(rBTCdaily$AnnRet),   
                                      sd=sd(rBTCdaily$AnnRet))*n_daily*bw, 
                color="darkred", linewidth=1) +
  xlab("Annualized Daily Growth Rates") +
  scale_x_continuous(labels=percent_format(scale=1)) +
  ylab("Frequency")
```

![](README_files/figure-gfm/btcplotbell-1.png)<!-- -->

``` r
# Same process for weekly series
bw = 100
n_weeks = nrow(rBTCweeks)
ggplot(rBTCweeks,aes(AnnRet)) +
  geom_histogram(binwidth=bw) +
  stat_function(fun=function(x) dnorm(x, mean=mean(rBTCweeks$AnnRet),
                                      sd=sd(rBTCweeks$AnnRet))*n_weeks*bw, 
                color="darkred", linewidth=1) +
  xlab("Annualized Weekly Growth Rates") +
  scale_x_continuous(labels=percent_format(scale=1)) +
  ylab("Frequency")
```

![](README_files/figure-gfm/btcplotbell-2.png)<!-- -->

``` r
# Same process for monthly series
bw = 50
n_month = nrow(rBTCmonth)
ggplot(rBTCmonth,aes(AnnRet)) +
  geom_histogram(binwidth=bw) +
  stat_function(fun=function(x) dnorm(x, mean=mean(rBTCmonth$AnnRet),   
                                      sd=sd(rBTCmonth$AnnRet,))*n_month*bw, 
                color="darkred", linewidth=1) +
  xlab("Annualized Monthly Growth Rates") +
  scale_x_continuous(labels=percent_format(scale=1)) +
  ylab("Frequency")
```

![](README_files/figure-gfm/btcplotbell-3.png)<!-- -->

The plots above show how the daily and weekly return series more closely
resemble the fitted bell curves. However, they do tend to have more tail
events than would be suggested by a normally distributed return series.

## Time Series Stationarity, Autocorrelation, and the Dickey-Fuller Test

A [Stationary](https://search.brave.com/search?q=stationary+time+series)
[Process](https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc442.htm)
is one that has a constant mean, variance, and autocorrelation structure
over time. A [Non-Stationary
Process](https://search.brave.com/search?q=non-stationary+time+series)
can have different aspects of non-stationarity, and financial variables
often exhibit those types of trends. As we could visually see from the
price and return plots, the prices had a clear upward trend over the
long-run, whereas the return series exhibited ‘flatter’ dynamics.

A more explicit way to model and visualize the autocorrelation structure
of the time series is to examine its [autocorrelation function
(ACF)](https://www.itl.nist.gov/div898/handbook/eda/section3/autocopl.htm).
The ACF effectively measures the correlation between $e_t$ and
$e_{t−1}$, and then for $e_t$ and $e_{t−2}$, and so on for as many lags
as desired. In each frequency, the price series exhibits substantial
autocorrelation, and the return series shows that much of it is
resolved, although there still are a few lags that exceed the error
bands.

``` r
acf(as.numeric(BTCdaily))
```

![](README_files/figure-gfm/btcacf-1.png)<!-- -->

``` r
acf(rBTCdaily$AnnRet)
```

![](README_files/figure-gfm/btcacf-2.png)<!-- -->

``` r
acf(as.numeric(BTCweeks))
```

![](README_files/figure-gfm/btcacf-3.png)<!-- -->

``` r
acf(rBTCweeks$AnnRet)
```

![](README_files/figure-gfm/btcacf-4.png)<!-- -->

``` r
acf(as.numeric(BTCmonth))
```

![](README_files/figure-gfm/btcacf-5.png)<!-- -->

``` r
acf(rBTCmonth$AnnRet)
```

![](README_files/figure-gfm/btcacf-6.png)<!-- -->

The [partial autocorrelation function
(PACF)](https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4463.htm)
is similar to the ACF, except it produces the estimates of each lag
after controlling for all shorter lags. In other words, rather than
measuring the correlation between $e_t$ and $e_{t−2}$, we’d measure the
linear regression coefficient for the second lag from a regression
including both the first and second lags. A notable feature here from
the price series is a very large (nearly 1) estimate, which follows from
the large autocorrelation. Then subsequent lags often fall within the
error bands, although some lags do exceed them. After converting to
returns, the PACF’s have a much more condensed y-axis, which reflects
the lower autocorrelation in the return series. Although, there
certainly are still some lags that cross the error bands for the daily
and weekly series.

``` r
pacf(as.numeric(BTCdaily))
```

![](README_files/figure-gfm/btcpacf-1.png)<!-- -->

``` r
pacf(rBTCdaily$AnnRet)
```

![](README_files/figure-gfm/btcpacf-2.png)<!-- -->

``` r
pacf(as.numeric(BTCweeks))
```

![](README_files/figure-gfm/btcpacf-3.png)<!-- -->

``` r
pacf(rBTCweeks$AnnRet)
```

![](README_files/figure-gfm/btcpacf-4.png)<!-- -->

``` r
pacf(as.numeric(BTCmonth))
```

![](README_files/figure-gfm/btcpacf-5.png)<!-- -->

``` r
pacf(rBTCmonth$AnnRet)
```

![](README_files/figure-gfm/btcpacf-6.png)<!-- -->

From the ACF and PACF, we can gather some visual evidence to support the
suggestion that the price series suffers from autocorrelation
(non-stationarity) and that the return series is stationary. To
explicitly test for this, the
[Augmented](https://search.brave.com/search?q=adf+test)
[Dickey-Fuller](https://www.rdocumentation.org/packages/aTSA/versions/3.1.2/topics/adf.test)
[Test](https://cran.r-project.org/web/packages/tseries/tseries.pdf)
allows us to construct a null hypothesis of non-stationarity, which
implies that rejection of the null concludes stationarity.

The `adf.test()` function from the tseries package generates the test
statistic (Dickey-Fuller in the output) and p-value. We can see that the
price series have smaller (in magnitude) test statistics that fail to
reject the null, and then the return series produce larger (in
magnitude) test statistics that reject the null and conclude that the
return series are a stationary process.

``` r
adf.test(as.numeric(BTCdaily))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  as.numeric(BTCdaily)
    ## Dickey-Fuller = -1.3743, Lag order = 15, p-value = 0.8433
    ## alternative hypothesis: stationary

``` r
adf.test(rBTCdaily$AnnRet)
```

    ## Warning in adf.test(rBTCdaily$AnnRet): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  rBTCdaily$AnnRet
    ## Dickey-Fuller = -15.341, Lag order = 15, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(as.numeric(BTCweeks))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  as.numeric(BTCweeks)
    ## Dickey-Fuller = -1.8683, Lag order = 8, p-value = 0.634
    ## alternative hypothesis: stationary

``` r
adf.test(rBTCweeks$AnnRet)
```

    ## Warning in adf.test(rBTCweeks$AnnRet): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  rBTCweeks$AnnRet
    ## Dickey-Fuller = -7.2877, Lag order = 8, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(as.numeric(BTCmonth))
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  as.numeric(BTCmonth)
    ## Dickey-Fuller = -0.74956, Lag order = 5, p-value = 0.9638
    ## alternative hypothesis: stationary

``` r
adf.test(rBTCmonth$AnnRet)
```

    ## Warning in adf.test(rBTCmonth$AnnRet): p-value smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  rBTCmonth$AnnRet
    ## Dickey-Fuller = -4.3168, Lag order = 5, p-value = 0.01
    ## alternative hypothesis: stationary

If you want to dig deeper into the theory and underlying assumptions
behind these concepts, [Stationarity and Memory in Financial
Markets](https://towardsdatascience.com/non-stationarity-and-memory-in-financial-markets-fcef1fe76053)
has a nice discussion on the topic from a general context of financial
markets.

## Seasonality

### Day-of-Week Seasonality

Another area of concern when it comes to stationarity of a time series
is seasonality. For the data that we have here, the first type of
*season* to explore is day-of-week seasonality. One way to do this is to
generate a count variable ($count=1,2,...,T$), and then calculate the
remainder after dividing by 7. This [modulo
operation](https://en.wikipedia.org/wiki/Modulo_operation) is done with
`%%` in R. Then a quick test for seasonality is to regress the return
series on a factor variable of the remainders. The `as.factor()`
function will transform the numeric variable into a categorical variable
with 7 categories.

Another important piece to note in the second line below is that the
`+6` is used to shift which day of the week is set to the reference
category in the regression model. The 6-day shift was selected by first
running the regression without the shift and examining the estimates for
the day dummies. Since `as.factor(day)1` (Friday) produced the most
negative estimate, the increment of 6 to the count variable by shift
that up to 7, and $7 \mod 7 = 0$.

Thus, after increasing the count, the most negative day will be the
baseline, and all the day dummies should produce a positive coefficient.
This shift will maximize the likelihood that we find a significant
result from the regression by comparing each other day to one of the
extremes. As of April 25, 2025, there is a marginally significant
finding that day=4 (Mondays) tend to out-perform relative to day=0
(Thursdays). *Perhaps one could explore how a strategy around this
observation would perform? Buy at end of Thursday and sell at end of
Monday?*

``` r
rBTCdaily$count = 1:length(rBTCdaily$AnnRet)
rBTCdaily$day = (rBTCdaily$count+6) %% 7
weekreg = lm(AnnRet~as.factor(day),data=rBTCdaily)
summary(weekreg)
```

    ## 
    ## Call:
    ## lm(formula = AnnRet ~ as.factor(day), data = rBTCdaily)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16953.8   -502.4     -3.5    537.8   8225.7 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)       -8.878     53.903  -0.165    0.869  
    ## as.factor(day)1   71.064     76.231   0.932    0.351  
    ## as.factor(day)2   62.358     76.231   0.818    0.413  
    ## as.factor(day)3   27.663     76.231   0.363    0.717  
    ## as.factor(day)4  165.556     76.264   2.171    0.030 *
    ## as.factor(day)5    8.668     76.264   0.114    0.910  
    ## as.factor(day)6   84.322     76.264   1.106    0.269  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1297 on 4043 degrees of freedom
    ## Multiple R-squared:  0.001625,   Adjusted R-squared:  0.0001431 
    ## F-statistic: 1.097 on 6 and 4043 DF,  p-value: 0.3617

In addition to testing for seasonality, we can also include the count
variable as a linear time trend to see if returns are systematically
changing over time. As seen below, this loads slightly negative and very
insignificant.

``` r
weekreg2 = lm(AnnRet~count+as.factor(day),data=rBTCdaily)
summary(weekreg2)
```

    ## 
    ## Call:
    ## lm(formula = AnnRet ~ count + as.factor(day), data = rBTCdaily)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16954.0   -502.8     -3.2    535.9   8219.9 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)       4.749448  64.431046   0.074    0.941  
    ## count            -0.006733   0.017434  -0.386    0.699  
    ## as.factor(day)1  71.071085  76.238664   0.932    0.351  
    ## as.factor(day)2  62.371513  76.238670   0.818    0.413  
    ## as.factor(day)3  27.683106  76.238680   0.363    0.717  
    ## as.factor(day)4 165.559532  76.271631   2.171    0.030 *
    ## as.factor(day)5   8.677976  76.271635   0.114    0.909  
    ## as.factor(day)6  84.339196  76.271643   1.106    0.269  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1297 on 4042 degrees of freedom
    ## Multiple R-squared:  0.001662,   Adjusted R-squared:  -6.733e-05 
    ## F-statistic: 0.9611 on 7 and 4042 DF,  p-value: 0.458

The models above show the decomposition of an observed time series into
(1) a trend component, (2) a seasonal component, (3) and a random
(residual/error) component. The `decompose()` function separates a time
series into those components and can be used to product an easy visual
plot of the decomposition. In the first line, the annual return series
is transformed into a time series object using the `ts()` function,
which has better compatibility with the `decompose()` function. The
`frequency=7` allows us to set the period cycle for seasonality to be
seven days in a week. Then the time component is indexed by weeks since
the start of the data series, as seen in the plots below.

``` r
BTCdailyts = ts(as.numeric(BTCdaily), frequency=7)
BTCdailytsdecomp = decompose(BTCdailyts)
plot(BTCdailytsdecomp)
```

![](README_files/figure-gfm/weektsdecomp-1.png)<!-- -->

### Day-of-Year Seasonality

A more common approach to seasonality is to model the time of the year
to capture the impact of actual seasons. This can be done with the daily
price series; however, since the regression model would output
coefficients for all the 364 day dummy variables, we’ll just generate
the time series decomposition plot. One additional parameter we can
include in this case is indicating the starting point of the data. Since
this is an annual cycle, the first numeric input is the starting year,
and the 274 is derived from the first observation on October 1, which
was the 274th day of year in 2014. *An potential flaw here may be the
handling of leap years. I’m not sure that the tseries package is
correctly handling leap years when the frequency is set to 365. It is
likely just modeling a 365-day cycle, so every leap year will shift the
particular dates by 1.*

``` r
BTCdayyearts = ts(as.numeric(BTCdaily), frequency=365, start=c(2014,274))
BTCdayyeartsdecomp = decompose(BTCdayyearts)
plot(BTCdayyeartsdecomp)
```

![](README_files/figure-gfm/yearseason-1.png)<!-- -->

### Month-of-Year Seasonality

The next level to test for seasonality at is for each month of the year.
Similar to above, we create a count in the monthly data frame, calculate
$count \mod 12$ with a 1-month shift (to adjust the baseline to the
worst month), and then regress the returns on a factor variable of the
remainders. As of April 25, 2025, there is a marginally significant
finding that month=1 (October) tends to out-perform relative to month=0
(September). *How would a “Buy Sept 1, Sell Oct 31” strategy perform?*

``` r
rBTCmonth$count = 1:length(rBTCmonth$AnnRet)
rBTCmonth$month = (rBTCmonth$count+1) %% 12
monthreg = lm(AnnRet~as.factor(month),data=rBTCmonth)
summary(monthreg)
```

    ## 
    ## Call:
    ## lm(formula = AnnRet ~ as.factor(month), data = rBTCmonth)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -611.49 -132.39   -4.63  133.77  614.93 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)        -25.0253    70.4563  -0.355    0.723  
    ## as.factor(month)1  224.6567    99.6402   2.255    0.026 *
    ## as.factor(month)2   93.2243    97.5423   0.956    0.341  
    ## as.factor(month)3   96.2748    99.6402   0.966    0.336  
    ## as.factor(month)4   -7.2142    99.6402  -0.072    0.942  
    ## as.factor(month)5  151.5586    99.6402   1.521    0.131  
    ## as.factor(month)6    8.2870    99.6402   0.083    0.934  
    ## as.factor(month)7  123.1796    99.6402   1.236    0.219  
    ## as.factor(month)8   85.6853    99.6402   0.860    0.392  
    ## as.factor(month)9   26.9077    99.6402   0.270    0.788  
    ## as.factor(month)10 122.5497    99.6402   1.230    0.221  
    ## as.factor(month)11   0.6344    99.6402   0.006    0.995  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 233.7 on 121 degrees of freedom
    ## Multiple R-squared:  0.08859,    Adjusted R-squared:  0.005737 
    ## F-statistic: 1.069 on 11 and 121 DF,  p-value: 0.3917

Similar to the daily series, the linear time trend of the monthly series
is not statistically significant.

``` r
monthreg2 = lm(AnnRet~count+as.factor(month),data=rBTCmonth[rBTCmonth$Date!=as.yearmon("Nov 2013"),])
summary(monthreg2)
```

    ## 
    ## Call:
    ## lm(formula = AnnRet ~ count + as.factor(month), data = rBTCmonth[rBTCmonth$Date != 
    ##     as.yearmon("Nov 2013"), ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -617.06 -128.23   -5.11  125.49  603.77 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)         -3.0282    80.0740  -0.038   0.9699  
    ## count               -0.3098     0.5309  -0.584   0.5606  
    ## as.factor(month)1  224.9665    99.9142   2.252   0.0262 *
    ## as.factor(month)2   91.9850    97.8322   0.940   0.3490  
    ## as.factor(month)3   93.4864   100.0270   0.935   0.3519  
    ## as.factor(month)4   -9.6928   100.0030  -0.097   0.9229  
    ## as.factor(month)5  149.3899    99.9819   1.494   0.1378  
    ## as.factor(month)6    6.4280    99.9636   0.064   0.9488  
    ## as.factor(month)7  121.6305    99.9481   1.217   0.2260  
    ## as.factor(month)8   84.4460    99.9354   0.845   0.3998  
    ## as.factor(month)9   25.9783    99.9255   0.260   0.7953  
    ## as.factor(month)10 121.9301    99.9185   1.220   0.2247  
    ## as.factor(month)11   0.3246    99.9142   0.003   0.9974  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 234.3 on 120 degrees of freedom
    ## Multiple R-squared:  0.09117,    Adjusted R-squared:  0.0002887 
    ## F-statistic: 1.003 on 12 and 120 DF,  p-value: 0.4504

Now let’s plot out the full time series decomposition of the monthly
series. Since this is one of the more meaningful decompositions we’ll
generate, the final three lines below will create a high-quality output
of the plot in a pdf format.

``` r
BTCts_Close = ts(as.numeric(BTCmonth), frequency=12, start=c(2014,10))
BTCtsdecomp = decompose(BTCts_Close)
plot(BTCtsdecomp)
```

![](README_files/figure-gfm/monthtsdecomp-1.png)<!-- -->

``` r
# Create output pdf file for the plot, plot it, and save file.
pdf(paste0(figdir,"BTCtsdecomp_month_year.pdf"), width=6)
plot(BTCtsdecomp)
dev.off()
```

    ## png 
    ##   2

### Bitcoin Halving Seasonality

One feature of the Bitcoin protocol is the reduction in block rewards
that occurs every four years. Specifically, this fixed portion of mining
rewards that are new bitcoins being mined into existence get reduced by
50% every 210,000 blocks. Since the mining process produces a new block
every 10 minutes, this translates to roughly 4 years.

$1 halving = 210,000 blocks \cdot \dfrac{10min}{1block} \cdot \dfrac{1hr}{60min} \cdot \dfrac{1day}{24hr} = 1,458days+8hr = 208.\overline{33}weeks = 3.995years$

In addition to not exactly measuring 4 years, there is the additional
variation around the 10 minute-per-block timing in the short-term. The
Bitcoin protocol has a difficulty adjustment that takes effect every
2,016 blocks (\$\$14 days). This automatic adjustment allows the mining
difficulty to adapt to changes in network hash rate without impacting
the block timing, which enables Bitcoin’s algorithmic monetary
inflation.

So there isn’t quite a clean way to split the halving timeline into
perfect cycles at any of the frequencies. But we can round to the
nearest whole number and see what we get with each.

#### Daily Halving Seasonality

The Bitcoin halving cycle is closest to 1,458 days, so lets examine the
time series decomposition with that frequency. Since the decomposition
charts transform the date component to cycle-time, the points below
transform the halving dates to the corresponding time values on the
chart.

- 2012 Halving (Nov. 28, block 210,000): No price data yet
- 2016 Halving (July 9, block 420,000): `dayhalf`=647 $\implies$
  Time=1.44
- 2020 Halving (May 11, block 630,000): `dayhalf`=591 $\implies$
  Time=2.41
- 2024 Halving (April 20, block 840,000): `dayhalf`=573 $\implies$
  Time=3.39
- 2028 Halving (March?, block 1,050,000): `dayhalf`=??? $\implies$
  Time=4.??

Since we just added 1,457 dummy variables to the regression model, it is
not surprising that absorbed much of the volatility. This is also why we
don’t display the summary of the results. Then the lack of three full
cycles leads to a flat period in the residual (random). This is expected
since some of the estimates are based on only two observations. As time
moves on and more halvings are experienced, any seasonality around this
cycle should become more apparent.

``` r
rBTCdaily$dayhalf = rBTCdaily$count %% 1458
dailyhalfreg = lm(AnnRet~count+as.factor(dayhalf),data=rBTCdaily)
BTCdailyhalfts = ts(as.numeric(BTCdaily), frequency=1458)
BTCdailyhalftsdecomp = decompose(BTCdailyhalfts)
plot(BTCdailyhalftsdecomp)
```

![](README_files/figure-gfm/dailyhalf-1.png)<!-- -->

#### Weekly Halving Seasonality

Similarly, we can modeling this halving seasonality at the weekly
frequency. The halving cycle is closest to 208 weeks, so we’ll use that
as the frequency for the time series decomposition. Overall, we see
similar trends as with the daily data. Similar to the daily
decomposition chart, the dates are transformed to cycle-time. So the
points below transform the halving dates to the corresponding time
values on the chart.

- 2012 Halving (Nov. 28, block 210,000): No price data yet
- 2016 Halving (July 9, block 420,000): `weekhalf`=92 $\implies$
  Time=1.44
- 2020 Halving (May 11, block 630,000): `weekhalf`=85 $\implies$
  Time=2.41
- 2024 Halving (April 20, block 840,000): `weekhalf`=82 $\implies$
  Time=3.39
- 2028 Halving (March?, block 1,050,000): `weekhalf`=??? $\implies$
  Time=4.??

``` r
rBTCweeks$count = 1:length(rBTCweeks$AnnRet)
rBTCweeks$weekhalf = rBTCweeks$count %% 208
weekshalfreg = lm(AnnRet~count+as.factor(weekhalf),data=rBTCweeks)
BTCweekshalfts = ts(as.numeric(BTCweeks), frequency=208)
BTCweekshalftsdecomp = decompose(BTCweekshalfts)
plot(BTCweekshalftsdecomp)
```

![](README_files/figure-gfm/weekshalf-1.png)<!-- -->

#### Monthly Halving Seasonality

Since the monthly halving cycle is only 48 months, let’s also generate
the regression estimates to check for some significant months in a
halving cycle. As with the other frequencies, the points below transform
the halving dates to cycle-time. Then let’s also save this plot as a
pdf.

- 2012 Halving (Nov. 28, block 210,000): No price data yet
- 2016 Halving (July 9, block 420,000): `monthhalf`=21 $\implies$
  Time=1.44
- 2020 Halving (May 11, block 630,000): `monthhalf`=19 $\implies$
  Time=2.40
- 2024 Halving (April 20, block 840,000): `monthhalf`=18 $\implies$
  Time=3.38
- 2028 Halving (March?, block 1,050,000): `monthhalf`=?? $\implies$
  Time=4.??

``` r
BTCmonthhalfts = ts(as.numeric(BTCmonth), frequency=48)
BTCmonthhalftsdecomp = decompose(BTCmonthhalfts)
plot(BTCmonthhalftsdecomp)
```

![](README_files/figure-gfm/monthhalf-1.png)<!-- -->

``` r
rBTCmonth$monthhalf = rBTCmonth$count %% 48
monthhalfreg = lm(AnnRet~count+as.factor(monthhalf),data=rBTCmonth)
summary(monthhalfreg)
```

    ## 
    ## Call:
    ## lm(formula = AnnRet ~ count + as.factor(monthhalf), data = rBTCmonth)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -602.38 -109.90   -1.28   95.95  525.16 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)              48.5684   167.9668   0.289   0.7732  
    ## count                    -0.6269     0.5400  -1.161   0.2489  
    ## as.factor(monthhalf)1  -225.3899   211.3212  -1.067   0.2892  
    ## as.factor(monthhalf)2  -126.7153   211.2901  -0.600   0.5503  
    ## as.factor(monthhalf)3   -68.9026   211.2604  -0.326   0.7451  
    ## as.factor(monthhalf)4    90.1803   211.2322   0.427   0.6705  
    ## as.factor(monthhalf)5    76.6491   211.2052   0.363   0.7176  
    ## as.factor(monthhalf)6    88.7511   211.1797   0.420   0.6754  
    ## as.factor(monthhalf)7   135.2880   211.1555   0.641   0.5235  
    ## as.factor(monthhalf)8   178.1020   211.1328   0.844   0.4013  
    ## as.factor(monthhalf)9   -26.0209   211.1113  -0.123   0.9022  
    ## as.factor(monthhalf)10 -163.7413   211.0913  -0.776   0.4401  
    ## as.factor(monthhalf)11  -45.3717   211.0727  -0.215   0.8303  
    ## as.factor(monthhalf)12  245.3042   211.0554   1.162   0.2484  
    ## as.factor(monthhalf)13   18.6142   211.0395   0.088   0.9299  
    ## as.factor(monthhalf)14   68.3098   211.0250   0.324   0.7470  
    ## as.factor(monthhalf)15   36.8431   211.0119   0.175   0.8618  
    ## as.factor(monthhalf)16  171.6794   211.0001   0.814   0.4181  
    ## as.factor(monthhalf)17  -81.9094   210.9898  -0.388   0.6988  
    ## as.factor(monthhalf)18   75.5421   210.9808   0.358   0.7212  
    ## as.factor(monthhalf)19  139.7246   210.9732   0.662   0.5096  
    ## as.factor(monthhalf)20   45.2750   210.9670   0.215   0.8306  
    ## as.factor(monthhalf)21   62.6621   210.9621   0.297   0.7672  
    ## as.factor(monthhalf)22  -61.6685   210.9587  -0.292   0.7708  
    ## as.factor(monthhalf)23   15.6720   210.9566   0.074   0.9410  
    ## as.factor(monthhalf)24  191.7067   210.9559   0.909   0.3661  
    ## as.factor(monthhalf)25  290.3383   210.9566   1.376   0.1724  
    ## as.factor(monthhalf)26  243.9095   210.9587   1.156   0.2509  
    ## as.factor(monthhalf)27   90.9444   210.9621   0.431   0.6675  
    ## as.factor(monthhalf)28  123.7179   210.9670   0.586   0.5592  
    ## as.factor(monthhalf)29   59.0765   210.9732   0.280   0.7802  
    ## as.factor(monthhalf)30  136.8461   210.9808   0.649   0.5184  
    ## as.factor(monthhalf)31   79.8398   210.9898   0.378   0.7061  
    ## as.factor(monthhalf)32   18.3229   211.0001   0.087   0.9310  
    ## as.factor(monthhalf)33  161.0938   211.0119   0.763   0.4473  
    ## as.factor(monthhalf)34  222.7959   211.0250   1.056   0.2941  
    ## as.factor(monthhalf)35  -37.5718   211.0395  -0.178   0.8591  
    ## as.factor(monthhalf)36  282.3994   211.0554   1.338   0.1845  
    ## as.factor(monthhalf)37  162.9800   211.0727   0.772   0.4422  
    ## as.factor(monthhalf)38   60.2770   231.1537   0.261   0.7949  
    ## as.factor(monthhalf)39 -315.5355   231.1417  -1.365   0.1759  
    ## as.factor(monthhalf)40   71.1241   231.1310   0.308   0.7591  
    ## as.factor(monthhalf)41 -215.7798   231.1215  -0.934   0.3532  
    ## as.factor(monthhalf)42   48.5932   231.1133   0.210   0.8340  
    ## as.factor(monthhalf)43 -234.7470   231.1064  -1.016   0.3127  
    ## as.factor(monthhalf)44 -384.8465   231.1007  -1.665   0.0996 .
    ## as.factor(monthhalf)45  210.5798   231.0963   0.911   0.3648  
    ## as.factor(monthhalf)46 -155.9826   231.0932  -0.675   0.5015  
    ## as.factor(monthhalf)47  -59.0375   231.0913  -0.255   0.7990  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 231.1 on 84 degrees of freedom
    ## Multiple R-squared:  0.3812, Adjusted R-squared:  0.02762 
    ## F-statistic: 1.078 on 48 and 84 DF,  p-value: 0.3756

``` r
# Create output pdf file for the plot, plot it, and save file.
pdf(paste0(figdir,"BTCtsdecomp_month_half.pdf"), width=6)
plot(BTCmonthhalftsdecomp)
dev.off()
```

    ## png 
    ##   2

### A Deeper Dive on Seasonality

So far, we have just used some fairly basic seasonality tools from the
tseries package. A more in-depth package focused around seasonal
adjustments for time series data is the seasonal package. The `seas()`
function will output the main results from the X-13 ARIMA-SEATS seasonal
adjustment method. If you wish to explore this model in more detail,
uncomment the `view()` function in the code chunk below to generate a
shiny dashboard that allows you to interact with the model. This
dashboard is a local version of
[seasonal.website](http://www.seasonal.website/). See the [Introduction
to Seasonal](http://www.seasonal.website/seasonal.html) page for more
info.

``` r
BTCts_Close |> seas() ##|> view()
```

    ## 
    ## Call:
    ## seas(x = BTCts_Close)
    ## 
    ## Coefficients:
    ## AR-Nonseasonal-01  
    ##            0.1636

The above output shows us some of the results from the time series model
that was estimated (X-13 ARIMA-SEATS). This is identified even for the
price model because the return transformation that we did effectively
represents the “I” in the ARIMA of the model name. That “I” stands for
“integrated” in the longer acronym of [AutoRegressive Integrated Moving
Average](https://search.brave.com/search?q=arima). Thus, if you switch
from “Original and Adjusted Series” to the option directly below with a
(%), that will transform the graph to the same shape as the return
series, but without the annualization.

``` r
BTCts_AnnRet = ts(rBTCmonth$AnnRet, frequency=12, start=c(2014,11))
BTCts_AnnRet |> seas() ## |> view()
```

    ## 
    ## Call:
    ## seas(x = BTCts_AnnRet)
    ## 
    ## Coefficients:
    ##          Constant  AR-Nonseasonal-01  
    ##           52.2834             0.1225
