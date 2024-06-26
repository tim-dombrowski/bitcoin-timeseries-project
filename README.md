# Bitcoin Time Series Analysis Project

### Overview

This project focuses on analyzing the price history of bitcoin (BTC) across various frequencies (daily, weekly, and monthly) and conducting tests for time series stationarity in each series. As with many financial time series, the price series for bitcoin exhibits significant autocorrelation, which is consistent with a time series being non-stationary. To address this issue, the price series is transformed into a return series, which resolves much of the autocorrelation and is a more appropriate measurement for conducting statistical analysis in asset pricing.

In addition to exploring the concept of autocorrelation and time series stationarity, this project also examines the time series decomposition of the price series and explores some R packages focused on time series analysis. These include the [quantmod package](https://cran.r-project.org/package=quantmod), [xts package](https://cran.r-project.org/package=xts), [tseries package](https://cran.r-project.org/package=tseries), and [seasonal package](https://cran.r-project.org/package=seasonal). With seasonality, there are a few variations to consider. First is day-of-week seasonality, which models any recurring weekly trends. Some other approaches to modeling the seasonality are day-of-year and month-of-year seasonality. Lastly, we'll explore another aspect of seasonality around the four-year halving cycle for Bitcoin. Given that we have only seen a few full halving cycles since bitcoin price data can be reliably measured, these results are still in their infancy in regard to any statistical conclusions.

### Repository Structure

The data work for this project demo is contained in the R Notebook directory of this repository. On GitHub, the webpage within that folder should display the README.md file, which contains the compiled output of the R Notebook. If you wish to explore the source code locally, then you can open the [btc.Rmd](https://github.com/tim-dombrowski/bitcoin-timeseries-project/blob/main/R%20Notebook/btc.Rmd) file in RStudio and execute the code chunks to replicate the data work. Note the `output: html_notebook` line in the header of that file, which indicates that the R Markdown document is an R Notebook. 

After running the code chunks in RStudio and making any desired changes, you can then create a copy that will generate a copy that will appear on GitHub. To do this, save a copy of the R Notebook and name it README.Rmd. Then, change the header line for the output type to `output: github_document`. This will switch the file from being an R Notebook to an R Markdown file that will compile into a generic [Markdown](https://www.markdownguide.org/) file (.md). This format (along with the README name) will automatically be recognized by GitHub and displayed in-browser. This will also replace the Preview button with an option to Knit the Markdown file. This knitting process will re-run all the code chunks and generate a new README.md file inside of the R Notebook folder, which will display on GitHub.

### Other Bitcoin-Related Projects

For some other R Notebook project demos related to bitcoin, you can check out the following repositories:

* [bitcoin-factoranalysis-project](https://github.com/tim-dombrowski/bitcoin-factoranalysis-project) - This project focuses on applying traditional asset pricing models to bitcoin (e.g. CAPM, Fama-French, etc) and exploring some other potential factors related to the Bitcoin blockchain, such as hashrate or trading volume.

* [bitcoin-miningstock-project]() - This project explores the relationship between the stock prices of publicly traded bitcoin mining companies and the price of bitcoin, as well as other mining-related metrics.
