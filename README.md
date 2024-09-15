# data.table finance patterns

The goal of this document is to show store common financial calculations
using the `data.table` package using the latest R and data.table syntax.

#### Generate data

Generate some fake stock prices for a few tickers.

``` r
library(data.table)

set.seed(1234L)

generate_prices <- function(ticker, start_date, end_date) {
  dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "days")
  n <- length(dates)
  prices <- cumprod(1 + rnorm(n, mean = 0.0005, sd = 0.01)) * 100
  data.table(
    ticker = ticker,
    date = dates,
    price = prices
  )
}

tickers <- c("AAPL", "GOOGL", "MSFT", "AMZN")
weights <- c(0.4, 0.3, 0.2, 0.1)
start_date <- "2015-01-01"
end_date <- Sys.Date()

dt <- lapply(tickers, generate_prices, start_date, end_date) |> rbindlist()
weights <- data.table(ticker = tickers, weight = weights)
dt <- dt[weights, on = "ticker"]
head(dt)
```

       ticker       date     price weight
       <char>     <Date>     <num>  <num>
    1:   AAPL 2015-01-01  98.84293    0.4
    2:   AAPL 2015-01-02  99.16657    0.4
    3:   AAPL 2015-01-03 100.29156    0.4
    4:   AAPL 2015-01-04  97.98917    0.4
    5:   AAPL 2015-01-05  98.45866    0.4
    6:   AAPL 2015-01-06  99.00615    0.4

#### Calculate returns

In most use-cases and asset classes working with log prices is
preferred. Change the price to log prices and calculate the return.

``` r
dt <- dt |>
  setorder(ticker, date) |>
  _[, price := log(price)] |>
  _[, ret := price - shift(price), by = ticker] |>
  _[, wret := ret * weight] |>
  na.omit("ret")
head(dt)
```

       ticker       date    price weight          ret         wret
       <char>     <Date>    <num>  <num>        <num>        <num>
    1:   AAPL 2015-01-02 4.596801    0.4  0.003268944  0.001307577
    2:   AAPL 2015-01-03 4.608082    0.4  0.011280546  0.004512219
    3:   AAPL 2015-01-04 4.584857    0.4 -0.023224592 -0.009289837
    4:   AAPL 2015-01-05 4.589637    0.4  0.004779805  0.001911922
    5:   AAPL 2015-01-06 4.595182    0.4  0.005545156  0.002218062
    6:   AAPL 2015-01-07 4.589921    0.4 -0.005261216 -0.002104486

#### Calculate weekly, monthly and yearly returns

Return for each instrument:

``` r
dt[, let(week = week(date), month = month(date), year = year(date))]
ret_week <- dt[, .(ret = sum(ret)), by = .(ticker, year, week)]
ret_month <- dt[, .(ret = sum(ret)), by = .(ticker, year, month)]
ret_year <- dt[, .(ret = sum(ret)), by = .(ticker, year)]
head(ret_year)
```

       ticker  year         ret
       <char> <int>       <num>
    1:   AAPL  2015  0.23085710
    2:   AAPL  2016 -0.13173292
    3:   AAPL  2017  0.04703517
    4:   AAPL  2018  0.38669953
    5:   AAPL  2019 -0.01543271
    6:   AAPL  2020  0.40194567

Return for the portfolio:

``` r
port_ret_week <- dt[, .(ret = sum(wret)), by = .(year, week)]
port_ret_month <- dt[, .(ret = sum(wret)), by = .(year, month)]
port_ret_year <- dt[, .(ret = sum(wret)), by = year]
head(port_ret_year)
```

        year        ret
       <int>      <num>
    1:  2015 0.23301906
    2:  2016 0.06998286
    3:  2017 0.13893524
    4:  2018 0.19150558
    5:  2019 0.13820316
    6:  2020 0.30724054

#### Calculate volatility

``` r
vola <- dt[, .(daily_vola = sd(ret)), by = .(ticker, year)] |>
  _[, let(
    weekly_vola = daily_vola * sqrt(5L),
    monthly_vola = daily_vola * sqrt(21L),
    yearly_vola = daily_vola * sqrt(252L)
  )]
head(vola)
```

       ticker  year  daily_vola weekly_vola monthly_vola yearly_vola
       <char> <int>       <num>       <num>        <num>       <num>
    1:   AAPL  2015 0.009941480  0.02222982   0.04555758   0.1578161
    2:   AAPL  2016 0.010131227  0.02265411   0.04642711   0.1608282
    3:   AAPL  2017 0.009635371  0.02154534   0.04415482   0.1529568
    4:   AAPL  2018 0.009500847  0.02124454   0.04353835   0.1508213
    5:   AAPL  2019 0.010148660  0.02269309   0.04650700   0.1611050
    6:   AAPL  2020 0.010526706  0.02353843   0.04823943   0.1671063

#### TODO:

- Max/average drawdown
- Tacking error
- Portfolio risk
