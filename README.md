# data.table finance notes

The goal of this document is to document common financial calculations
using the `data.table` package using the latest `R >= 4.4` and
`data.table >= 1.16` syntax.

## Portfolio Management

#### Generate data

Generate some fake stock prices for a few tickers.

``` r
library(data.table)

set.seed(1994L)

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

generate_benchmark <- function(start_date, end_date) {
  dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "days")
  n <- length(dates)
  prices <- cumprod(1 + rnorm(n, mean = 0.0003, sd = 0.008)) * 3000
  data.table(
    ticker = "SP500",
    date = dates,
    price = prices
  )
}

ticker <- c("AAPL", "GOOGL", "MSFT", "AMZN")
start_date <- "2015-01-01"
end_date <- Sys.Date()

dt <- lapply(ticker, generate_prices, start_date, end_date) |> rbindlist()
weights <- data.table(
  ticker = ticker,
  weight = c(0.4, 0.3, 0.2, 0.1),
  country = c("USA", "USA", "USA", "USA")
)
dt <- dt[weights, on = "ticker"]
head(dt)
```

       ticker       date     price weight country
       <char>     <Date>     <num>  <num>  <char>
    1:   AAPL 2015-01-01  98.76269    0.4     USA
    2:   AAPL 2015-01-02  99.09730    0.4     USA
    3:   AAPL 2015-01-03 100.83187    0.4     USA
    4:   AAPL 2015-01-04 102.29253    0.4     USA
    5:   AAPL 2015-01-05 102.44505    0.4     USA
    6:   AAPL 2015-01-06 101.12377    0.4     USA

#### Calculate returns

``` r
dt <- dt |>
  setorder(ticker, date) |>
  _[, ret := price / shift(price) - 1, by = ticker] |>
  na.omit("ret") |>
  _[, wret := ret * weight]
head(dt)
```

       ticker       date    price weight country          ret          wret
       <char>     <Date>    <num>  <num>  <char>        <num>         <num>
    1:   AAPL 2015-01-02  99.0973    0.4     USA  0.003388113  0.0013552454
    2:   AAPL 2015-01-03 100.8319    0.4     USA  0.017503699  0.0070014795
    3:   AAPL 2015-01-04 102.2925    0.4     USA  0.014486079  0.0057944315
    4:   AAPL 2015-01-05 102.4451    0.4     USA  0.001491033  0.0005964132
    5:   AAPL 2015-01-06 101.1238    0.4     USA -0.012897486 -0.0051589943
    6:   AAPL 2015-01-07 101.9909    0.4     USA  0.008575100  0.0034300399

Alternatively, calculate the log returns:

``` r
logret <- function(x){
  x <- log(x)
  x - shift(x)
}

dt <- dt |>
  setorder(ticker, date) |>
  _[, ret := logret(price), by = ticker] |>
  na.omit("ret") |>
  _[, wret := ret * weight]
head(dt)
```

#### Calculate weekly, monthly and yearly returns

Return for each instrument:

``` r
dt[, let(week = week(date), month = month(date), year = year(date))]
ret_week <- dt[, .(ret = prod(1 + ret) - 1), by = .(ticker, year, week)]
ret_month <- dt[, .(ret = prod(1 + ret) - 1), by = .(ticker, year, month)]
ret_year <- dt[, .(ret = prod(1 + ret) - 1), by = .(ticker, year)]
head(ret_year)
```

       ticker  year         ret
       <char> <int>       <num>
    1:   AAPL  2015  0.16949691
    2:   AAPL  2016  0.13929434
    3:   AAPL  2017  0.05661279
    4:   AAPL  2018  0.21636018
    5:   AAPL  2019 -0.29360467
    6:   AAPL  2020  0.43077795

Return for the portfolio:

``` r
port_ret_week <- dt[, .(ret = prod(1 + wret) - 1), by = .(year, week)]
port_ret_month <- dt[, .(ret = prod(1 + wret) - 1), by = .(year, month)]
port_ret_year <- dt[, .(ret = prod(1 + wret) - 1), by = year]
head(port_ret_year)
```

        year         ret
       <int>       <num>
    1:  2015  0.17409548
    2:  2016  0.21111375
    3:  2017  0.13838336
    4:  2018  0.09718639
    5:  2019 -0.17371132
    6:  2020  0.05690970

#### Compare with benchmark

Calculat the benchmark return:

``` r
bmr <- generate_benchmark(start_date, end_date) |>
  setorder(date) |>
  _[, ret := price / shift(price) - 1] |>
  na.omit("ret")

port <- dt |>
  _[, .(ret = prod(1 + wret) - 1, ticker = "Portfolio"), by = date] |>
  rbind(bmr[, .(ticker, date, ret)]) |>
  setorder(ticker, date) |>
  _[, cum_ret := cumprod(1 + ret) - 1, by = ticker] |>
  _[, ticker := fifelse(ticker == "Portfolio", ticker, "Benchmark")]
```

Compare the portfolio with the benchmark performance:

``` r
library(ggplot2)

port |>
  _[between(date, "2021-01-01", max(date))] |>
  ggplot(aes(x = date, y = cum_ret, color = ticker)) +
  geom_line() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Portfolio" = "darkblue", "Benchmark" = "black")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2L)) +
  labs(title = "Performance: Portfolio vs. Benchmark")
```

![](README_files/figure-commonmark/unnamed-chunk-7-1.png)

Or turn it into a wide-format and display the performance as an area
chart:

``` r
# TODO: remove the legend with T/F and bmr/port labels
perf <- port |>
  dcast(date ~ ticker, value.var = "cum_ret") |>
  setnames(tolower) |>
  _[, diff := portfolio - benchmark]

perf |>
  _[between(date, "2021-01-01", max(date))] |>
  ggplot(aes(x = date)) +
  geom_ribbon(aes(
      ymin = pmin(portfolio, benchmark),
      ymax = pmax(portfolio, benchmark),
      fill = diff > 0
    ),
    alpha = 0.4
  ) +
  geom_line(aes(y = portfolio), color = "darkblue") +
  geom_line(aes(y = benchmark), color = "black") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2L)) +
  labs(title = "Performance: Portfolio vs. Benchmark") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )
```

![](README_files/figure-commonmark/unnamed-chunk-8-1.png)

``` r
library(gt)

perf |>
  _[date >= "2022-01-10", .(
    benchmark = last(benchmark) - first(benchmark),
    portfolio = last(portfolio) - first(portfolio)
  ), by = .(year(date))] |>
  gt() |>
  fmt_markdown() |>
  tab_header(title = "Performance: Portfolio vs. Benchmark") |>
  fmt_percent(columns = c("benchmark", "portfolio")) |>
  cols_label(year = "Year", benchmark = "Benchmark", portfolio = "Portfolio")
```

#### Analyse the portfolio exposure

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
    1:   AAPL  2015 0.009842508  0.02200852   0.04510404   0.1562450
    2:   AAPL  2016 0.010360105  0.02316590   0.04747597   0.1644616
    3:   AAPL  2017 0.009606768  0.02148139   0.04402374   0.1525027
    4:   AAPL  2018 0.010042423  0.02245554   0.04602016   0.1594185
    5:   AAPL  2019 0.010333729  0.02310692   0.04735510   0.1640429
    6:   AAPL  2020 0.009252727  0.02068973   0.04240132   0.1468825

#### TODO:

- Max/average drawdown
- Tacking error
- Portfolio risk
