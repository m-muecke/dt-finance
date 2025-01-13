---
title: "Finance with {data.table}"
---

Just a place to store some code snippets and notes on finance with
using the latest `data.table` package.

Load the required libraries:

``` {.r}
library(clock)
library(data.table)
library(ggplot2)
```

## Portfolio Management

#### Generate data

Generate some fake stock prices for a few tickers.

``` {.r}
set.seed(1994)

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
alloc <- data.table(
  ticker = ticker,
  weight = c(0.4, 0.3, 0.2, 0.1),
  country = c("USA", "USA", "USA", "USA")
)
dt <- dt[alloc, on = "ticker"]
head(dt)
```
|ticker|date|price|weight|country|
|---|---|--:|--:|---|
|AAPL|2015-01-01| 98.763|0.4|USA|
|AAPL|2015-01-02| 99.097|0.4|USA|
|AAPL|2015-01-03|100.832|0.4|USA|
|AAPL|2015-01-04|102.293|0.4|USA|
|AAPL|2015-01-05|102.445|0.4|USA|
|AAPL|2015-01-06|101.124|0.4|USA|


TODO: holdings table current date: name, total value, abs. and relative change in value (from start), relative weight
TODO: doughnut chart of portfolio composition

#### Calculate returns

``` {.r}
logret <- function(x) {
  x <- log(x)
  x - shift(x)
}

dt <- dt |>
  setorder(ticker, date) |>
  _[, let(ret = price / shift(price) - 1, log_ret = logret(price)), by = ticker] |>
  na.omit("ret") |>
  _[, let(wret = ret * weight, value = price * weight)]
head(dt)
```
|ticker|date|price|weight|country|ret|log_ret|wret|value|
|---|---|--:|--:|---|--:|--:|--:|--:|
|AAPL|2015-01-02| 99.097|0.4|USA| 0.003| 0.003| 0.001|39.639|
|AAPL|2015-01-03|100.832|0.4|USA| 0.018| 0.017| 0.007|40.333|
|AAPL|2015-01-04|102.293|0.4|USA| 0.014| 0.014| 0.006|40.917|
|AAPL|2015-01-05|102.445|0.4|USA| 0.001| 0.001| 0.001|40.978|
|AAPL|2015-01-06|101.124|0.4|USA|-0.013|-0.013|-0.005|40.450|
|AAPL|2015-01-07|101.991|0.4|USA| 0.009| 0.009| 0.003|40.796|


``` {.r}
dt |>
  _[date >= add_months(end_date, -12L), .(value = sum(value)), by = date] |>
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_blank()
  ) +
  labs(title = "Portfolio Value")
```
![](<README__files/chunk-4-1.png>)

#### Calculate weekly, monthly and yearly returns

Return for each instrument:

``` {.r}
ret_week <- dt[, .(ret = prod(1 + ret) - 1), by = .(ticker, year(date), week(date))]
ret_month <- dt[, .(ret = prod(1 + ret) - 1), by = .(ticker, yearmon(date))]
ret_year <- dt[, .(ret = prod(1 + ret) - 1), by = .(ticker, year(date))]
head(ret_year)
```
|ticker|year|ret|
|---|--:|--:|
|AAPL|2015| 0.169|
|AAPL|2016| 0.139|
|AAPL|2017| 0.057|
|AAPL|2018| 0.216|
|AAPL|2019|-0.294|
|AAPL|2020| 0.431|


Return for the portfolio:

``` {.r}
port_ret_week <- dt[, .(ret = prod(1 + wret) - 1), by = .(year(date), week(date))]
port_ret_month <- dt[, .(ret = prod(1 + wret) - 1), by = .(yearmon(date))]
port_ret_year <- dt[, .(ret = prod(1 + wret) - 1), by = year(date)]
head(port_ret_year)
```
|year|ret|
|--:|--:|
|2015| 0.217|
|2016| 0.147|
|2017| 0.089|
|2018| 0.098|
|2019|-0.212|
|2020| 0.168|


#### Compare performance with a benchmark

Calculat the benchmark return:

``` {.r}
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

``` {.r}
port |>
  _[date > "2021-01-01"] |>
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
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  labs(title = "Cumulative Return: Portfolio vs. Benchmark")
```
![](<README__files/chunk-8-1.png>)

Or turn it into a wide-format and display the performance as an area chart:

``` {.r}
perf <- port |>
  dcast(date ~ ticker, value.var = "cum_ret") |>
  setnames(tolower) |>
  _[, diff := portfolio - benchmark]

perf |>
  _[date > "2021-01-01"] |>
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(
      ymin = pmin(portfolio, benchmark),
      ymax = pmax(portfolio, benchmark),
      fill = diff > 0
    ),
    alpha = 0.4,
    show.legend = FALSE
  ) +
  geom_line(aes(y = portfolio, color = "Portfolio")) +
  geom_line(aes(y = benchmark, color = "Benchmark")) +
  scale_color_manual(values = c("Portfolio" = "darkblue", "Benchmark" = "black")) +
  scale_fill_manual(values = c("TRUE" = "#00A651", "FALSE" = "#FF0000")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2L)) +
  labs(title = "Cumulative Return: Portfolio vs. Benchmark") +
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
  )
```
![](<README__files/chunk-9-1.png>)

``` {.r}
perf |>
  _[date >= "2022-01-10", .(
    benchmark = last(benchmark) - first(benchmark),
    portfolio = last(portfolio) - first(portfolio)
  ), by = .(year(date))]
```

#### Analyse the portfolio exposure



#### Calculate volatility

Note this is scaling volatility by $\sqrt{h}$, which has some shortcomings,
see for example
[Diebold et.al. (1996)](https://www.sas.upenn.edu/~fdiebold/papers/paper18/dsi.pdf&ved=2ahUKEwjM2P-7jfGKAxUkBdsEHcrTCAkQFnoECBcQAQ&usg=AOvVaw36skVdLjP1SwTgB6J1rdnz).


``` {.r}
vola <- dt[, .(daily_vola = sd(log_ret)), by = .(ticker, year(date))] |>
  _[, let(
    weekly_vola = daily_vola * sqrt(5),
    monthly_vola = daily_vola * sqrt(21),
    yearly_vola = daily_vola * sqrt(252)
  )]
head(vola)
```
|ticker|year|daily_vola|weekly_vola|monthly_vola|yearly_vola|
|---|--:|--:|--:|--:|--:|
|AAPL|2015|0.010|0.022|0.045|0.156|
|AAPL|2016|0.010|0.023|0.047|0.164|
|AAPL|2017|0.010|0.021|0.044|0.152|
|AAPL|2018|0.010|0.022|0.046|0.159|
|AAPL|2019|0.010|0.023|0.047|0.164|
|AAPL|2020|0.009|0.021|0.042|0.147|


#### Portfolio risk

Portfolio risk is defined as:

$$
\sigma_p = \sqrt{w^T \Sigma w}
$$

``` {.r}
wgt <- alloc$weight
cov_mat <- dt |>
  dcast(date ~ ticker, value.var = "log_ret") |>
  _[, date := NULL] |>
  cov(use = "pairwise.complete.obs")
port_risk <- as.numeric(sqrt(t(wgt) %*% cov_mat %*% wgt))
port_risk
```

```
#> [1] 0.005584329
```

#### Drawdown

Maximum Drawdown is defined as follows:

$$
MDD = \max_{i \leq j} \left( \frac{V_j - V_i}{V_i} \right)
$$

Instrument drawdown:

``` {.r}
drawdown <- copy(dt) |>
  _[, cum_ret := cumprod(1 + ret) - 1, by = ticker] |>
  _[, drawdown := (cum_ret - cummax(cum_ret)), by = ticker]
head(drawdown)
```
|ticker|date|price|weight|country|ret|log_ret|wret|value|cum_ret|drawdown|
|---|---|--:|--:|---|--:|--:|--:|--:|--:|--:|
|AAPL|2015-01-02| 99.097|0.4|USA| 0.003| 0.003| 0.001|39.639|0.003| 0.000|
|AAPL|2015-01-03|100.832|0.4|USA| 0.018| 0.017| 0.007|40.333|0.021| 0.000|
|AAPL|2015-01-04|102.293|0.4|USA| 0.014| 0.014| 0.006|40.917|0.036| 0.000|
|AAPL|2015-01-05|102.445|0.4|USA| 0.001| 0.001| 0.001|40.978|0.037| 0.000|
|AAPL|2015-01-06|101.124|0.4|USA|-0.013|-0.013|-0.005|40.450|0.024|-0.013|
|AAPL|2015-01-07|101.991|0.4|USA| 0.009| 0.009| 0.003|40.796|0.033|-0.005|


Portfolio drawdown:

``` {.r}
drawdown <- dt |>
  _[, .(wret = sum(wret)), by = date] |>
  _[, cum_ret := cumprod(1 + wret) - 1] |>
  _[, drawdown := (cum_ret - cummax(cum_ret))]
head(drawdown)
```
|date|wret|cum_ret|drawdown|
|---|--:|--:|--:|
|2015-01-02| 0.003|0.003| 0.000|
|2015-01-03| 0.014|0.017| 0.000|
|2015-01-04| 0.006|0.023| 0.000|
|2015-01-05| 0.004|0.027| 0.000|
|2015-01-06|-0.006|0.021|-0.006|
|2015-01-07| 0.004|0.025|-0.003|


``` {.r}
drawdown[drawdown < 0, .(min_drawdown = min(drawdown), avg_drawdown = mean(drawdown))]
```
|min_drawdown|avg_drawdown|
|--:|--:|
|-0.574|-0.146|


#### TODO:

- Tacking error
