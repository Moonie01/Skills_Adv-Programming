Portfolio Analysis Dashboard
================================================================================

A dashboard for analysing and visualising the performance of a portfolio of selected stocks. This project is designed to provide users with a detailed insight into performance, portfolio composition, individual stock analysis, and price simulations, using data from the S\&P 500, sourced from Yahoo Finance. 

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Table of Contents:
1. Installation
2. Usage
3. Monte Carlo Simulations
4. Contributing
5. License
6. Contact
7. Acknowledgements

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Installation
--------------------------------------------------------------------------------

Follow these steps to set up and run the Portfolio Analysis Dashboard locally.

**1.1 Prerequisites**

Ensure you have R and RStudio installed on your machine. You also need to install the following R packages if not already installed:
  
      install.packages(c("shiny", "shinydashboard", "shinydashboardPlus", "shinyFeedback", "quantmod", "dplyr", "DT", "ggplot2",   "PerformanceAnalytics", "tidyquant", "RColorBrewer", "tseries", "ggcorrplot"))

**1.2 Cloning the repo**

git clone <https://github.com/Moonie01/Skills_Adv-Programming/> cd Skills_Adv-Programming

**1.3 Running in RStudio**

First, ensure you have a working internet connection.
Second, open the app.R file in RStudio and click on the "Run App" button or use the following code in the R console:

  shiny::runApp("app.R")

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Usage
--------------------------------------------------------------------------------

This dashboard provides several features for analyzing stock performance and portfolio composition.

**2.1 Dashboard Instructions**

  2.1.1 Select Stocks: Use the sidebar to choose up to 10 stocks you want to analyze from the 10 biggest S&P 500 companies. The limit of 10 stocks is done  in order to provide a user-friendly experience. In case the user wants a broader selection from the whole index, the tool can be easily scaled by changing the following lines:
  
  - Change the length n of the filtered vector of stocks: sp500_symbols <- head(sp500_symbols, n = 10);

  - Change the number of maximum stocks accordingly: max_stocks <- 10. 

  2.1.2 Select Date Range: Define the date range for your analysis, at least 90 days for a meaningful analysis. Selecting an invalid date range, such as a start date after the end date, will give a warning. This is done for the sake of code robustness. The code also ensures robustness in case of other invalid inputs, for example, if either the start date or end date is chosen to be in the future.

  2.1.3 Tabs: Navigate through different tabs for various analyses and visualizations, which are
  
  - Dashboard Instructions: General instructions on how to use the dashboard.

  - Executive Summary: Key performance indicators and visualizations of the portfolio.

  - Portfolio Composition: Adjust the weights of each stock in your portfolio.

  - Summary Statistics: Detailed statistics of stock prices and returns.

  - Stock Analysis: In-depth analysis of individual stocks, including performance and correlation matrices.
  
  - Risk Metrics: Summary of main risk measures for the portfolio, inclusing annualized volatility, variance, standard deviation and Sharpe Ratio.
  
  - Monte Carlo Simulation: Monte Carlo simulation of stocks and portfolio price dynamics.

2.2 Key Features

  - Executive Summary: View key performance indicators such as lowest return, highest volume, highest price, and portfolio Sharpe ratio.
  
  - Portfolio Composition: Use interactive sliders to adjust stock weights and visualize the composition with a pie chart.
  
  - Summary Statistics: Examine detailed statistics of prices and returns.
  
  - Stock Analysis: Analyze historical stock prices, log-returns, and correlations among selected stocks.
  
  - Risk Metrics: Analyze annualized standard deviation, variance, and Sharpe ratio. Visualize annualized volatility in a plot with a rolling window of 1 month. 
  
  - Monte Carlo Simulation: Allows to simulate the stocks' and portfolio's performance in a unique Monte Carlo setting.

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Monte Carlo Simulations
--------------------------------------------------------------------------------

The following section will summarise the methods used in the Monte Carlo simulation of the Portfolio Analytics Dashboard.

Let's first introduce the model used to simulate stock price dynamics: the geometric Brownian motion. The simulated prices paths follows the following formula (stochastic differential equation and its analytic solution, respectively):

  (i) $dS_t = \mu S_t \, dt + \sigma S_t \, dB_t$,

  (ii) $S_t = S_0 e^{(\mu - \frac{1}{2} \sigma^2)t + \sigma B_t} > 0$,

where $S_t$ is the stock price, $\mu$ is the drift, $\sigma$ is the volatility, and $B_t$ a standard Brownian motion (i.e. a Wiener process). By definition, the Brownian motion $B_t$ is normally distributed. Its increments, $B_t - B_s$, where $t>s$, are normally distributed with mean zero and variance equal to $t-s$.

The independent Monte Carlo method used is called "rejection sampling". In involves three main components: the target density $f(x)$, the proposal density $g(x)$ and the average acceptance probability $1/M$.

1. The target density $f(x)$ is the density we want to draw from. Given that the stochastic component in the geometric Brownian motion is the standard Brownian motion, which is normally distributed, our target density $f(x)$ is Gaussian. Following, its expectation is zero and its variance is one (reflecting the one period increase, from $t-1$ to $t$, in price at each iteration).

2. The proposal density $g(x)$ is the density where we want to sample our proposal values from. In this implementation, it is a Laplace with location zero and scale one. The choice of a Laplace proposal density is due to its similarity to the normal density: $g(x)$ needs to be reasonably close to $f(x)$ for the expected number of rejections to be not too large.

3. M is the expected number of pairs that need to be sampled until the first proposed value is accepted. Following, $1/M$ is the expected acceptance probability. In this implementation, M has been chosen for the expected acceptance rate to be high enough and for the acceptance function, $a(x) = f(x) / ( g(x) * M )$ to be simple.

  (iii) $f(x) = \frac{1}{\sqrt{2\pi}} e^{-\frac{x^2}{2}}$,
  
  (iv) $g(x) = \frac{1}{2} e^{-\|x\|}$,
  
  (v) $M = \frac{e^{0.5}}{\sqrt{\pi/2}}$.

By doing some easy math, it can be shown that the acceptance function is the following:

  (iv) $a(x) = \frac{f(x)}{g(x)M} = e^{-\frac{1}{2}(|x|-1)^2}$.

The key idea between these equations is the following:
  1. Simulate with a different density g(x) called proposal,
  2. Correct to obtain a sample from the target f(x).

The rejection sampling algorithm has three main steps:
  1. Sample $U$ from a uniform distribution, $U \sim Unif(0,1)$;
  2. Plug $U$ into the inverse of the CDF of $g(x)$ to sample a proposed value, $Y$;
  3. If $U ≤ a(Y)$, set $X = Y$, i.e. accept the proposed value. Otherwise, go back to step 1.

This ensures that, on average, the accepted values have the same distribution of the target.

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Contributing
--------------------------------------------------------------------------------

We welcome contributions from the community. Follow these steps to contribute:

- Fork the repository.

- Create a new branch (git checkout -b feature/NewFeature).

- Make your changes and commit them (git commit -m 'Add some NewFeature').

- Push to the branch (git push origin feature/NewFeature).

- Create a pull request.

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

License
--------------------------------------------------------------------------------

- This project is licensed under Creative Commons.

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Contact
--------------------------------------------------------------------------------

- Lorenzo Furlani - [lorenzo.furlani\@student.unisg.ch](mailto:lorenzo.furlani@student.unisg.ch)
- Julian Dawson Kuppel - [juliandawson.kuppel\@student.unisg.ch](mailto:juliandawson.kuppel@student.unisg.ch)
- Oliver Moon - [oliver.moon\@student.unisg.ch](mailto:oliver.moon@student.unisg.ch)
- Marco Pavarino - [marco.pavarino\@student.unisg.ch](mailto:marco.pavarino@student.unisg.ch)

Project Link: https://github.com/Moonie01/Skills_Adv-Programming/

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

References and Acknowledgements
--------------------------------------------------------------------------------

- ChatGTP
- Yahoo Finance
- "Monte Carlo Methods in Financial Engineering" by Paul Glasserman (2003)
- Used Packages (see Section 1.1)
