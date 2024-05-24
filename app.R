################################################################################
########       Project for Skills: Advanced Programming Languages       ########
################################################################################

### Authors:
# Lorenzo Furlani
# Julian Dawson Kuppel
# Oliver Moon
# Marco Pavarino

# Load required packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyFeedback)
library(quantmod)
library(dplyr)
library(DT)
library(ggplot2)
library(PerformanceAnalytics)
library(tidyquant)
library(RColorBrewer)
library(tseries)
library(ggcorrplot)
library(shinyjs)



################################################################################
##### USER Code Instructions

# -> this code implements an interactive dashboard
# -> the dashboard is specialized to conduct a Financial Portfolio Analysis
# -> make sure that you have an internet connection
# -> install all the packages required (see above) and click on "Run App"
# -> the dashboard will contain an 'instruction' page to explain further details


################################################################################
##### Prepare the data

# Fetch the list of S&P 500 stock tickers (symbols)
# The data for these symbols will later be fetched on Yahoo Finance
sp500_symbols <- tq_index("SP500") %>% select(symbol) %>% pull

# Drop 'stocks' in list that are not stocks
sp500_symbols <- sp500_symbols[sp500_symbols != '-']
sp500_symbols <- sp500_symbols[sp500_symbols != '2443527D']

# extract the companies of the S&P 500 with the highest index weights
sp500_symbols <- head(sp500_symbols, n = 10)

# Sort in alphabetical order
sp500_symbols = sort(sp500_symbols)


################################################################################
##### Define User Interface (UI)

ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Analysis Dashboard"),
  
  # define the interactive sidebar of the dashboard
  dashboardSidebar(
    # Java Script Snippet to prevent user from inserting invalid date inputs (this option is not currently supported by R shiny itself) 
    tags$head(
      tags$script(
        '$(document).ready(function() {
        $("#dateRange input").attr("readonly", "readonly"); 
      });'
      )
    ),
    sidebarMenu(
      # create the tabs and sub-tabs in the sidebar
      menuItem("Dashboard Instructions", tabName = "instructions", icon = icon("info-circle")),
      menuItem("Executive Summary", tabName = "executive_summary", icon = icon("square-poll-vertical")),
      menuItem("Portfolio Composition", tabName = "portfolio_comp", icon = icon("pie-chart")),
      menuItem("Summary Statistics", tabName = "summary_statistics", icon = icon("table")),
      menuItem("Stock Analysis", tabName = "individual_stock_analysis", icon = icon("bar-chart"), 
               menuSubItem("Stock Performance", tabName = "stock_perf"),
               menuSubItem("Correlation Matrix", tabName = "correlation_matrix")),
      menuItem("Risk Metrics", tabName = "risk_metrics", icon = icon("exclamation-triangle")), 
      menuItem("Monte Carlo Simulation", tabName = "monte_carlo", icon = icon("dice"))
    ),
    # provide the user to select the input
    # here: the user can select any of the ten biggest stocks in the S&P500 index and the time range
    selectInput("stocks", "Select Portfolio Stocks", choices = sp500_symbols, selected = c("AAPL", "MSFT"), multiple = TRUE), # default are Apple and Microsoft
    # let the user choose the time period considered for the stock analysis; as default the time horizon is one year, while the end of the time period is the current date
    dateRangeInput("dateRange", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date(), min = "2010-01-01", max = Sys.Date()), # ensures that the user cannot select a future date (ensuring robustness)
    # show a message to the user that the chosen start date is after the end data (not valid)
    useShinyFeedback()
  ),
  
  # defining the main body of the dashboard
  dashboardBody(
    tags$head(
      # fine-tuning the aesthetics of the pages (colors etc.)
      tags$style(HTML("
      body, .content-wrapper {
        font-family: 'Times New Roman', Georgia, serif !important;
      }
      .main-header .logo {
        background-color: #003366 !important;
        color: #FFFFFF !important;
        font-family: 'Georgia Pro', Georgia, serif !important;
      }
      .main-header .navbar {
        background-color: #003366 !important;
        color: #FFFFFF !important;
      }
      .skin-blue .main-header .navbar .sidebar-toggle {
        color: #FFFFFF !important;
      }
      .skin-blue .main-header .logo:hover, .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background-color: #002244 !important;
      }
      .box {
        border: none !important;  /* Remove border */
      }
      .box-header {
        background-color: #a0bfde !important; /* Navy blue */
        color: #FFFFFF !important;
      }
      .plot-container {
        background-color: #4A4A4A !important;
        color: #FFFFFF !important;
      }
      .plot-title, .plot-axis-title {
        color: #FFFFFF !important;
      }
      .ggplot2 {
        font-family: 'Times New Roman', Georgia, serif !important;
        color: #FFFFFF !important;
      }
      .value-box-custom .small-box {
        border-radius: 15px; /* Rounded corners */
        box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1);
      }
      /* Adjust sidebar elements when collapsed */
      .sidebar-mini.sidebar-collapse .sidebar-menu>li>.treeview-menu {
        display: none !important;
      }
      .sidebar-mini.sidebar-collapse .selectize-input {
        width: 0 !important;
        padding: 0 !important;
      }
      .sidebar-mini.sidebar-collapse .selectize-dropdown {
        display: none !important;
      }
      .sidebar-mini.sidebar-collapse .form-group {
        display: none !important;
      }
    "))
    ),
    # Defining the layout for each tab individually
    # Important!: most tools here are only referenced and implemented in the server part (see below)
    tabItems(
      # first of all, we define the 'Instructions' Tab
      tabItem(tabName = "instructions",
              fluidRow(
                box(title = "Dashboard Instructions", width = 12, status = "primary", solidHeader = TRUE,
                    # adding the instructions for a user-friendly experience
                    p("Welcome to the", strong("Portfolio Analysis Dashboard!"), "This dashboard allows you to analyze and visualize the performance of selected stocks from the S&P500."),
                    p("Use the sidebar to select the stocks you want to analyze and the date range for the analysis. You can go back as far as 2010. Please select a date range of at least 90 days for meaningful analysis."),
                    p("Tabs include various analyses and visualizations such as stock price development, portfolio composition, individual stock performance, historical returns, correlation matrix, summary tables, portfolio returns, risk metrics, and Monte Carlo Simulations."),
                    tags$ul(
                      tags$li(tags$i("Executive Summary"), ": You can check the assigned composition of your portfolio and its cumulative returns."),
                      tags$li(tags$i("Portfolio Composition"), ": You can use the sliders to manually adjust the dollar amount invested for each stock in your portfolio. The weights will automatically sum up to 1."),
                      tags$li(tags$i("Summary Statistics"), ": You can check for the main statistical attributes of your selected stocks."),
                      tags$li(tags$i("Stock Analysis"), ": You can check for individual stock performance and correlation matrices for your selected stocks. You can:"),
                      tags$ul(
                        tags$li("Select which stocks from your chosen portfolio you want to graphically analyze."),
                        tags$li("View historical performance of individual stocks through prices and returns."),
                        tags$li("Compare the correlation between different stocks.")
                      ),
                      tags$li(tags$i("Risk Metrics"), ": You can check the annualized standard deviation, variance, and Sharpe ratio of your portfolio, as well as a graphical representation of its annualized volatility."),
                      tags$li(tags$i("Monte Carlo Simulation"), ": You can run a Monte Carlo simulation to assess stock and portfolio price dynamics. The simulation will provide a graphical representation and tables with summary statistics and quantiles reporting")
                    ),
                    p(''),
                    p(strong("Overview of Monte Carlo Simulation:")),
                    p("The most important feature of this dashboard is the possibility to run Monte Carlo (MC) simulations to estimate stock and portfolio return statistics. The simulation generates 500 one-year paths for each stock and the entire portfolio, providing valuable insights into potential future performance. The process is outlined as follows:"),
                    tags$ul(
                      tags$li(strong("Simulating Stock Prices:"),
                              p("The simulation employs the 'Geometric Brownian Motion' (GBM) model to capture stock price dynamics. GBM combines (i) the initial stock price, (ii) the average growth rate (drift), and (iii) the randomness of the fluctuations (volatility) to simulate the evolution of prices.")),
                      tags$li(strong("Incorporating Randomness:"),
                              p("Randomness is introduced through a standard Brownian motion component in the GBM, representing the inherent uncertainty in stock price movements. This component helps to generate multiple potential independent future paths for each stock price.")),
                      tags$li(strong("Rejection Sampling Technique:"),
                              p("To ensure the accuracy and independence of the simulated paths, the independent MC method of 'rejection sampling' is used. In this technique, proposed random fluctuations are evaluated based on an acceptance criterion. Only those fluctuations that meet the criterion are retained, ensuring that the simulated paths adhere to the GBM model.")),
                      tags$li(strong("Simulation Steps for each Lag of a Path:"),
                              tags$ol(
                                tags$li("Propose a Fluctuation: Generate an initial estimate for the random fluctuation."),
                                tags$li("Evaluate the Fit: Assess the reasonableness of this estimate based on predefined criteria."),
                                tags$li("Accept or Reject: Accept the estimate if it aligns with the model's criteria; otherwise, propose a new estimate."),
                                tags$li("Repeat: Iterate this process to create a complete path of future stock prices.")
                              )),
                      p(''),
                      tags$li(strong("Benefits of Monte Carlo Simulations:"),
                              p("MC simulations provide a comprehensive view of possible future outcomes based on various random changes in stock prices. This approach helps in understanding the range of potential future prices and associated risks.")),
                      tags$li(strong("Visual Representation:"),
                              p("The simulation outputs a graph displaying 100 independent paths of portfolio values, each representing a different potential scenario.")),
                      tags$li(strong("Summary Statistics:"),
                              p("The simulation also generates tables with key summary statistics for both the stock prices and for the portfolio, including average returns and stock price quantiles one year into the future.")),
                      p("NB: More details on the MC simulations can be found in the READ_ME file.")
                    )
                )
              )
      ),
      # Secondly, we show an overview of different plots and statistics to the user
      tabItem(tabName = "executive_summary",
              # define the first row in the page layout using the fluidRow() function
              fluidRow(
                # add four widgets to the tab containing metrics about the stocks
                div(class = "value-box-custom", valueBoxOutput("lowest_return", width = 3)),
                div(class = "value-box-custom", valueBoxOutput("highest_price", width = 3)),
                div(class = "value-box-custom", valueBoxOutput("highest_weight", width = 3)),
                div(class = "value-box-custom", valueBoxOutput("sharpe_ratio", width = 3))
              ),
              # add two plots in the same row, i.e. next to each other
              # create the layout accordingly using the column() function
              # important! the width of the columns should add up to 12 (here 6 each)
              fluidRow(
                column(width = 6,
                       # define title, width, status (background color), solidHeader (background color of the header), height
                       # plotOutput() displays the content of the passed parameter (that is defined in the server part later)
                       # a similar logic applies to all the other boxes
                       box(title = "Portfolio Composition", width = 12, status = "primary", solidHeader = TRUE,
                           plotOutput("donutChart", height = 500)) 
                ),
                column(width = 6,
                       box(title = "Cumulative Portfolio Returns", width = 12, status = "primary", solidHeader = TRUE,
                           plotOutput("CumulativeReturnsPlot", height = 500)) 
                )
              )
      ),
      # This tab defines the layout of the portfolio composition (selecting dollar amount to invest)
      tabItem(tabName = "portfolio_comp",
              fluidRow(
                box(title = "Portfolio Composition", width = 12, status = "primary", solidHeader = TRUE,
                    # provide the user with a slider where they can input the amount of money invested in each stock
                    # slider is implemented in the server part
                    uiOutput("sliders"),
                    tableOutput("weights_tab"),
                    # plot weights of the portfolio in a pie chart
                    tags$div(class = "plot-container", plotOutput("pieChart", height = 500))
                )
              )
      ),
      # This tab defines the layout for the summary statistics
      tabItem(tabName = "summary_statistics",
              fluidRow(
                # three widgests 
                div(class = "value-box-custom", valueBoxOutput("highest_return", width = 4)),
                div(class = "value-box-custom", valueBoxOutput("most_trending", width = 4)),
                div(class = "value-box-custom", valueBoxOutput("highest_volatility", width = 4))
              ),
              # statistics table for prices and returns 
              fluidRow(
                box(title = "Summary Statistics of Prices", status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("price_statistics")
                ),
                box(title = "Summary Statistics of Returns", status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("return_statistics")
                )
              )
      ),
      # The next two items are sub tabs for the Stock Analysis tab
      # This tab defines the layout for the individual stock performance
      tabItem(tabName = "stock_perf",
              fluidRow(
                column(12, uiOutput("dynamicStockSelectors")),
                column(12, actionButton("addStock", "Add Stock", icon = icon("plus")), br(), br())
              ),
              fluidRow(
                box(title = "Historical Stock Price", width = 12, status = "primary", solidHeader = TRUE,
                    tags$div(class = "plot-container", plotOutput("indivStockPlot", height = 500))),
                box(title = "Historical Log-Returns", width = 12, status = "primary", solidHeader = TRUE,
                    tags$div(class = "plot-container", plotOutput("returnsPlot", height = 500))),
                box(title = "Density of Log-Returns", width = 12, status = "primary", solidHeader = TRUE,
                    tags$div(class = "plot-container", plotOutput("densityPlot", height = 500)))
              )
      ),
      # This tab defines the layout for the correlation plot
      tabItem(tabName = "correlation_matrix",
              fluidRow(
                box(title = "Correlation Matrix", width = 12, status = "primary", solidHeader = TRUE,
                    tags$div(class = "plot-container", plotOutput("correlationMatrixPlot", height = 500)))
              )
      ),
      # This tab defines the layout for the risk metrics 
      tabItem(tabName = "risk_metrics",
              fluidRow(
                div(class = "value-box-custom", valueBoxOutput("Annualized_Std", width = 4)),
                div(class = "value-box-custom", valueBoxOutput("Annualized_Variance", width = 4)),
                div(class = "value-box-custom", valueBoxOutput("Annualized_sharpe_ratio", width = 4))
              ),
              # plot for annualized volatility 
              fluidRow(
                box(title = "Volatility over Time", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("volatility_plot", height = 500)
                )
              )
      ),
      # This tab defines the layout for the Monte Carlo simulation 
      tabItem(tabName = "monte_carlo",
              fluidRow(
                column(12, actionButton("run_mc", "Run MC Simulation", icon = icon("play-circle")), br(), br())
              ),
              fluidRow(
                box(title = "Monte Carlo Simulation Paths", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("mc_plot", height = 500))
              ),
              fluidRow(
                box(title = "Summary Statistics", width = 6, status = "primary", solidHeader = TRUE,
                    DTOutput("mc_summary_table")),
                box(title = "Quantiles", width = 6, status = "primary", solidHeader = TRUE,
                    DTOutput("mc_quantiles_table"))
              )
      )
    )
  ),
  # header color 
  skin = "blue"
)

################################################################################
##### Define Server Logic

server <- function(input, output, session) {
  
  
  ##############################################################################
  ##### Global Settings and Parameters for Server Functionality
  
  ### Set a maximum number of stocks for the Portfolio
  max_stocks <- 10
  
  ### Limit the number of selected stocks to max_stocks
  # show a message that tells the user that the maximum number of selected stocks is exceeded
  observeEvent(input$stocks, {
    if (length(input$stocks) > max_stocks) {
      showModal(modalDialog(
        title = "Selection Limit Exceeded",
        paste("You can only select up to ", max_stocks, "stocks. Premium features coming soon!"),
        easyClose = TRUE,
        footer = NULL
      ))
      
      # reset the input to the first max_stocks selected stocks
      updateSelectInput(session, "stocks", selected = input$stocks[1:max_stocks])
    }
  })
  
  ### Reactive values to store the weights
  # these weights are re-evaluated depending on the user input, leading to dynamic adjustment of the dashboard
  # Important: the variable does not store normalized weights, but the investment amount.
  # These inputs are then later normalized to be used as weights
  weights <- reactiveValues()
  
  ### Initialize investment to be 10,000
  observe({
    num_stocks <- length(input$stocks) # get number of stocks
    if (num_stocks > 0) { 
      for (stock in input$stocks) {
        weights[[stock]] <- 10000 # initialize the investment in each stock to 10,000
      }
    }
  })
  
  # Ensure that the user selects a valid and suitable time frame
  observeEvent(input$dateRange, {
    # ensure a valid input, i.e. start date before end date
    if (input$dateRange[1] > input$dateRange[2]) {
      hideFeedback("dateRange")
      showFeedbackDanger(
        inputId = "dateRange",
        text = "Start date cannot be after end date."
      )
      # ensure that the time period chosen is long enough for a meaningful analysis
    } else if (as.numeric(difftime(input$dateRange[2], input$dateRange[1], units = "days")) < 90) {
      hideFeedback("dateRange")
      showFeedbackWarning(
        inputId = "dateRange",
        text = "Date range must be at least 90 days."
      )
    } else {
      hideFeedback("dateRange")
    }
  })
  
  
  ##############################################################################
  ##### Helper Functions
  
  ### General Helper Functions #################################################
  
  ### Sum of all the weights (investments)
  sumWeights <- reactive({
    sum(sapply(input$stocks, function(stock) weights[[stock]]))
  })
  
  ### Normalize the weights (investments), scale 0-1
  normWeights <- reactive({
    sum_weights <- sum(sapply(input$stocks, function(stock) weights[[stock]]))
    weights_norm <- sapply(input$stocks, function(stock) weights[[stock]] / sum_weights)
  })
  
  ### Fetch stock data from all selected stocks from Yahoo Finance
  stockData <- reactive({
    # validate the input of the user
    validate(
      # show a message to the user in case the user did not select any stock (message is displayed in each box)
      need(length(input$stocks) > 0, "Please select at least one stock."),
      # show a message to the user in case the of an invalid date range, i.e. start date after end date
      need(input$dateRange[1] <= input$dateRange[2], "Start date must be before the end date.")
    )
    # for a valid date range, ensure that the time period chosen is sufficiently long for a meaningful analysis
    if (input$dateRange[1] <= input$dateRange[2]) {
      validate(
        # show a message to the user that the selected time period is too short for a reasonable analysis
        need(as.numeric(difftime(input$dateRange[2], input$dateRange[1], units = "days")) >= 90, "Date range must be at least 90 days for a meaningful analysis of historic prices.")
      )
    }
    lapply(input$stocks, function(stock) {
      # retrieve the stock data in the specified time range by the user for each stock
      getSymbols(stock, src = "yahoo", from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE)
    })
  })
  
  ### Generate a distinct color palette (using the RColorBrewer Package- Paired)
  # depending on the number of selected stocks, the color palette gets extended
  getColors <- reactive({
    num_colors <- length(input$stocks)
    brewer.pal(max(3, min(num_colors, 12)), "Paired")
  })
  
  
  ## Calculate summary statistics for the widgets
  summary_stats <- reactive({
    # get reactive data (stocks and weights)
    stocks <- stockData()
    weights_normalized <- normWeights()
    
    # get returns of the price series (using Cl() to get the stock's closing price)
    # we calculate log-returns in order to be able to sum them later (exploit properties of log-returs)
    returns <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock), type = "log")))
    colnames(returns) <- input$stocks
    
    # Highest return
    total_returns <- colSums(returns, na.rm = TRUE)
    highest_return_stock <- names(total_returns[which.max(total_returns)]) # retrieve the stock name with the highest return in the time period considered
    
    # Lowest return
    lowest_return_stock <- names(total_returns[which.min(total_returns)]) # retrieve the stock name with the lowest return in the time period considered
    
    # Highest price
    prices <- do.call(merge, lapply(stocks, function(stock) Cl(stock))) #  get closing prices
    colnames(prices) <- input$stocks
    last_prices <- sapply(prices, function(price) as.numeric(last(price))) # get last price
    highest_price_stock <- names(last_prices[which.max(last_prices)])
    
    # Highest weight
    highest_weight_stock <- names(weights_normalized[which.max(weights_normalized)])
    
    # Highest volatility
    volatility <- apply(returns, 2, sd, na.rm = TRUE) # compute the standard deviation as a proxy for volatiliy
    highest_volatility_stock <- names(volatility[which.max(volatility)])
    
    # Sharpe ratio (portfolio level)
    returns_sharpe <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock)))) # using arithmetic returns (non-continuous)
    colnames(returns_sharpe) <- input$stocks
    # annualize the mean and variance of returns (scaling by 252 which is the number of trading days within a year)
    T <- 252
    mu_stocks <- colMeans(returns_sharpe, na.rm = TRUE) * T  # mean stocks returns
    sigma2_stocks <- cov(returns_sharpe) * T                 # stocks variance
    mu_PF <- mu_stocks %*% weights_normalized                # portfolio returns
    sigma2_PF <- t(weights_normalized) %*% sigma2_stocks %*% weights_normalized # portfolio variance
    sharpe_ratio <- mu_PF / sqrt(sigma2_PF) # compute Sharpe ratio
    
    # Most trending stock (highest return in the past 10 days)
    recent_returns <- tail(returns, 10)
    recent_cum_returns <- colSums(recent_returns, na.rm = TRUE)
    most_trending_stock <- names(recent_cum_returns[which.max(recent_cum_returns)])
    
    # create a list with the results
    list(
      highest_return_stock = highest_return_stock,
      lowest_return_stock = lowest_return_stock,
      highest_price_stock = highest_price_stock,
      highest_weight_stock = highest_weight_stock,
      highest_volatility_stock = highest_volatility_stock,
      sharpe_ratio = round(sharpe_ratio, 4),
      most_trending_stock = most_trending_stock)
  })
  
  
  ### Helper Functions: for tab "individual_stock_analysis": ###################
  
  # Reactive values to store the dynamic stock selectors
  stockSelectors <- reactiveValues(selectors = list(), counter = 1)
  
  # Function to get selected stocks
  updateSelectedStocks <- reactive({
    stocks <- stockData() # used to validate the input of the user
    selected_stocks <- unlist(lapply(names(stockSelectors$selectors), function(selector) input[[selector]]))
    selected_stocks <- selected_stocks[selected_stocks != ""]
    selected_stocks
  })
  
  # Helper function to update selectors UI (user can delete stocks interactively)
  # Add the functionality (selecterUI), that the user can delete stocks via a button
  updateSelectorsUI <- function() {
    output$dynamicStockSelectors <- renderUI({
      selectorUI <- lapply(names(stockSelectors$selectors), function(selector) {
        # check if the selector is the default one
        if (selector == "stockSelector1") {
          fluidRow(
            column(10, selectInput(selector, "Select Stock", choices = input$stocks, selected = input[[selector]]))
          )
          # otherwise, add "delete" feature for the rest
        } else {
          fluidRow(
            # determine the position of the user input tool
            column(10, selectInput(selector, "Select Stock", choices = input$stocks, selected = input[[selector]])),
            column(2, style = "margin-top: 29px;", actionButton(paste0("delete_", selector), label = "Delete", icon = icon("trash")))
          )
        }
      })
      do.call(tagList, selectorUI)
    })
    
    # Disable add button if maximum number of selectors is reached
    if (length(stockSelectors$selectors) >= length(input$stocks)) {
      disable("addStock") # the button is disabled when the maximum number of stocks is reached 
    } else {
      enable("addStock")
    }
  }
  
  # Add initial stock selector
  # the first stock shown as default is the first stock in the portfolio
  observe({
    if (length(stockSelectors$selectors) == 0) {
      stockSelectors$selectors[["stockSelector1"]] <- selectInput("stockSelector1", "Select Stock", choices = input$stocks, selected = input$stocks[1])
      updateSelectorsUI()
    }
  })
  
  # Add new stock selector when a new stock is added
  observeEvent(input$addStock, {
    if (length(stockSelectors$selectors) < length(input$stocks)) {
      # Store the current selections before adding
      current_selections <- lapply(names(stockSelectors$selectors), function(sel) input[[sel]])
      names(current_selections) <- names(stockSelectors$selectors)
      
      # Increment the counter to ensure unique IDs
      stockSelectors$counter <- stockSelectors$counter + 1
      new_selector_id <- paste0("stockSelector", stockSelectors$counter)
      
      # Add new selector
      stockSelectors$selectors[[new_selector_id]] <- selectInput(new_selector_id, "Select Stock", choices = input$stocks, selected = NULL)
      updateSelectorsUI()
      
      # Restore previous selections of the stocks shown
      lapply(names(stockSelectors$selectors), function(sel) {
        updateSelectInput(session, sel, selected = current_selections[[sel]])
      })
    } else {
      showNotification("Not possible to add stocks because the maximum has been reached.", type = "warning", duration = 4)
    } # Warning for maximum reached 
  })
  
  # Handle deleting stock selectors
  observe({
    lapply(names(stockSelectors$selectors), function(selector) {
      observeEvent(input[[paste0("delete_", selector)]], {
        # Store the current selections before deleting
        current_selections <- lapply(names(stockSelectors$selectors), function(sel) input[[sel]])
        names(current_selections) <- names(stockSelectors$selectors)
        
        # Delete the selected stock
        stockSelectors$selectors[[selector]] <- NULL
        
        # Remove NULL elements and update UI
        stockSelectors$selectors <- stockSelectors$selectors[!sapply(stockSelectors$selectors, is.null)]
        
        # Update the selectors UI
        updateSelectorsUI()
        
        # Restore previous selections
        lapply(names(stockSelectors$selectors), function(sel) {
          updateSelectInput(session, sel, selected = current_selections[[sel]])
        })
      })
    })
  })
  
  # Observe changes in the portfolio stocks and update all selectors accordingly
  observeEvent(input$stocks, {
    updateSelectorsUI()
  })
  
  
  ### Helper Functions: for tab "portfolio_comp": ##############################
  
  ### Generate sliders and input boxes dynamically
  output$sliders <- renderUI({
    if (length(input$stocks) <= max_stocks) { # add sliders up until the max number of stocks allowed
      sliderInputs <- lapply(input$stocks, function(stock) {
        fluidRow(
          column(12,
                 sliderInput(inputId = paste0("investment_", stock), 
                             label = paste0("Investment into ", stock), 
                             min = 0, max = 100000, value = weights[[stock]], step = 1, pre = "$", width = "100%"))
        )
      })
      do.call(tagList, sliderInputs)
    }
  })
  
  ### Observe changes in each weight slider and update the weights
  observe({
    lapply(input$stocks, function(stock) {
      # if weight for any stock has changed
      observeEvent(input[[paste0("investment_", stock)]], {
        isolate({
          # update the weights
          weights[[stock]] <- input[[paste0("investment_", stock)]]
        })
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    })
  })
  
  ### Helper Functions: for tab "risk_metrics": ################################
  
  # Calculate weighted returns for the portfolio
  portfolio_returns <- reactive({
    stocks <- stockData() # shows a warning to the user when no stock is selected
    weights_normalized <- normWeights() # get the normalized weights
    returns <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock)))) # get returns via closing prices
    colnames(returns) <- input$stocks # name the columns w.r.t the stock symbols
    weighted_returns <- returns %*% weights_normalized # compute weighted returns
    return(weighted_returns)
  })
  
  ##############################################################################
  ###### Tab Implementation
  
  ##############################################################################
  ##### Tab "executive_summary"
  
  #### OUTPUT
  
  ### Cumulative returns plot for "summary_plots"
  output$donutChart <- renderPlot({
    stocks <- stockData() # try to import stock data (throws a warning message to the user when no stock is selected)
    req(input$stocks) # check that required input is available
    weights_normalized <- normWeights() # use helper function to normalize the weights
    donut_data <- data.frame( # create date set for plotting the portfolio composition
      Stock = input$stocks,
      Weight = weights_normalized)
    
    # plotting a pie chart displaying the split of the portfolio using ggplot()
    ggplot(donut_data, aes(x = 2, y = Weight, fill = Stock)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") + # transforming the plot to a polar coordinate system
      xlim(0.5, 2.5) + # creates the hole in the middle of the pie chart
      theme_void() +
      scale_fill_brewer(palette = "PuRd") +
      theme(legend.position = "right") +
      geom_text(aes(label = scales::percent(Weight)), position = position_stack(vjust = 0.5))
  })
  
  ### Cumulative returns plot for "summary_plots"
  output$CumulativeReturnsPlot <- renderPlot({
    # Fetch the stock data
    stocks <- stockData()
    
    # Ensure the weights are normalized
    weights_normalized <- normWeights() # helper function
    
    # Calculate daily returns
    returns <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock))))
    colnames(returns) <- input$stocks
    
    # Apply portfolio weights to calculate the weighted returns
    weighted_returns <- returns %*% weights_normalized
    
    # Calculate cumulative portfolio returns
    cumulative_returns <- cumprod(1 + weighted_returns) - 1
    cumulative_returns_df <- data.frame(Date = index(returns), Cumulative_Return = coredata(cumulative_returns))
    
    # Ensure Date column is of proper Date class type
    cumulative_returns_df$Date <- as.Date(cumulative_returns_df$Date)
    
    # Determine the date break interval dynamically (depending on the selected time period by the user)
    date_range <- as.numeric(difftime(max(cumulative_returns_df$Date), min(cumulative_returns_df$Date), units = "days"))
    if (date_range <= 30) {
      date_breaks <- "1 day"
    } else if (date_range <= 365) {
      date_breaks <- "1 month"
    } else if (date_range <= 365*5) {
      date_breaks <- "3 months"
    } else if (date_range <= 365*10){
      date_breaks <- "6 months"
    } else if (date_range <= 365*20){
      date_breaks <- "1 year"
    } else {
      date_breaks <- "3 years"
    }
    
    # Plot the cumulative portfolio returns
    ggplot(cumulative_returns_df, aes(x = Date, y = Cumulative_Return)) +
      geom_line(color = "purple", size = 1) +
      labs(x = "Date", y = "Cumulative Return") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid = element_blank(),  # Remove grid lines
        panel.border = element_blank(),  # Remove panel border
        axis.line = element_line(color = "black"),  # Keep axis lines
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      ) +
      scale_x_date(date_breaks = date_breaks, date_labels = "%b %Y")  # Adjust the date breaks and labels dynamically
  })
  
  ### Widgets for "summary_plot"
  output$lowest_return <- renderValueBox({
    stats <- summary_stats()
    valueBox(
      value = stats$lowest_return_stock,
      subtitle = "Lowest Return",
      icon = icon("arrow-down"),
      color = "navy")
  })
  
  output$highest_price <- renderValueBox({
    stats <- summary_stats()
    valueBox(
      value = stats$highest_price_stock,
      subtitle = "Highest Price",
      icon = icon("signal"),
      color = "aqua")
  })
  
  output$highest_weight <- renderValueBox({
    stats <- summary_stats()
    valueBox(
      value = stats$highest_weight,
      subtitle = "Highest Weight",
      icon = icon("chart-pie"),
      color = "maroon")
  })
  
  output$sharpe_ratio <- renderValueBox({
    stats <- summary_stats()
    valueBox(
      value = stats$sharpe_ratio,
      subtitle = "Portfolio Sharpe Ratio",
      icon = icon("exclamation-triangle"),
      color = "orange")
  })
  
  ##############################################################################
  ##### Tab "portfolio_comp"
  
  #### OUTPUT
  
  ### Table for "portfolio_comp"
  output$weights_tab <- renderTable({
    stocks <- stockData() # in case no stock is selected, this displays a message to the user to select at least one stock
    
    # Extract the normalized weights, and multiply by 100 to get percentages
    weights_normalized <- normWeights() *100
    # transpose the weights
    weights_normalized <- t(weights_normalized)
    # Turn it into a data frame
    weights_tab = data.frame(weights_normalized, row.names = c('Weight per Stock in (%)'))
    colnames(weights_tab) <- input$stocks
    weights_tab
    
  }, rownames = TRUE)
  
  ### Pie chart for "portfolio_comp"
  output$pieChart <- renderPlot({
    
    # Check that the values are available 
    req(input$stocks)
    
    # Get normalized weights
    weights_normalized <- normWeights()
    
    # Create data to plot
    pie_data <- data.frame(
      Stock = input$stocks,
      Weight = weights_normalized
    )
    
    # Plot the data
    ggplot(pie_data, aes(x = "", y = Weight, fill = Stock)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_brewer(palette = "Blues") + # specify the color palette
      labs(title = "Pie Chart Portfolio Composition") +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  
  ##############################################################################
  ##### Tab "summary_statistics"
  
  #### OUTPUT
  
  ### Calculate widget statistic: highest_return
  output$highest_return <- renderValueBox({
    stats <- summary_stats()
    valueBox(
      value = stats$highest_return_stock,
      subtitle = "Highest Return",
      icon = icon("arrow-up"),
      color = "green")
  })
  
  ### Calculate widget statistic: most_trending
  output$most_trending <- renderValueBox({
    stats <- summary_stats()
    valueBox(
      value = stats$most_trending_stock,
      subtitle = "Most Trending",
      icon = icon("chart-line"),
      color = "aqua")
  })
  
  ### Calculate widget statistic: highest_volatility
  output$highest_volatility <- renderValueBox({
    stats <- summary_stats()
    valueBox(
      value = stats$highest_volatility_stock,
      subtitle = "Highest Volatility",
      icon = icon("exclamation-triangle"),
      color = "orange")
  })
  
  ### Calculate table of statistics: price_statistics
  output$price_statistics <- DT::renderDataTable({
    stocks <- stockData()
    prices <- do.call(merge, lapply(stocks, function(stock) Cl(stock)))
    colnames(prices) <- unlist(strsplit(input$stocks, split = ","))
    risk_metrics <- table.Stats(prices)
    datatable(risk_metrics, options = list(scrollX = T, lengthChange = F, pageLength = -1, searching = F, paging = F, info = F))
  })
  
  ### Calculate table of statistics: return_statistics
  output$return_statistics <- DT::renderDataTable({
    stocks <- stockData()
    returns <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock))))
    colnames(returns) <- unlist(strsplit(input$stocks, split = ","))
    risk_metrics <- table.Stats(returns)
    datatable(risk_metrics, options = list(scrollX = T, lengthChange = F, pageLength = -1, searching = F, paging = F, info = F))
  })
  
  
  ##############################################################################
  ##### Tab "individual_stock_analysis": "stock_perf"
  
  #### OUTPUT
  
  # Function to get color palette used in the individual stock analysis plots
  fixed_palette <- brewer.pal(max_stocks, "Paired")
  
  # Helper function to get color based on stock name
  getStockColors <- function(stocks) {
    stock_colors <- fixed_palette[seq_along(stocks)]
    names(stock_colors) <- stocks
    return(stock_colors)
  }
  
  # Plot of the prices
  output$indivStockPlot <- renderPlot({
    selected_stocks <- updateSelectedStocks() # get the selected plots for the individual stock analysis (subset of the portfolio)
    req(selected_stocks)  # check that selected stocks are existing before proceeding
    # get the stock data for the subset of selected stocks
    stock_data <- lapply(selected_stocks, function(stock) {
      getSymbols(stock, src = "yahoo", from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE)
    })
    stock_prices <- do.call(merge, lapply(stock_data, Cl)) # extract closing prices
    colnames(stock_prices) <- selected_stocks
    
    # get consistent colors for stocks
    stock_colors <- getStockColors(selected_stocks)
    
    # plot the evolution of stock prices
    plot.zoo(stock_prices, screens = 1, col = stock_colors[selected_stocks], lty = 1, lwd = 2, xlab = "Date", ylab = "Price", main = "Individual Stock Performance")
    legend("topright", legend = selected_stocks, col = stock_colors[selected_stocks], lty = 1, lwd = 2, bty = "n")
  })
  
  # Plot of the return
  output$returnsPlot <- renderPlot({
    selected_stocks <- updateSelectedStocks() # get the selected plots for the individual stock analysis (subset of the portfolio)
    req(selected_stocks) # check that selected stocks are existing before proceeding
    # get the stock data for the subset of selected stocks
    stock_data <- lapply(selected_stocks, function(stock) {
      getSymbols(stock, src = "yahoo", from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE)
    })
    returns <- do.call(merge, lapply(stock_data, function(stock) dailyReturn(Cl(stock))))
    colnames(returns) <- selected_stocks
    
    # get consistent colors for stocks
    stock_colors <- getStockColors(selected_stocks)
    
    # plot the evolution of returns
    plot.zoo(returns, screens = 1, col = stock_colors[selected_stocks], lty = 1, lwd = 2, xlab = "Date", ylab = "Returns", main = "Historical Log-Returns")
    legend("topright", legend = selected_stocks, col = stock_colors[selected_stocks], lty = 1, lwd = 2, bty = "n")
  })
  
  # Plot of the distribution of the returns
  output$densityPlot <- renderPlot({ 
    selected_stocks <- updateSelectedStocks() # get the selected plots for the individual stock analysis (subset of the portfolio)
    req(selected_stocks) # check that selected stocks are existing before proceeding
    # get the stock data for the subset of selected stocks
    stock_data <- lapply(selected_stocks, function(stock) {
      getSymbols(stock, src = "yahoo", from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE)
    })
    # compute returns from the closing prices
    returns <- do.call(merge, lapply(stock_data, function(stock) dailyReturn(Cl(stock))))
    colnames(returns) <- selected_stocks
    
    # get consistent colors for stocks
    stock_colors <- getStockColors(selected_stocks)
    
    # plot the densities of the returns of each selected stock
    plot(NULL, xlim = range(returns, na.rm = TRUE), ylim = c(0, max(sapply(returns, function(x) max(density(na.omit(x))$y)))), xlab = "Returns", ylab = "Density", main = "Density of Log-Returns")
    for (i in 1:ncol(returns)) {
      stock_density <- density(na.omit(returns[, i]))
      lines(stock_density, col = stock_colors[selected_stocks][i], lwd = 2)
    }
    legend("topright", legend = selected_stocks, col = stock_colors[selected_stocks], lty = 1, lwd = 2, bty = "n")
  })
  
  
  ##############################################################################
  ##### Tab "individual_stock_analysis": "correlation_matrix"
  
  #### OUTPUT
  
  # correlation matrix of all the selected portfolio stocks
  output$correlationMatrixPlot <- renderPlot({
    stocks <- stockData() # get selected stocks
    returns <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock)))) # get returns of closing prices
    colnames(returns) <- input$stocks # column names according to the stock names
    
    # create the correlation matrix between the different returns series
    correlation_matrix <- cor(returns, use = "complete.obs") # 'use' argument to deal with possible missing values
    
    # plot the correlation matrix using ggcorrplot
    # add correlation coefficient by lab = TRUE
    ggcorrplot(correlation_matrix, lab = TRUE, lab_size = 5,
               colors = c(brewer.pal(11, "RdBu")[10], "white", brewer.pal(11, "RdBu")[2]),
               ggtheme = theme_minimal())
  })
  
  
  ##############################################################################
  ##### Tab "risk_metrics"
  
  #### OUTPUT
  
  # Calculate annualized standard deviation
  output$Annualized_Std <- renderValueBox({
    returns <- portfolio_returns()
    # calculate the standard deviation and annualize the result
    annualized_std <- sd(returns, na.rm = TRUE) * sqrt(252) # multiply by 252 (which is the number of trading days within a year); sqrt as std. dev. is square root of the variance
    valueBox(
      value = round(annualized_std, 4),
      subtitle = "Annualized Standard Deviation",
      icon = icon("square-root-variable"),
      color = "blue"
    )
  })
  
  # Calculate annualized variance
  output$Annualized_Variance <- renderValueBox({
    returns <- portfolio_returns()
    # calculate the variance and annualize the result
    annualized_variance <- var(returns, na.rm = TRUE) * 252 # multiply by 252 (which is the number of trading days within a year)
    valueBox(
      value = round(annualized_variance, 4),
      subtitle = "Annualized Variance",
      icon = icon("circle-exclamation"),
      color = "purple"
    )
  })
  
  # Calculate Sharpe ratio
  output$Annualized_sharpe_ratio <- renderValueBox({
    # input: stocks, weights and returns
    stocks <- stockData()
    weights_normalized <- normWeights()
    returns <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock)))) # arithmetic returns (non-continuous) of all stocks
    colnames(returns) <- input$stocks
    
    # scale the mean and variance of returns in order to report the annualized sharpe ratio (use 252 as the number of trading days in a year)
    T <- 252
    mu_stocks <- colMeans(returns, na.rm = TRUE) * T   # mean stocks returns
    sigma2_stocks <- cov(returns) * T                  # stocks variance
    mu_PF <- mu_stocks %*% weights_normalized          # portfolio returns
    sigma2_PF <- t(weights_normalized) %*% sigma2_stocks %*% weights_normalized # portfolio variance
    sharpe_ratio <- mu_PF / sqrt(sigma2_PF) # compute Sharpe ratio
    
    valueBox(
      value = round(sharpe_ratio, 4),
      subtitle = "Annualized Sharpe Ratio",
      icon = icon("bolt"),
      color = "orange"
    )
  })
  
  output$volatility_plot <- renderPlot({
    # get the return series that stores the proper dates in the Date format (needed later for plotting)
    stocks <- stockData()
    returns_dates <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock))))
    
    # get the stocks' returns
    returns <- portfolio_returns()
    # compute the standard deviation on a rolling window with a time period of 1 month (that is 21 trading days)
    rolling_std <- rollapply(returns, width = 21, FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252), by.column = FALSE, fill = NA)
    
    # Convert xts object to data frame and ensure proper date handling (use the dates from the first non-edited return series)
    rolling_std_df <- data.frame(Date = index(returns_dates), RollingStd = coredata(rolling_std))
    
    # remove rows with NA values to avoid warnings
    rolling_std_df <- na.omit(rolling_std_df)
    
    # plot the rolling volatility
    ggplot(rolling_std_df, aes(x = Date, y = RollingStd)) +
      geom_line(color = "orange", size = 1) +
      labs(title = "Annualized Volatility (Rolling Window of 1 Month)", x = "Date", y = "Annualized Std Dev") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black")
      ) 
  })
  
  
  ##############################################################################
  ##### Monte Carlo (MC) Simulation
  
  #### INPUT (i.e. helper functions, they are here because they were too many lines to be reported in "Helper Functions")
  
  #### the following part defines functions used for the performed Monte Carlo Simulations
  
  ### Brief Summary of the MC related Functions:
  ### GMB:
  # Computes the price increment using the geometric Brownian motion (GMB)
  
  ### MC_engine:
  # Simulates independent paths for each stock and the resulting portfolio using 
  # independent MC (rejection sampling) while modelling the prices using the geometric Brownian motion (GMB)
  
  ### calc_stats:
  # Calculates summary statistics for each stock and for the portfolio
  
  ### plot_paths(paths, weights, mu_stocks)
  ## Plots the paths of the portfolio
  
  ### GBM:
  GBM = function(dB, mu, sigma, dt) {
    ## Purpose: Computes the price increment using with the geometric Brownian motion (GMB)
    ## ----------------------------------------------------------------------
    ## Arguments:
    ##    dB: standard Brownian motion increment
    ##    mu: expected value of the stock price log returns
    ##    sigma: volatility of the stock price log returns
    ## ----------------------------------------------------------------------
    ## Values:
    ##    The proportional price change
    
    # Check if the arguments are correct (numeric and length of 1)
    stopifnot(is.numeric(dB),is.numeric(mu),is.numeric(sigma),is.numeric(dt))
    stopifnot(length(dB)==1,length(mu)==1,length(sigma)==1,length(dt)==1)
    return(exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * dB))
  }
  
  ### MC_engine:
  MC_engine = function(stocksValue, stockWeights, stocksMu, stocksSigma2) {
    ## Purpose: Simulates independent paths for each stock and the resulting portfolio using
    ##          independent MC (rejection sampling) while modelling the prices
    ##          using the geometric Brownian motion (GMB)
    ## ----------------------------------------------------------------------
    ## Arguments:
    ##    stocksValue: vector, value of stocks at time t=0
    ##    stockWeights: vector, value of weight allocated to each stock
    ##    stocksMu: vector, annual expected value of the returns of each stock
    ##    stocksSigma: matrix, annual covariance matrix of the returns of each stock
    ## ----------------------------------------------------------------------
    ## Values:
    ##    paths: list of T x N matrices, where each matrix contains N=1000 paths of
    ##            length T=252 for each stock and for the portfolio
    ## NB: in case a single stock is involved, no portfolio simulations will be run
    
    # Check if the arguments are what they should be
    stopifnot(is.numeric(stocksValue),is.numeric(stockWeights),is.numeric(stocksMu),is.numeric(stocksSigma2))
    # Should all have the same length
    stopifnot(length(stocksValue)==length(stockWeights),length(stockWeights)==length(stocksMu),length(stocksMu)==length(diag(stocksSigma2)))
    # The covariance matrix should be a square matrix
    stopifnot(is.matrix(stocksSigma2), nrow(stocksSigma2) == ncol(stocksSigma2))
    # The variances should be positive
    stopifnot(all(diag(stocksSigma2) > 0))
    
    ############################################################################
    ### Prepare the inputs
    
    # Change the names of the arguments and fix their format
    d0 = length(stocksValue); value_stocks = matrix(stocksValue,d0,1)
    w = matrix(stockWeights,d0,1) / sum(stockWeights); mu_stocks = matrix(stocksMu,d0,1)
    Sigma2_stocks = matrix(stocksSigma2,d0,d0)
    
    # If only one stock, only run one simulation
    if (length(value_stocks) > 1) {
      # Calculate portfolio stats
      mu_PF     = t(w) %*% mu_stocks
      Sigma_PF2  = t(w) %*% Sigma2_stocks %*% w
      value_PF  = t(w) %*% value_stocks
      # Mean of log returns of each stock and the portfolio
      mu = c(mu_stocks, mu_PF)
      # Volatility of log returns of each stock and the portfolio
      Sigma = sqrt(c(diag(Sigma2_stocks), Sigma_PF2))
      # Value of asset at t=0 of each stock and the portfolio
      value = c(value_stocks, value_PF)
    } else {
      mu = mu_stocks; Sigma = sqrt(Sigma2_stocks); value = value_stocks
    }
    
    ############################################################################
    ### Model asset values
    
    d = length(mu)
    T = 252 # length of paths
    dt = 1 / T
    N = 500 # number of paths per stock / portfolio
    paths = vector("list", length = d) # We will store the paths here
    
    for (j in c(1:d)) {
      paths[[j]] = matrix(NA, nrow = T, ncol = N) # create storage for single asset
      paths[[j]][1,] = value[j] # make its value at t=0 equal to the predetermined value
    }
    
    for (i in c(1:d)) { # An outer for-loop, to cover each stock and the portfolio
      
      mu_temp = mu[i]; sigma_temp = Sigma[i]
      
      for (j in c(1:N)) { # An inner for-loop, to simulate the N paths per stock / portfolio
        
        n_accepted = 1
        
        while (n_accepted < T) { # Simulate one path
          
          # Sample one value from the target distribution using its inverse CDF
          sim = runif(1); x = (sim <= 0.5) * log(2 * sim) - (sim > 0.5) * log(2 - 2 * sim)
          
          # Calculate the acceptance probability
          a = exp(-0.5*(abs(x)-1)^2) # f(x) / g(x) * M
          
          ## Acceptance test
          if (runif(1)<a) {
            # If the proposed value is accepted use it to calculate the next lag of stock price path
            paths[[i]][n_accepted+1,j] = paths[[i]][n_accepted,j] * GBM(x, mu_temp, sigma_temp, dt)
            n_accepted = n_accepted + 1
          }
        }
      }
    }
    return(paths = paths)
  }
  
  ### Calculate portfolio statistics for one year holding
  calc_stats = function(stockNames, paths) {
    ## Purpose: calculates summary statistics for each stock and for the portfolio
    ## ----------------------------------------------------------------------
    ## Arguments:
    ##    stockNames: vector, names of the stocks, with the same order than in the list "paths"
    ##    paths: list of T x N matrices, where each matrix contains N=100 paths of
    ##            length T=252 for each stock and for the portfolio
    ## NB: in case a single stock is involved, no portfolio simulations are expected
    ## ----------------------------------------------------------------------
    ## Values:
    ##    stats_table: table of statistics for each stock and for the portfolio
    ##    quantiles: table of quantiles for each stock and for the portfolio
    
    # The argument must be a list
    stopifnot(is.list(paths), length(stockNames)==length(paths)-1 || length(paths)==length(stockNames))
    
    # Fix the format of the arguments
    d = length(paths); mean_MC = numeric(d); quantiles_MC = matrix(NA,d,5); sigma_MC = numeric(d)
    
    ## Calculate the simulated statistics of annual log-returns
    for (j in c(1:d)) {
      logReturns = log(paths[[j]][T,] / paths[[j]][1,]) # annual log-returns
      mean_MC[j] = mean(logReturns) # means
      quantiles_MC[j,] = quantile(paths[[j]][T,], probs = c(0.05,0.25,0.5,0.75,0.95), na.rm = T) # quantiles
      sigma_MC[j] = sd(logReturns) # volatility
    }
    # If a single stock is involved, make the portfolio's statistics identical to the following
    if (d == 1) {
      mean_MC = c(mean_MC,mean_MC)
      sigma_MC = c(sigma_MC,sigma_MC)
      quantiles_MC = rbind(quantiles_MC,quantiles_MC)
    }
    
    ## Store the simulated statistics
    stats = data.frame(mean = round(mean_MC,3), vola = round(sigma_MC,3),
                       SR = round(mean_MC/sigma_MC,3), row.names = c(stockNames,'Portfolio'))
    quantiles = data.frame(round(quantiles_MC,3), row.names = c(stockNames,'Portfolio'))
    colnames(quantiles) = c('5%','25%','50%','75%','95%')
    
    return(all_stats = list(stats_table = stats, quantiles = quantiles))
  }
  
  ### Plot portfolio paths
  plot_paths = function(paths, weights, mu_stocks) {
    ## Purpose: plot the paths of the portfolio
    ## ----------------------------------------------------------------------
    ## Arguments:
    ##    paths: list of T x N matrices, where each matrix contains N=100 paths of
    ##            length T=252 for each stock and for the portfolio
    ##  NB: in case a single stock is involved, no portfolio simulations are expected
    ##    weights: vector, value of weight allocated to each stock
    ##    mu_stocks: vector, annual expected value of the returns of each stock
    ## ----------------------------------------------------------------------
    ## Values:
    ##    The function will return a plot of the paths of the portfolio
    
    # The argument must be a list
    stopifnot(is.list(paths))
    stopifnot(is.numeric(weights),is.numeric(mu_stocks))
    stopifnot(length(weights) == length(mu_stocks))
    
    # Fix the format of the arguments
    d0 = length(mu_stocks)
    w = matrix(weights / sum(weights),d0,1)
    mu_stocks = matrix(mu_stocks,d0,1)
    
    # Paths of interest:
    d = length(paths)
    paths_PF = paths[[d]][,c(1:100)] # NB: plot a random sample of paths
    paths_PF = 100 * paths_PF / paths_PF[1,1] # normalise the paths
    mu_PF = t(w) %*% mu_stocks
    
    # Calculate the (deterministic) trend in stock price
    T = 252; trend = numeric(T); trend[1] = 100; trend[2:T] = 100 * (1 + c(1:251) * c(mu_PF) / T)
    
    # Calculate mean absolute deviation from trend for each path
    deviations = apply(paths_PF, 2, function(path) mean(abs(path - trend)))
    
    # Normalize deviations to the range [0, 1]
    max_deviation = max(deviations)
    min_deviation = min(deviations)
    normalized_deviations = (deviations - min_deviation) / (max_deviation - min_deviation)
    
    # Create a color gradient from dark color to red
    color_gradient = colorRampPalette(c("darkblue", "red"))(100)
    
    # Map normalized deviations to the color gradient
    path_colors = color_gradient[as.numeric(cut(normalized_deviations, breaks = 100))]
    
    # Set larger white borders
    par(mar=c(5, 5, 4, 5) + 0.1)
    
    # Plot all paths using matplot with custom colors
    matplot(paths_PF, type = 'l', lty = 1, xlab = 'Time', ylab = 'Value', main = 'Simulated Portfolio Paths', col = path_colors)
    
    # Set smaller margin and tick length
    par(mgp=c(2, .5, 0), tcl=-0.3)
    
    # Calculate quantiles for the values of the paths at row 252
    quantiles = quantile(paths_PF[T,], probs = c(0.05,0.25,0.5,0.75,0.95))
    
    # Add red ticks for the specified quantiles on the right y-axis with labels
    axis(side = 4, at = quantiles, labels = FALSE, col.axis = "blue")
    axis(side = 4, at = quantiles, labels = paste0(c("5%", "25%", "50%", "75%", "95%"), ': ', round(quantiles, 2)), 
         col.axis = "blue", las = 2, cex.axis = 0.8)
  }
  
  ### Calculate returns for MC simulation
  getMCReturns <- reactive({
    stocks <- stockData()
    # calculate the log returns via the closing prices of the stock
    returns <- do.call(merge, lapply(stocks, function(stock) dailyReturn(Cl(stock), type = 'log')))
    returns <- na.omit(returns)
    colnames(returns) <- input$stocks
    returns
  })
  
  ### Get the last close value for each stock
  getLastCloseValues <- reactive({
    # Get the close column name dynamically for each selected stock
    stocks <- stockData()
    sapply(stocks, function(stock) {
      close_col_name <- grep("\\.Close$", colnames(stock), value = TRUE)
      # Extract the last value from the close column
      stock[nrow(stock), close_col_name]
    })
  })
  
  ### Run the Monte Carlo simulation
  runMCSimulation <- eventReactive(input$run_mc, {
    warning <- stockData() # gives a warning (message) to the user that no stock when no stock is selected
    
    # display a message to the user that the simulation is running in the background
    showModal(modalDialog(
      title = "Running Monte Carlo Simulation.",
      "Please wait...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    # Fetch necessary data
    stocks <- input$stocks                  # stock symbols
    weights <- normWeights()                # normalized weights
    returns <- getMCReturns()               # returns
    value_stocks <- getLastCloseValues()   # last close price for each stock
    T <- 252                                # time period of one year
    mu_stocks <- colMeans(returns, na.rm = TRUE) * T    # annualized return
    Sigma_stocks2 <- cov(returns) * T       # annualized variance
    
    # Run MC simulation
    paths <- MC_engine(value_stocks, weights, mu_stocks, Sigma_stocks2)
    stats <- calc_stats(stocks, paths)
    
    # remove the modal dialog
    removeModal()
    
    # return the simulation paths and the statistics
    list(paths = paths, stats = stats)
  })
  
  
  #### OUTPUT
  
  ### Render MC simulation plot
  output$mc_plot <- renderPlot({
    mc_results <- runMCSimulation()
    paths <- mc_results$paths
    weights <- normWeights()
    mu_stocks <- colMeans(getMCReturns(), na.rm = TRUE) * 252
    plot_paths(paths, weights, mu_stocks)
  })
  
  ### Render MC summary statistics table
  output$mc_summary_table <- renderDT({
    mc_results <- runMCSimulation()
    datatable(mc_results$stats$stats_table,
              options = list(
                paging = FALSE,                         # disable pagination
                lengthMenu = list(c(-1), c("All")))     # always show all entries
    )
  })
  
  ### Render MC quantiles table
  output$mc_quantiles_table <- renderDT({
    mc_results <- runMCSimulation()
    datatable(mc_results$stats$quantiles,
              options = list(
                paging = FALSE,                         # disable pagination
                lengthMenu = list(c(-1), c("All")))     # always show all entries
    )
  })
}

################################################################################
#### Run the application 

shinyApp(ui = ui, server = server)


################################################################################
#### Disclaimer: We acknowledge the use of ChatGPT for aid in the writing of this document.
