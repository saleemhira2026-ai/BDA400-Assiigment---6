library(shiny)
library(ggplot2)
library(quantmod)

# Get stock data
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# User Interface
ui <- fluidPage(
  
  titlePanel("Stock Portfolio Dashboard"),
  
  dateRangeInput(
    "date_range",
    "Select Date Range:",
    start = "2023-01-01",
    end = "2023-07-01"
  ),
  
  selectInput(
    "time_frame",
    "Select Time Frame:",
    choices = c("Daily", "Weekly", "Monthly")
  ),
  
  checkboxGroupInput(
    "technical_indicators",
    "Select Technical Indicators:",
    choices = c(
      "Moving Averages",
      "RSI",
      "MACD"
    ),
    selected = "Moving Averages"
  ),
  
  plotOutput("stock_chart")
)

# Server

server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    
    # Filter data by selected dates
    filtered_data <- subset(
      stock_data,
      index(stock_data) >= input$date_range[1] &
        index(stock_data) <= input$date_range[2]
    )
    
    # Change time frame
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(filtered_data)
      
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(filtered_data)
    }
    
    # Closing prices
    close_prices <- as.numeric(Cl(filtered_data))
    
    # Create data frame
    plot_data <- data.frame(
      Date = index(filtered_data),
      Close = close_prices
    )
    
    # 20-day and 50-day moving averages
    short_ma <- SMA(close_prices, n = 20)
    long_ma <- SMA(close_prices, n = 50)
    
    # Buy / Sell / Hold signals
    signals <- ifelse(
      short_ma > long_ma,
      "Buy",
      ifelse(
        short_ma < long_ma,
        "Sell",
        "Hold"
      )
    )
    
    plot_data$Signal <- signals
    plot_data$ShortMA <- as.numeric(short_ma)
    plot_data$LongMA <- as.numeric(long_ma)
    
    # Basic stock chart
    p <- ggplot(
      plot_data,
      aes(x = Date, y = Close)
    ) +
      geom_line() +
      labs(
        title = "AAPL Stock Price",
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()
    
    # Moving Average
    if ("Moving Averages" %in% input$technical_indicators) {
      
      plot_data$MovingAverage <- as.numeric(
        SMA(close_prices, n = 20)
      )
      
      p <- p +
        geom_line(
          data = subset(plot_data, !is.na(MovingAverage)),
          aes(x = Date, y = MovingAverage)
        )
    }
    
    # RSI
    if ("RSI" %in% input$technical_indicators) {
      
      rsi_values <- RSI(close_prices, n = 14)
      
      plot_data$RSI <- as.numeric(rsi_values)
      
      p <- p +
        geom_line(
          data = subset(plot_data, !is.na(RSI)),
          aes(x = Date, y = RSI)
        )
    }
    
    # MACD
    if ("MACD" %in% input$technical_indicators) {
      
      macd_values <- MACD(
        close_prices,
        nFast = 12,
        nSlow = 26,
        nSig = 9
      )
      
      plot_data$MACD <- as.numeric(
        macd_values[, "macd"]
      )
      
      p <- p +
        geom_line(
          data = subset(plot_data, !is.na(MACD)),
          aes(x = Date, y = MACD)
        )
    }
    
    # Create signal data
    signal_data <- subset(
      plot_data,
      Signal %in% c("Buy", "Sell") &
        !is.na(ShortMA) &
        !is.na(LongMA)
    )
    
    # Add Buy and Sell annotations
    p <- p +
      geom_text(
        data = signal_data,
        aes(
          x = Date,
          y = Close,
          label = Signal
        ),
        vjust = -0.8,
        size = 3
      )
    
    print(p)
  })
}

shinyApp(ui, server)

shinyApp(ui, server)


server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    
    filtered_data <- subset(
      stock_data,
      index(stock_data) >= input$date_range[1] &
        index(stock_data) <= input$date_range[2]
    )
    # Apply selected time frame
 
    
    
    
    # Filter data by selected dates
    filtered_data <- subset(
      stock_data,
      index(stock_data) >= input$date_range[1] &
        index(stock_data) <= input$date_range[2]
    )
    
    # Change time frame
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(filtered_data)
      
    } else if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(filtered_data)
    }
    
    # Closing prices
    close_prices <- as.numeric(Cl(filtered_data))
    
    # Create data frame
    plot_data <- data.frame(
      Date = index(filtered_data),
      Close = close_prices
    )
    
    # 20-day and 50-day moving averages
    short_ma <- SMA(close_prices, n = 20)
    long_ma <- SMA(close_prices, n = 50)
    
    # Buy / Sell / Hold signals
    signals <- ifelse(
      short_ma > long_ma,
      "Buy",
      ifelse(
        short_ma < long_ma,
        "Sell",
        "Hold"
      )
    )
    
    plot_data$Signal <- signals
    plot_data$ShortMA <- as.numeric(short_ma)
    plot_data$LongMA <- as.numeric(long_ma)
    
    # Basic stock chart
    p <- ggplot(
      plot_data,
      aes(x = Date, y = Close)
    ) +
      geom_line() +
      labs(
        title = "AAPL Stock Price",
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()
    
    # Moving Average indicator
    if ("Moving Averages" %in% input$technical_indicators) {
      
      plot_data$MovingAverage <- as.numeric(
        SMA(close_prices, n = 20)
      )
      
      p <- p +
        geom_line(
          data = subset(plot_data, !is.na(MovingAverage)),
          aes(x = Date, y = MovingAverage)
        )
    }
    
    # RSI indicator
    if ("RSI" %in% input$technical_indicators) {
      
      rsi_values <- RSI(close_prices, n = 14)
      
      plot_data$RSI <- as.numeric(rsi_values)
      
      p <- p +
        geom_line(
          data = subset(plot_data, !is.na(RSI)),
          aes(x = Date, y = RSI)
        )
    }
    
    # MACD indicator
    if ("MACD" %in% input$technical_indicators) {
      
      macd_values <- MACD(
        close_prices,
        nFast = 12,
        nSlow = 26,
        nSig = 9
      )
      
      plot_data$MACD <- as.numeric(macd_values[, "macd"])
      
      p <- p +
        geom_line(
          data = subset(plot_data, !is.na(MACD)),
          aes(x = Date, y = MACD)
        )
    }
    
    # Add Buy and Sell annotations
    signal_data <- subset(
      plot_data,
      Signal %in% c("Buy", "Sell") &
        !is.na(ShortMA) &
        !is.na(LongMA)
    )
    # Select Buy and Sell signals for annotation
    signal_data <- subset(
      plot_data,
      Signal %in% c("Buy", "Sell") &
        !is.na(ShortMA) &
        !is.na(LongMA)
    )
    p <- p +
      geom_text(
        data = signal_data,
        aes(
          x = Date,
          y = Close,
          label = Signal
        ),
        vjust = -0.8,
        size = 3
      )
    
    print(p)
  })
}

