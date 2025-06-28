library(shiny)

# Define the UI for the efficient frontier app
ui <- fluidPage(
  titlePanel("Efficient Frontier of Two Stocks"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("risk_free", "Risk-free rate:",
                  min = 0, max = 0.1, value = 0.02, step = 0.001),
      sliderInput("return1", "Return1:",
                  min = 0, max = 0.1, value = 0.07, step = 0.001),
      sliderInput("return2", "Return2:",
                  min = 0, max = 0.1, value = 0.1, step = 0.001),
      sliderInput("sd1", "Standard Deviation 1:",
                  min = 0.05, max = 0.5, value = 0.1, step = 0.01),
      sliderInput("sd2", "Standard Deviation 2:",
                  min = 0.05, max = 0.5, value = 0.2, step = 0.01),
      sliderInput("correlation", "Correlation:",
                  min = -0.99, max = 0.99, value = 0.5, step = 0.01)
    ),
    
    mainPanel(
      plotOutput("efficientFrontierPlot")
    )
  )
)

# Define the server logic for the efficient frontier app
server <- function(input, output) {
  
  output$efficientFrontierPlot <- renderPlot({
    # Define inputs
    risk_free <- input$risk_free
    return1 <- input$return1
    return2 <- input$return2
    sd1 <- input$sd1
    sd2 <- input$sd2
    correlation <- input$correlation
    
    # Calculate covariance between the two stocks
    cov12 <- correlation * sd1 * sd2
    
    # Portfolio returns and standard deviations
    weights <- seq(0, 1, by = 0.01)
    portfolio_returns <- weights * return1 + (1 - weights) * return2
    portfolio_sd <- sqrt((weights^2) * (sd1^2) + ((1 - weights)^2) * (sd2^2) + 
                           2 * weights * (1 - weights) * cov12)
    
    # Calculate Capital Market Line (CML)
    sharpe_ratio <- (portfolio_returns - risk_free) / portfolio_sd
    max_sharpe_index <- which.max(sharpe_ratio)
    cml_slope <- sharpe_ratio[max_sharpe_index]
    cml_returns <- risk_free + cml_slope * portfolio_sd
    
    # Plot efficient frontier and CML
    plot(portfolio_sd, portfolio_returns, type = "l", col = "orange", lwd = 2,
         xlab = "standard deviation", ylab = "return",
         main = paste("Efficient Frontier and CML for Two Stocks\ncorrelation =", round(correlation * 100, 1), "%"))
    points(sd1, return1, col = "green", pch = 19)
    text(sd1, return1, labels = "stock1", pos = 4)
    points(sd2, return2, col = "green", pch = 19)
    text(sd2, return2, labels = "stock2", pos = 4)
    lines(portfolio_sd, cml_returns, col = "blue", lwd = 2)
    
    # Highlight the max Sharpe ratio point
    points(portfolio_sd[max_sharpe_index], portfolio_returns[max_sharpe_index], col = "blue", pch = 19)
    text(portfolio_sd[max_sharpe_index], portfolio_returns[max_sharpe_index],
         labels = paste("Max Sharpe\n", round(sharpe_ratio[max_sharpe_index], 2)), pos = 4)
    
    # Add labels for the risk-free rate
    points(0, risk_free, col = "blue", pch = 19)
    text(0, risk_free, labels = "risk-free rate", pos = 4)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
