# Title: S&P 500 Returns
# Description: 
# Author: Arin Telimi
 


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(lubridate)  # for working with dates
library(tidyquant)  # financial & quant functions
library(plotly)     # for web interactive graphics


# ======================================================
# Auxiliary objects/functions 
# (that don't depend on input widgets)
# ======================================================
# You may want to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)





# =======================================================
# Define UI for application
# =======================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("S&P 500 Returns"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # inputs
      sliderInput(inputId = "period",
                  label = "Time Period:",
                  min = 1928,
                  max = 2023, 
                  value = c(1990, 2020), 
                  sep = ""),
      radioButtons(inputId = "scale",
                   label = "Y-Axis Scale:",
                   choices = list("Linear Scale" = "linear",
                                  "Logarithmic Scale (log-10)" = "log")),
      numericInput(inputId = "nyears",
                   label = "Number-of-Years Return:",
                   value = 5,
                   min = 1,
                   max = 95),
      selectInput(inputId = "statistics",
                   label = "Statistics to Show:",
                   choices = c("Mean Return (Red Line)" = "mean",
                               "Median Return (Blue Line)" = "median",
                               "Standard Deviation (Orange Line)" = "sd"),
                  multiple = TRUE,
                  ),
      #Prints a brief description of the project. 
      print("This application provides descriptive analyses of S&P 500 historical daily closing values and S&P 500 multi-year returns."),
      print("Plot 1 provides a time-period adjustable line graph of the daily closing values."),
      print("Plot 2 provides a time-period adjustable bar chart of average returns over an adjustable year range."),
      print("The Table provides time-period adjustable summary statistics over an adjustable year range.")
      
      
    ),  # closes sidebarPanel of inputs
    
    # -------------------------------------------------------
    # Main Panel with outputs: plots and table
    # -------------------------------------------------------
    mainPanel(
      h3("S&P 500 Daily Closing Values"),
      plotlyOutput(outputId = "plot1"),
      hr(),
      h3("S&P 500 N-Year-Returns"),
      plotlyOutput(outputId = "plot2"),
      hr(),
      h3("S&P 500 N-Year Summary Statistics Table"),
      tableOutput(outputId = "table"),
    ) # closes mainPanel of outputs
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Auxiliary table (note that this is a reactive conductor)
  # ------------------------------------------------------------
  
  #Getting our data.
  sp500 = reactive({
    tq_get("^GSPC", from = "1928-01-03", to = "2023-12-29")
  })
  #Filtering data specifically for our period
  sp500_period = reactive({
    data = sp500()
    data |>
      filter(year(date) >= !!input$period[1], year(date) <= !!input$period[2])
  })
  
  #nyear calculations
  sp500_nyear_returns = reactive({
    sp500_dat = sp500_period() |>
      select(date, close) |>
      mutate(year = year(date))
    sp500_returns = data.frame(years = character(), return = numeric())
    #if/else ensures that period and number-of-years are compatible for plot2
    if ((input$period[2] - input$nyears < input$period[1]) | input$nyears == 0) {
      return(sp500_returns)  # Return an empty data frame
    } else{
    for (i in seq(input$period[1], input$period[2] - input$nyears + 1, by = 1)) {  
      year_range = paste(i, i + input$nyears - 1, sep = "-")
      return_value = sp500_dat |>
        filter(year >= i & year <= i + input$nyears - 1) |>
        summarise(return = ((last(close) - first(close)) / first(close))*100, .groups = 'drop') |>
        pull(return)
      sp500_returns = rbind(sp500_returns, data.frame(years = year_range, return = return_value))
    }
    } 
    sp500_returns
  })
  
  #Summary Statistics Calculations
  summary_stats = reactive({
    return_data = sp500_nyear_returns()$return
    if (length(return_data) > 0){
      
      tenth = quantile(return_data, 0.1, na.rm = TRUE)
      quartile1 = quantile(return_data, 0.25, na.rm = TRUE)
      mymean = mean(return_data, na.rm = TRUE)
      mymedian = median(return_data, na.rm = TRUE)
      quartile3 = quantile(return_data, 0.75, na.rm = TRUE)
      ninetieth = quantile(return_data, 0.9, na.rm = TRUE)
      mysd = sd(return_data, na.rm = TRUE)
      
      mystats = data.frame(
        Statistic = c(
          "10th Percentile", 
          "25th Percentile", 
          "Mean", 
          "Median", 
          "75th Percentile", 
          "90th Percentile", 
          "Standard Deviation"),
        #used paste0 to add percent sign after each percent shown
        Value = c(
          paste0(round(tenth, 2), "%"),
          paste0(round(quartile1, 2), "%"),
          paste0(round(mymean, 2), "%"),
          paste0(round(mymedian, 2), "%"),
          paste0(round(quartile3, 2), "%"),
          paste0(round(ninetieth, 2), "%"),
          paste0(round(mysd, 2), "%")
        )
      )
    } else {
      mystats = data.frame(
        statistic = "No Data Available",
        Value = NA   
      )
    }
    return(mystats)
  })
  
  # ------------------------------------------------------------
  # Plot (timeline of daily closing values)
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    sp500_period() |>
      ggplot(aes(x = date, y = close)) +
      geom_line(color = "chartreuse") +
      labs(x = "Date",
           y = "Closing Value",
           title = paste(input$period[1], input$period[2], sep = " - ")) +
      scale_y_continuous(trans = if (!!input$scale == "log") "log10" else "identity") +
      theme_dark()
    
  })
  
  
  # ------------------------------------------------------------
  # Plot (bar-chart of multi-year returns)
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    
    
   
    if (input$period[2] - input$nyears >= input$period[1]) {
    myplot = sp500_nyear_returns() |>
      ggplot() +
      geom_col(aes(x = years, y = return), fill = "chartreuse") + 
      theme_minimal() +
      labs(x = "Years",
           y = "Return (%)",
           title = paste(input$period[1], "-" , input$period[2], input$nyears, "Year Returns", sep = " "),
           caption = "Red Line is the Mean \n Blue Line is the Median \n Orange Line is the Standard Deviation") +
      theme_dark() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
    
    if ("mean" %in% input$statistics) {
      mean_value = mean(sp500_nyear_returns()$return, na.rm = TRUE)
      myplot = myplot +
        geom_hline(yintercept = mean_value, color = "red")
    }
    
    if ("median" %in% input$statistics) {
      median_value = median(sp500_nyear_returns()$return, na.rm = TRUE)
      myplot = myplot +
        geom_hline(yintercept = median_value, color = "blue")
    }
    
    if ("sd" %in% input$statistics) {
      sd_value = sd(sp500_nyear_returns()$return, na.rm = TRUE)
      myplot = myplot +
        geom_hline(yintercept = sd_value, color = "orange")
    }
    
    } else {
      myplot = ggplot() + 
        theme_dark() + 
        annotate("text", x = 0.5, y = 0.5, label = "The Number-of-Years Return must be less than the time period", 
                 color = "red", size = 3, hjust = 0.5, vjust = 0.5)
    }
    ggplotly(myplot)
  })
  
  
  # ------------------------------------------------------------
  # Table
  # (adapt code to display appropriate table!!!)
  # ------------------------------------------------------------
  output$table <- renderTable({
    
    summary_stats()
    
  })
  
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
