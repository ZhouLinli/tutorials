#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(broom) # for nice regression output
library(broom.mixed) # same for Bayes
library(rstanarm) # for quick Bayesian models

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Regression Simulator"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("N",
                  "Number of points",
                  min = 2,
                  max = 100,
                  value = 20),
      sliderInput("xmean",
                  "True Mean of X",
                  min = -5,
                  max = 5,
                  value = 2),
      sliderInput("xsd",
                  "True SD of X",
                  min = 0,
                  max = 5,
                  value = 1,
                  step = .1),
      checkboxInput("sort",
                    "Sort data",
                    value = FALSE),
      #sliderInput("pmean",
      #            "Proposed Mean",
      #            min = -5,
      #            max = 5,
      #            value = 2),
      actionButton("button", "Run simulation")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Least Squares"),
      plotOutput("lmPlot"),
      tableOutput("lmModel"),
      h2("Maximum Likelihood"),
      plotOutput("bayesPlot"),
      tableOutput("bayesModel")
    )
  )
)

# Define server
server <- function(input, output) {
  
  rv <- reactiveValues(sim = NULL, model = NULL)
  
  # generate data
  observeEvent(input$button, {
    x <- rnorm(input$N, input$xmean, input$xsd)
    
    # save data in accessible dataframe   
    rv$sim <- data.frame(x = x)
    
    # run least squares model
    rv$model <- lm(x ~ 1, rv$sim )
    
    # run Bayes model
    rv$b_model <- stan_glm(x ~ 1, rv$sim, family = gaussian() )
  })
  
  # least squares version
  output$lmPlot <- renderPlot({
    x_mean <- mean(rv$sim$x)
    p_mean <- input$pmean
    .title <- str_c("Average = ", round(x_mean, 2))
    
    tdf <- rv$sim
    
    if(input$sort == TRUE) tdf <- tdf %>% arrange(x)
    
     tdf %>% 
      mutate(obs = row_number()) %>% 
      ggplot(aes(x = obs, y = x, yend = x_mean, xend = obs)) +
      geom_segment() +
      geom_point(size = 2, color = "steelblue") +
      geom_hline(yintercept = x_mean, color = "blue") +
     # geom_hline(yintercept = p_mean, color = "black") +
      geom_smooth(method = "lm", se = FALSE, formula = "y ~ 1") +
      theme_bw() +
      ggtitle(.title)
  })
  
  output$lmModel <- renderTable({
    rv$model %>% 
      broom::tidy()
  })
  
  # Bayes version
  output$bayesPlot <- renderPlot({
    
    b_model_params <- rv$b_model %>% 
                        broom.mixed::tidy()
    
    x_mean <- b_model_params$estimate[1]
    x_sd   <- b_model_params$std.error[1] * sqrt(input$N - 1)
    
    rv$sim %>%
      ggplot(aes(x = x)) + 
      geom_density(fill = "gray") +
      geom_function(fun = dnorm, colour = "blue", size = 1, 
                    args = list(mean = x_mean, sd = x_sd)) +
      theme_bw()
  })
  
  output$bayesModel <- renderTable({
    rv$b_model %>% 
      broom.mixed::tidy()
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
