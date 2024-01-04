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

# keep grades in range
grade_range <- function(x) {pmax( pmin(x, 4), 0)}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Selection and Direct Effects"),
  
  # Sidebar layout with a sliders
  sidebarLayout(
    sidebarPanel(
      sliderInput("N",
                  "Number of points",
                  min = 50,
                  max = 500,
                  value = 100),
      sliderInput("xxx_gpa1",
                  "XXX selection effect",
                  min = -1,
                  max = 1,
                  value = 0,
                  step = .1),
      sliderInput("xxx_gpa2",
                  "XXX grade effect",
                  min = -1,
                  max = 1,
                  value = 0,
                  step = .1),
      sliderInput("gpa1",
                  "Grade Reliability",
                  min = 0,
                  max = 1,
                  value = .5,
                  step = .1),
      checkboxInput("sort",
                    "Sort data",
                    value = FALSE),
      actionButton("button", "Run simulation")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("lmPlot"),
      tableOutput("lmModel"),
    )
  )
)

# Define server
server <- function(input, output) {
  
  rv <- reactiveValues(sim = NULL, model = NULL)
  
  # generate data
  observeEvent(input$button, {

    # save data in accessible dataframe   
    rv$sim <- data.frame(GPA1 = rnorm(input$N, 3, 1),
                         Org = sample(c("XXX","Other"), input$N, replace = TRUE, prob = c(.2, .8))) %>% 
                         mutate( XXX_GPA1 = ifelse( Org == "XXX",input$xxx_gpa1,0),
                                 XXX_GPA2 = ifelse( Org == "XXX",input$xxx_gpa2,0),
                                 # selection effect
                                 GPA1 = grade_range(GPA1 + XXX_GPA1),
                                 # grade reliability
                                 GPA2 = grade_range(input$gpa1*GPA1 +
                                                    ((1-input$gpa1)*rnorm(input$N, 3, 1))+
                                                    XXX_GPA2))
    
    # run least squares model
    rv$model <- lm(GPA2 ~ Org + GPA1, rv$sim )
    
    rv$sim <- rv$sim %>% 
      mutate(pGPA2 = fitted.values(rv$model))
   
  })
  
  # least squares model
  output$lmPlot <- renderPlot({
    r2 <- broom::glance(rv$model) %>% select(adj.r.squared) %>% pull()

    .title <- str_c("R^2 = ", round(r2, 2))
    
    org_means <- rv$sim %>% 
      group_by(Org) %>% 
      summarize(GPA1 = mean(GPA1),
                GPA2 = mean(GPA2))
    
    rv$sim %>% 
      ggplot(aes(x = GPA1, y = GPA2, group = Org, color = Org)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      theme_bw() +
      geom_label(aes(x = GPA1, y = GPA2, label = Org), data = org_means) +
      ggtitle(.title)
  })
  
  output$lmModel <- renderTable({
    rv$model %>% 
      broom::tidy()
  })
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
