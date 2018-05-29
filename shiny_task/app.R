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
library(wesanderson)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Task Completion Calculator"),
  
  
  
  # Numeric input total
  sidebarLayout(
    sidebarPanel(
      
      #subtitle
      h4("Enter your study data below"),
      hr(),
      
      
      numericInput(inputId = "task_num",
                   "Number of tasks in the study",
                   min = 1,
                   max = 8,
                   value = 2)
      ,
      
      
      numericInput(inputId = "total",
                   "Number of users tested",
                   min = 1,
                   max = 50,
                   value = 10)
      ,
      
      #start completion sliders
      hr(),
      h4("Enter task completion rates below for each task"),
      hr(),
      #task 1 passed input
      sliderInput(inputId = "pass1",
                  "Task 1 Successes",
                  min = 1,
                  max = 50,
                  value = 10,
                  ticks = FALSE)
      ,
      #task 2 passed input (conditional)
      conditionalPanel(
        condition = "input.task_num >= 2",
        sliderInput(inputId = "pass2",
                    "Task 2 Successes",
                    min = 1,
                    max = 50,
                    value = 10,
                    ticks = FALSE)
      )
      ,
      #task 3 passed input (conditional)
      conditionalPanel(
        condition = "input.task_num >= 3",
        sliderInput(inputId = "pass3",
                    "Task 3 Successes",
                    min = 1,
                    max = 50,
                    value = 10,
                    ticks = FALSE)
      )
      ,
      #task 4 passed input (conditional)
      conditionalPanel(
        condition = "input.task_num >= 4",
        sliderInput(inputId = "pass4",
                    "Task 4 Successes",
                    min = 1,
                    max = 50,
                    value = 10,
                    ticks = FALSE)
      ),
      
      #task 5 passed input (conditional)
      conditionalPanel(
        condition = "input.task_num >= 5",
        sliderInput(inputId = "pass5",
                    "Task 5 Successes",
                    min = 1,
                    max = 50,
                    value = 10,
                    ticks = FALSE)
      ),
      
      #task 6 passed input (conditional)
      conditionalPanel(
        condition = "input.task_num >= 6",
        sliderInput(inputId = "pass6",
                    "Task 6 Successes",
                    min = 1,
                    max = 50,
                    value = 10,
                    ticks = FALSE)
      ),
      
      #task 7 passed input (conditional)
      conditionalPanel(
        condition = "input.task_num >= 7",
        sliderInput(inputId = "pass7",
                    "Task 7 Successes",
                    min = 1,
                    max = 50,
                    value = 10,
                    ticks = FALSE)
      ),
      
      #task 8 passed input (conditional)
      conditionalPanel(
        condition = "input.task_num >= 8",
        sliderInput(inputId = "pass8",
                    "Task 8 Successes",
                    min = 1,
                    max = 50,
                    value = 10,
                    ticks = FALSE)
      ),
      
      #advanced panel open
      radioButtons(inputId = "adv",
                    label = "Advanced options",
                    c("Hide" = "hide",
                      "Show" = "show"
                                ),
                   inline=T
      ),
      
      #advanced options
      
      #point estimate selection
      conditionalPanel(
        condition = "input.adv == 'show' ",
        radioButtons(inputId = "point_est",
                    "Point estimate calculation method:",
                    choices = c("LaPlace","Exact"),
                    selected = "LaPlace")
      ),
      
      #color chooser
      
      conditionalPanel(
        condition = "input.adv == 'show' ",
        selectInput(inputId = "colpal",
                     "Color palette generation:",
                     choices = c("Red Hat Brand" = "rh",
                                 "The Life Aquatic" = "ziz",
                                 "The Grand Budapest Hotel" = "buda",
                                 "The Royal Tenenbaums" = "royal",
                                 "The Darjeeling Limited" = "dar",
                                 "Moonrise Kingdom" = "moon",
                                 "Color Blind Friendly" = "color-b",
                                 "Single Hue" = "single"),
                     selected = "rh")
      )
      
      #end sidebar
    ), 
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      hr(),
      h4("Data used to produce graph"),
      tableOutput("table")
    )
  )
)

# Define server logic r
server <- function(input, output, session) {
  
  
  #change slider end points
  observe({
    val <- input$total
    # Control the value, min, max, and step on range of task completion
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "pass1", max = val)
    updateSliderInput(session, "pass2", max = val)
    updateSliderInput(session, "pass3", max = val)
    updateSliderInput(session, "pass4", max = val)
    updateSliderInput(session, "pass5", max = val)
    updateSliderInput(session, "pass6", max = val)
    updateSliderInput(session, "pass7", max = val)
    updateSliderInput(session, "pass8", max = val)
  })
  
  
  #build df for all plots/tables
  df <- reactive({
    if (input$task_num == 8) {
      pass <- c(input$pass1,input$pass2,input$pass3,input$pass4,input$pass5,input$pass6,input$pass7,input$pass8)
      task <- c(1:8)
      df_t <- data.frame(pass,task)
      
    } else if (input$task_num == 7) {
      pass <- c(input$pass1,input$pass2,input$pass3,input$pass4,input$pass5,input$pass6,input$pass7)
      task <- c(1:7)
      df_t <- data.frame(pass,task)
      
    } else if (input$task_num == 6) {
      pass <- c(input$pass1,input$pass2,input$pass3,input$pass4,input$pass5,input$pass6)
      task <- c(1:6)
      df_t <- data.frame(pass,task)
      
    } else if (input$task_num == 5) {
      pass <- c(input$pass1,input$pass2,input$pass3,input$pass4,input$pass5)
      task <- c(1:5)
      df_t <- data.frame(pass,task)
      
    } else if (input$task_num == 4) {
      pass <- c(input$pass1,input$pass2,input$pass3,input$pass4)
      task <- c(1:4)
      df_t <- data.frame(pass,task)
      
    } else if (input$task_num == 3) {
      pass <- c(input$pass1,input$pass2,input$pass3)
      task <- c(1:3)
      df_t <- data.frame(pass,task)
      
    } else if (input$task_num == 2) {
      pass <- c(input$pass1,input$pass2)
      task <- c(1:2)
      df_t <- data.frame(pass,task)
      
    } else {
      pass <- c(input$pass1)
      task <- c(1)
      df_t <- data.frame(pass,task)
      
      
    }
   
   # z <- input$z_value
    
    
    df_t %>% 
      mutate(total=input$total) %>% 
      mutate(prop=pass/total) %>%
      mutate(laplace = (pass+1)/(total+2)) %>%
      mutate(marg_laplace=( 
        (sqrt( (laplace * (1-laplace)) /(total+2)) ) *1.96)
      ) %>%
      mutate(lowerci=laplace-marg_laplace) %>%
      mutate(lowerci= ifelse(lowerci<=0,0,lowerci)) %>%
      mutate(upperci=laplace+marg_laplace) %>%
      mutate(upperci= ifelse(upperci>=1,1,upperci)) %>%
      mutate(task=as.factor(trunc(task,digits=1)) ) -> df
      
      
      
      }) #end df creation
      
      #color palette generator
  
      colpal <- reactive({
        
        if (input$colpal == "rh") {
          c("#cc0000",
            "#0088ce",
            "#f0ab00",
            "#3B0083",
            "#00b9e4",
            "#92d400",
            "#007a87",
            "#f0ab00"
          ) -> colpal
          
        } else if (input$colpal == "ziz") {
          
          wes_palette(8, name = "Zissou1", type = "continuous") -> colpal
        
        } else if (input$colpal == "buda") {
          
          wes_palette(8, name = "GrandBudapest1", type = "continuous") -> colpal
        
        } else if (input$colpal == "royal") {
          
          wes_palette(8, name = "Royal1", type = "continuous") -> colpal
          
        } else if (input$colpal == "dar") {
          
          wes_palette(8, name = "Darjeeling1", type = "continuous") -> colpal
          
        } else if (input$colpal == "moon") {
          
          wes_palette(8, name = "Moonrise1", type = "continuous") -> colpal
          
        }  else if (input$colpal == "color-b") {
          
          brewer.pal(8,"Accent",colorblindFriendly=T) -> colpal
          
        } else {
          
          brewer.pal(8,"Blues") -> colpal
          
        } 
        
        
        
      }) #end color palette
  
  
  
      #create plot output
      output$plot <- renderPlot({
        
        
      #plot colors
      pal <- colpal()
        
      #start plot
      df() -> df_p
      df_p %>%  
      ggplot(aes(x=task,y=laplace)) + 
      geom_bar(aes(fill=task), stat = "identity") +
      geom_errorbar(aes(ymin=lowerci, ymax=upperci, width=.2)) +
      scale_fill_manual(values=pal) +
      coord_cartesian(ylim = c(0,1)) +
      scale_y_continuous(labels = scales::percent) +
      guides(fill=FALSE) +
      theme_minimal() +
      labs(x="Task", y="General estimate of successes") +
      ggtitle(label="Successes by task", subtitle = "Estimated with Laplace method") +
      theme(axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),  
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            title = element_text(size=18))
    
    
  }) #end table render
  
  
  
  
  #plot table output with reactive df
  output$table <- renderTable({
    
    #get data from reactive expression
    df() -> df_t
      
      
    
    
    df_t %>%
      select("Task" = task,
             "Exact Proportion" = prop,
             "LaPlace Proportion" = laplace,
             "Lower CI" = lowerci,
             "Upper CI" = upperci
      )-> df_t
    
    
  }) #end table render
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

