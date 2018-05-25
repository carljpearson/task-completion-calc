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
                   max = 4,
                   value = 2)
      ,
      
      
      numericInput(inputId = "total",
                   "Number of users tested",
                   min = 1,
                   max = 50,
                   value = 10)
      ,
      
      #task 1 passed input
      sliderInput(inputId = "pass1",
                  "Number of users that succeeded in task 1",
                  min = 1,
                  max = 50,
                  value = 10,
                  ticks = FALSE)
      ,
      #task 2 passed input (conditional)
      conditionalPanel(
        condition = "input.task_num >= 2",
        sliderInput(inputId = "pass2",
                    "Number of users that succeeded in task 2",
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
                    "Number of users that succeeded in task 3",
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
                    "Number of users that succeeded in task 4",
                    min = 1,
                    max = 50,
                    value = 10,
                    ticks = FALSE)
      )
      
      
      #end sidebar
    ), 
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define server logic r
server <- function(input, output, session) {
  
  
  #change slider end points
  observe({
    val <- input$total
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "pass1", max = val)
    updateSliderInput(session, "pass2", max = val)
    updateSliderInput(session, "pass3", max = val)
    updateSliderInput(session, "pass4", max = val)
  })
  
  
  #plot table output with reactive df
  output$plot <- renderPlot({
    if (input$task_num == 4) {
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
    pal <- wes_palette(4, name = "Zissou1", type = "discrete")
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
      mutate(task=as.factor(trunc(task,digits=1)) ) %>%
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
    if (input$task_num == 4) {
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
      mutate(task=as.factor(trunc(task,digits=1)) ) %>%
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

