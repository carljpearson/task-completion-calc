#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Big plans:
# Add group possibilities (persona types, software versions, competitive analysis... etc)

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(RColorBrewer)
library(plotly)


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Task Completion Calculator"),
  
  #side panel -----
  column(
    4,
    fluidRow(
      
      wellPanel(
        #subtitle
        h4("Enter your study data below"),
        hr(),
        
        #initial data parameters ----
        numericInput(
          inputId = "task_num",
          "Number of tasks in the study",
          min = 1,
          max = 8,
          value = 2
        )
        ,
        
        
        numericInput(
          inputId = "total",
          "Number of users tested",
          min = 2,
          max = 50,
          value = 10
        )
        ,
        
        #start completion sliders -----
        hr(),
        h4("Enter task success rates below for each task"),
        hr(),
        #task 1 passed input
        sliderInput(
          inputId = "pass1",
          "Task 1 Successes",
          min = 0,
          max = 50,
          value = 10,
          ticks = FALSE
        )
        ,
        #task 2 passed input (conditional)
        conditionalPanel(
          condition = "input.task_num >= 2",
          sliderInput(
            inputId = "pass2",
            "Task 2 Successes",
            min = 0,
            max = 50,
            value = 10,
            ticks = FALSE
          )
        )
        ,
        #task 3 passed input (conditional)
        conditionalPanel(
          condition = "input.task_num >= 3",
          sliderInput(
            inputId = "pass3",
            "Task 3 Successes",
            min = 0,
            max = 50,
            value = 10,
            ticks = FALSE
          )
        )
        ,
        #task 4 passed input (conditional)
        conditionalPanel(
          condition = "input.task_num >= 4",
          sliderInput(
            inputId = "pass4",
            "Task 4 Successes",
            min = 0,
            max = 50,
            value = 10,
            ticks = FALSE
          )
        ),
        
        #task 5 passed input (conditional)
        conditionalPanel(
          condition = "input.task_num >= 5",
          sliderInput(
            inputId = "pass5",
            "Task 5 Successes",
            min = 0,
            max = 50,
            value = 10,
            ticks = FALSE
          )
        ),
        
        #task 6 passed input (conditional)
        conditionalPanel(
          condition = "input.task_num >= 6",
          sliderInput(
            inputId = "pass6",
            "Task 6 Successes",
            min = 0,
            max = 50,
            value = 10,
            ticks = FALSE
          )
        ),
        
        #task 7 passed input (conditional)
        conditionalPanel(
          condition = "input.task_num >= 7",
          sliderInput(
            inputId = "pass7",
            "Task 7 Successes",
            min = 0,
            max = 50,
            value = 10,
            ticks = FALSE
          )
        ),
        
        #task 8 passed input (conditional)
        conditionalPanel(
          condition = "input.task_num >= 8",
          sliderInput(
            inputId = "pass8",
            "Task 8 Successes",
            min = 0,
            max = 50,
            value = 10,
            ticks = FALSE
          )
        ),
        
        #advanced panel open -----
        prettySwitch(
          inputId = "adv",
          label = "View advanced options",
          status = "primary",
          slim = TRUE
        ),
        
        
        #advanced options -----
        
        #z value selection
        conditionalPanel(
          condition = "output.adv_out",
          selectInput(
            inputId = "zval",
            "Confidence level selection:",
            choices = c(
              "80%" = 1.28,
              "85%" = 1.44,
              "90%" = 1.64,
              "95%" = 1.96,
              "99%" = 2.58
            ),
            selected = "1.96"
          )
        ),
        
        
        
        
        #point estimate selection
        conditionalPanel(
          condition = "output.adv_out",
          radioButtons(
            inputId = "point_est",
            "Point estimate calculation method:",
            choices = c("LaPlace", "Exact"),
            selected = "LaPlace"
          )
        )
        
        
        
      ) #end well panel
    )
  ),
  #end sidebar
  
  
  #main panel-----
  
  # Show a plot of the generated distribution ----
  column(
    8,
    
    #show main graph
    plotlyOutput("plot"),
    
    hr(),
    
    fluidRow(#plot options
      
      column(4,
             
            
             
             
             
             
                    conditionalPanel(
                      condition = "output.plot_adv_out",
                      p("Add benchmark line:")
                    ),
                    conditionalPanel(
                      condition = "output.plot_adv_out",
                      #toggle abline
                      switchInput(
                        inputId = "abline"
                      )
                    )
              ),
             
             #color chooser
             column(4, 
            
                    conditionalPanel(
                      condition = "output.plot_adv_out",
                      selectizeInput(
                        inputId = "colpal",
                        "Choose color palette:",
                        choices = c(
                          "Red Hat Brand" = "rh",
                          "Color Blind Friendly" = "color-b",
                          "Gray Alternating" = "single"
                        ),
                        selected = "rh"
                      )
                    )
             
             
      )
    
              
              
    ), #end dl fluid flow
    hr(),
    
    
    fluidRow( #text fluidrow -----
              
              
              column(
                5,
                #for text options and output
                
                #toggle text
                prettySwitch(
                  inputId = "text_view",
                  label = "View graph descriptions",
                  status = "primary",
                  slim = TRUE
                )
              ),
              
              column(3,
                
                
                #toggle table
                prettySwitch(
                  inputId = "table_view",
                  label = "View data table",
                  status = "primary",
                  slim=TRUE
                )
                
                
                
              )
    ),
    #end graph fluidrow
    
    fluidRow(#table fluid row -----
             column(
               8,
               #for datatable options and output
              
               #show datatable
               conditionalPanel(
                 condition = "output.table_out",
                 h4("Data used to produce graph"),
                 tableOutput("table")
               ),
               
               #text output
               conditionalPanel(condition = "output.text_out",
                                tableOutput("text"))
             ) #end table col),
             
    )
  ) #end fluidpage
)

# Define server logic r ----
server <- function(input, output, session) {
  #switch controls----
  
  #adv data
  output$adv_out <- reactive({
    input$adv == TRUE
  })
  outputOptions(output, "adv_out", suspendWhenHidden = FALSE)
  #text
  output$text_out <- reactive({
    input$text_view == TRUE
  })
  outputOptions(output, "text_out", suspendWhenHidden = FALSE)
  #table
  output$table_out <- reactive({
    input$table_view == TRUE
  })
  outputOptions(output, "table_out", suspendWhenHidden = FALSE)
  
  #adv plot
  output$plot_adv_out <- reactive({
    input$plot_adv == TRUE
  })
  outputOptions(output, "plot_adv_out", suspendWhenHidden = FALSE)
  
  #adv dl
  output$dl_adv_out <- reactive({
    input$dl_adv == TRUE
  })
  outputOptions(output, "dl_adv_out", suspendWhenHidden = FALSE)
  
  
  
  #change slider end points -----
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
  
  
  #build df for all plots/tables -----
  df <- reactive({
    if (input$task_num == 8) {
      pass <-
        c(
          input$pass1,
          input$pass2,
          input$pass3,
          input$pass4,
          input$pass5,
          input$pass6,
          input$pass7,
          input$pass8
        )
      task <- c(1:8)
      df_t <- data.frame(pass, task)
      
    } else if (input$task_num == 7) {
      pass <-
        c(
          input$pass1,
          input$pass2,
          input$pass3,
          input$pass4,
          input$pass5,
          input$pass6,
          input$pass7
        )
      task <- c(1:7)
      df_t <- data.frame(pass, task)
      
    } else if (input$task_num == 6) {
      pass <-
        c(input$pass1,
          input$pass2,
          input$pass3,
          input$pass4,
          input$pass5,
          input$pass6)
      task <- c(1:6)
      df_t <- data.frame(pass, task)
      
    } else if (input$task_num == 5) {
      pass <-
        c(input$pass1,
          input$pass2,
          input$pass3,
          input$pass4,
          input$pass5)
      task <- c(1:5)
      df_t <- data.frame(pass, task)
      
    } else if (input$task_num == 4) {
      pass <- c(input$pass1, input$pass2, input$pass3, input$pass4)
      task <- c(1:4)
      df_t <- data.frame(pass, task)
      
    } else if (input$task_num == 3) {
      pass <- c(input$pass1, input$pass2, input$pass3)
      task <- c(1:3)
      df_t <- data.frame(pass, task)
      
    } else if (input$task_num == 2) {
      pass <- c(input$pass1, input$pass2)
      task <- c(1:2)
      df_t <- data.frame(pass, task)
      
    } else {
      pass <- c(input$pass1)
      task <- c(1)
      df_t <- data.frame(pass, task)
      
      
    }
    
    #get zval as numeric
    zval <- as.numeric(input$zval)
    
    #z value for exact when values are 0 or 1
    
    if (input$zval == 2.58) {
      z_one_sided <- 2.33
    } else if (input$zval == 1.96) {
      z_one_sided <- 1.65
    } else if (input$zval == 1.64) {
      z_one_sided <- 1.28
    } else if (input$zval == 1.44) {
      z_one_sided <- 1.04
    } else {
      z_one_sided <- .84
    }
    
    #calc df
    df_t %>%
      mutate(total = input$total) %>%  #get n col
      mutate(prop = pass / total) %>% #exact proportion
      mutate(laplace = (pass + 1) / (total + 2)) %>% #laplace point
      mutate(p_adj = (total * prop + (zval * zval) / 2) / (total + (zval *
                                                                      zval))) %>% #adjust p for wald
      mutate(n_adj = total + (zval * zval)) %>% #adjust n for wald
      mutate(adj_wald_marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj)) %>% #wald margin value
      mutate(lowerci = p_adj - adj_wald_marg) %>% #lower wald ci
      mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>%
      mutate(lowerci_plotly = laplace - lowerci) %>% #special col as plotly error bar must reference y value
      mutate(upperci = p_adj + adj_wald_marg) %>% #upper wald ci
      mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>%
      mutate(upperci_plotly = upperci - laplace) %>% #special col as plotly error bar must reference y value
      mutate(task = as.factor(trunc(task, digits = 1))) -> df
    
    
  }) #end df creation
  
  
  
  
  #color palette generator -----
  
  colpal <- reactive({
    
    f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
    
    if (input$colpal == "rh") {
      c(
        "#cc0000",
        "#0088ce",
        "#f0ab00",
        "#92d400",
        "#00b9e4",
        "#6C44A0",
        "#007a87",
        "#f0ab00"
      ) -> colpal
      
    } else if (input$colpal == "color-b") {
      brewer.pal(8, "Accent") -> colpal
      
    } else {
      
      c(
        "#dbdbdb",
        "#3f3f3f",
        "#dbdbdb",
        "#3f3f3f",
        "#dbdbdb",
        "#3f3f3f",
        "#dbdbdb",
        "#3f3f3f"
      ) -> colpal
      
    }
    
    
    
  }) #end color palette
  
  
  
  #naming z value perc ------
  zp <- reactive({
    if (input$zval == 2.58) {
      zp <- "99%"
    } else if (input$zval == 1.96) {
      zp <- "95%"
    } else if (input$zval == 1.64) {
      zp <- "90%"
    } else if (input$zval == 1.44) {
      zp <- "85%"
    } else {
      zp <- "80%"
    }
  })
  #create plot object -----
  plotInput <- reactive({
    req(input$task_num)
    task_num <- input$task
    #get z percentage for name
    zp <- zp()
    
    #plot colors
    pal <- colpal()
    pal[1:input$task_num] -> pal
    #start plot
    
    df() -> df_p
    #ifelse based on excat or laplace
    
    
    if (input$point_est == "LaPlace" & input$abline == FALSE) {
     
     
      plot <- df_p %>% #get df
        plot_ly(x = ~task, #x is task variable
                y = ~laplace, #y is laplace est
                type = "bar", #it's a bar graph
                hoverinfo = 'text', #define var name for tooltip formatting
                text = ~paste0( #tool tip formatting
                  'Task: ', task,
                  '<br> Success rate estimate: ', ( 100*round(laplace,2)),"%", #laplace rounded
                  '<br> Observed success rate: ',(100*(round(prop,2))),"%",    #actual proportion 
                  '<br> Plausible failure rate: ',(100*(1-round(lowerci,2))),"%" #lower CI subtracted from 1
                  
                ),
                error_y = list(type = "data", #starting error bar
                               symmetric = FALSE, #upper and lower bars are different
                               array = ~upperci_plotly, #special variable as plotly needs to add based on y val
                               arrayminus = ~lowerci_plotly, #special variable as plotly needs to add based on y val
                               color = "black" 
                ),
                color = ~task, #bar color 
                colors = pal   #palettle to draw from
        ) %>%
        layout(title = paste0("Success Rates by Task ", "(Confidence at ",zp,")"), #title that includes z value for conf
               yaxis = list(tickformat = "%", #convert y axis to %
                            range = c(0,1.01), #specify y range, 1.01 because tick doesn't appear otherwise
                            tickvals = c(.2,.4,.6,.8,1), #specify ticks
                            title = "Success Rate" #retitle y
               ),
               xaxis = list(title = "Task" #retitle x axis
               )
        )
      
      
      
    } else if (input$point_est == "LaPlace" & input$abline == TRUE) {
      
      plot <- df_p %>% #get df
        plot_ly(x = ~task, #x is task variable
                y = ~laplace, #y is laplace est
                type = "bar", #it's a bar graph
                hoverinfo = 'text', #define var name for tooltip formatting
                text = ~paste0( #tool tip formatting
                  'Task: ', task,
                  '<br> Success rate estimate: ', ( 100*round(laplace,2)),"%", #laplace rounded
                  '<br> Observed success rate: ',(100*(round(prop,2))),"%",    #actual proportion 
                  '<br> Plausible failure rate: ',(100*(1-round(lowerci,2))),"%" #lower CI subtracted from 1
                  
                ),
                error_y = list(type = "data", #starting error bar
                               symmetric = FALSE, #upper and lower bars are different
                               array = ~upperci_plotly, #special variable as plotly needs to add based on y val
                               arrayminus = ~lowerci_plotly, #special variable as plotly needs to add based on y val
                               color = "black" 
                ),
                color = ~task, #bar color 
                colors = pal   #palettle to draw from
        ) %>%
        layout(title = paste0("Success Rates by Task ", "(Confidence at ",zp,")"), #title that includes z value for conf
               yaxis = list(tickformat = "%", #convert y axis to %
                            range = c(0,1.01), #specify y range, 1.01 because tick doesn't appear otherwise
                            tickvals = c(.2,.4,.6,.8,1), #specify ticks
                            title = "Success Rate" #retitle y
               ),
               xaxis = list(title = "Task" #retitle x axis
               ),
               shapes=list(type='line', #horizontal line
                           y0 = 0.78,   #y height
                           y1 = 0.78,   #y height
                           xref = "paper", #unsure, I think this makes it not stop at bar center
                           x0 = 0, #starting beyond the left
                           x1 = 1, #working, unsure how this fits with paper argument right now though
                           line=list(dash='dot', #dotted line
                                     width=3,
                                     color="gray"
                                    )
                          )
        )
      
    } else if (input$point_est == "Exact" & input$abline == TRUE ) {
      
      plot <- df_p %>% #get df
        plot_ly(x = ~task, #x is task variable
                y = ~prop, #y is laplace est
                type = "bar", #it's a bar graph
                hoverinfo = 'text', #define var name for tooltip formatting
                text = ~paste0( #tool tip formatting
                  'Task: ', task,
                  '<br> Success rate estimate: ', ( 100*round(laplace,2)),"%", #laplace rounded
                  '<br> Observed success rate: ',(100*(round(prop,2))),"%",    #actual proportion 
                  '<br> Plausible failure rate: ',(100*(1-round(lowerci,2))),"%" #lower CI subtracted from 1
                  
                ),
                color = ~task, #bar color 
                colors = pal,   #palettle to draw from
                legendgroup = 'Exact Bars'
        ) %>%
        add_trace(x=~task,
                  y=~laplace,
                  type = "scatter",
                  mode = "markers",
                  marker = list(size = 10,
                                color = "black"
                  ),
                  error_y = list(type = "data", #starting error bar
                                 symmetric = FALSE, #upper and lower bars are different
                                 array = ~upperci_plotly, #special variable as plotly needs to add based on y val
                                 arrayminus = ~lowerci_plotly, #special variable as plotly needs to add based on y val
                                 color = "black" 
                  ),
                  hoverinfo='none', #no hover on error bars and laplace
                  legendgroup = 'LaPlace and Error Bars'
                  
        ) %>%
        layout(title = paste0("Success Rates by Task ", "(Confidence at ",zp,")"), #title that includes z value for conf
               yaxis = list(tickformat = "%", #convert y axis to %
                            range = c(0,1.01), #specify y range, 1.01 because tick doesn't appear otherwise
                            tickvals = c(.2,.4,.6,.8,1), #specify ticks
                            title = "Success Rate" #retitle y
                            ),
               xaxis = list(title = "Task" #retitle x axis
                            ),
               shapes=list(type='line', #horizontal line
                           y0 = 0.78,   #y height
                           y1 = 0.78,   #y height
                           xref = "paper", #unsure, I think this makes it not stop at bar center
                           x0 = 0, #starting beyond the left
                           x1 = 1, #working, unsure how this fits with paper argument right now though
                           line=list(dash='dot', #dotted line
                                     width=3,
                                     color="gray"
                           )
               )
        )
      
      
    } else {
      
      plot <- df_p %>% #get df
        plot_ly(x = ~task, #x is task variable
                y = ~prop, #y is laplace est
                type = "bar", #it's a bar graph
                hoverinfo = 'text', #define var name for tooltip formatting
                text = ~paste0( #tool tip formatting
                  'Task: ', task,
                  '<br> Success rate estimate: ', ( 100*round(laplace,2)),"%", #laplace rounded
                  '<br> Observed success rate: ',(100*(round(prop,2))),"%",    #actual proportion 
                  '<br> Plausible failure rate: ',(100*(1-round(lowerci,2))),"%" #lower CI subtracted from 1
                  
                ),
                color = ~task, #bar color 
                colors = pal,   #palettle to draw from
                legendgroup = 'Exact Bars'
        ) %>%
        add_trace(x=~task,
                  y=~laplace,
                  type = "scatter",
                  mode = "markers",
                  marker = list(size = 10,
                                color = "black"
                  ),
                  error_y = list(type = "data", #starting error bar
                                 symmetric = FALSE, #upper and lower bars are different
                                 array = ~upperci_plotly, #special variable as plotly needs to add based on y val
                                 arrayminus = ~lowerci_plotly, #special variable as plotly needs to add based on y val
                                 color = "black" 
                  ),
                  hoverinfo='none', #no hover on error bars and laplace
                  legendgroup = 'LaPlace and Error Bars'
                  
        ) %>%
        layout(title = paste0("Success Rates by Task ", "(Confidence at ",zp,")"), #title that includes z value for conf
               yaxis = list(tickformat = "%", #convert y axis to %
                            range = c(0,1.01), #specify y range, 1.01 because tick doesn't appear otherwise
                            tickvals = c(.2,.4,.6,.8,1), #specify ticks
                            title = "Success Rate" #retitle y
               ),
               xaxis = list(title = "Task" #retitle x axis
               )
        )
      
      
    }#end ifelse of point est
  }) #end table render
  
  
  #plot output -----
  output$plot <- renderPlotly({
    p <- plotInput()
    
  })
  
  
  
  #table output ----
  output$table <- renderTable({
    #get data from reactive expression
    df() -> df_t
    
    
    
      df_t %>%
        select(
          "Task" = task,
          "Exact Proportion" = prop,
          "LaPlace Statitistical Proportion" = laplace,
          "Lower CI" = lowerci,
          "Upper CI" = upperci
        ) -> df_t
    
    
  }) #end table render
  
  
  #text output -----
  output$text <- renderTable({
    df() -> df_text
    zp() -> zp
    
    
    
    data.frame(
      paste(
        "For task ",
        df_text$task,
        ", we observed ",
        df_text$pass,
        " out of ",
        df_text$total,
        " participants succeed in the task goal, but our best estimate is that ",
        (100 * round(df_text$laplace, digits = 2)),
        "% of users will succeed in general.",
        "  To consider this more conservatively, given our confidence level of ",
        zp,
        " and sample size of ",
        df_text$total,
        ", we can plausibly expect at that up to ",
        (100 * round((
          1 - df_text$lowerci
        ), digits = 2)),
        "% of users will not be able to succeed in the task.",
        sep = ""
      )
    ) -> text.output
    text.output %>% rename(" " = !!names(.[1])) -> text.output
    
    
  }) #end print render
  

  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
