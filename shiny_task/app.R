#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Big plans:
# Add group possibilities (persona types, software versions, competitive analysis... etc)

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(wesanderson)
library(RColorBrewer)


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
          min = 1,
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
      plotOutput("plot"),
      
      hr(),
      
      fluidRow(#plot options
        
        column(8,
               
               #plot advanved options open -----
               prettySwitch(
                 inputId = "plot_adv",
                 label = "View plot options",
                 status = "primary",
                 slim = TRUE
               ),
               
               
               
               column(5,
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
                     "The Life Aquatic" = "ziz",
                     "The Grand Budapest Hotel" = "buda",
                     "The Royal Tenenbaums" = "royal",
                     "The Darjeeling Limited" = "dar",
                     "Moonrise Kingdom" = "moon",
                     "Color Blind Friendly" = "color-b",
                     "Gray Alternating" = "single"
                   ),
                   selected = "rh"
                 )
               )
               )
              
        )
      ), #end plot options row
      
      hr(),
      fluidRow( #dl fluid row ------
        
                    
                
        column(
          3,
          #download button
          downloadButton("download", 
                         "Download Plot"),
          br(), br(),
          #plot advanved options open -----
          prettySwitch(
            inputId = "dl_adv",
            label = "Download options",
            status = "primary",
            slim = TRUE
          )   
          
        ), #end dl col) #dl fluid row end
        column(
          4,
          #options
          conditionalPanel(
            condition = "output.dl_adv_out",
          radioGroupButtons(
            inputId = "bg",
            label = "Background:",
            choices = c("Transparent", 
                        "White"),
            justified = TRUE
          )
          )
        ) #end dl col) #dl fluid row end
        
        
        
      ), #end dl fluid flow
      hr(),
      
      
      fluidRow( #text fluidrow -----
        
        
        column(
          8,
          #for text options and output
          
          #toggle text
          prettySwitch(
            inputId = "text_view",
            label = "View graph descriptions",
            status = "primary",
            slim = TRUE
          ),
          
          #text output
          conditionalPanel(condition = "output.text_out",
                           tableOutput("text"))
          
        )
      ),
        #end graph fluidrow
        
        fluidRow(#table fluid row -----
          column(
            8,
            #for datatable options and output
            #toggle table
            prettySwitch(
              inputId = "table_view",
              label = "View data table",
              status = "primary",
              slim=TRUE
            ),
            
            #show datatable
            conditionalPanel(
              condition = "output.table_out",
              h4("Data used to produce graph"),
              tableOutput("table")
            )
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
              mutate(upperci = p_adj + adj_wald_marg) %>% #upper wald ci
              mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>%
              mutate(task = as.factor(trunc(task, digits = 1))) -> df
            
            
          }) #end df creation
          
          
          
          
          #color palette generator -----
          
          colpal <- reactive({
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
              brewer.pal(8, "Accent") -> colpal
              
            } else {
              c(
                "#808080",
                "#2a2a2a",
                "#808080",
                "#2a2a2a",
                "#808080",
                "#2a2a2a",
                "#808080",
                "#2a2a2a"
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
            #get z percentage for name
            zp <- zp()
            
            
            
            
            #plot colors
            pal <- colpal()
            
            #start plot
            
            df() -> df_p
            #ifelse based on excat or laplace
            
            
            if (input$point_est == "LaPlace" & input$abline == FALSE) {
              df_p %>%
                ggplot(aes(x = task, y = laplace)) +
                geom_bar(aes(fill = task), stat = "identity") +
                geom_errorbar(aes(
                  ymin = lowerci,
                  ymax = upperci,
                  width = .2
                )) +
                scale_fill_manual(values = pal) +
                coord_cartesian(ylim = c(0, 1)) +
                scale_y_continuous(labels = scales::percent) +
                guides(fill = FALSE) +
                theme_minimal() +
                labs(x = "Task", y = "Success rate proportion") +
                ggtitle(label = "Estimates of success rate by task",
                        subtitle = paste("Confidence Intervals at", zp)) +
                theme(
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15),
                  title = element_text(size = 18)
                )
            
            
            } else if (input$point_est == "LaPlace" & input$abline == TRUE) {
              df_p %>%
                ggplot(aes(x = task, y = laplace)) +
                geom_bar(aes(fill = task), stat = "identity") +
                geom_errorbar(aes(
                  ymin = lowerci,
                  ymax = upperci,
                  width = .2
                )) +
                scale_fill_manual(values = pal) +
                coord_cartesian(ylim = c(0, 1)) +
                geom_abline(intercept=.78,slope=0, color = "gray",linetype = 2, size=2)+
                scale_y_continuous(labels = scales::percent) +
                guides(fill = FALSE) +
                theme_minimal() +
                labs(x = "Task", y = "Success rate proportion") +
                ggtitle(label = "Estimates of success rate by task",
                        subtitle = paste("Confidence Intervals at", zp)) +
                theme(
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15),
                  title = element_text(size = 18)
                )
            } else if (input$point_est == "Exact" & input$abline == TRUE ) {
            
              df_p %>%
                ggplot(aes(x = task, y = prop)) +
                geom_bar(aes(fill = task), stat = "identity") +
                geom_errorbar(aes(
                  ymin = lowerci,
                  ymax = upperci,
                  width = .2
                )) +
                scale_fill_manual(values = pal) +
                coord_cartesian(ylim = c(0, 1)) +
                geom_abline(intercept=.78,slope=0, color = "gray", linetype = 2, size=2)+
                geom_point(aes(y=laplace),size=8) +
                scale_y_continuous(labels = scales::percent) +
                guides(fill = FALSE) +
                theme_minimal() +
                labs(x = "Task", y = "Success rate proportion") +
                ggtitle(
                  label = "Exact observered proportions",
                  subtitle = paste(
                    "Confidence Intervals at",
                    zp,
                    "and black points indicate statistical best estimates"
                  )
                ) +
                theme(
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15),
                  title = element_text(size = 18)
                )
                  
            } else {
              df_p %>%
                ggplot(aes(x = task, y = prop)) +
                geom_bar(aes(fill = task), stat = "identity") +
                geom_errorbar(aes(
                  ymin = lowerci,
                  ymax = upperci,
                  width = .2
                )) +
                scale_fill_manual(values = pal) +
                coord_cartesian(ylim = c(0, 1)) +
                geom_point(aes(y=laplace),size=8) +
                scale_y_continuous(labels = scales::percent) +
                guides(fill = FALSE) +
                theme_minimal() +
                labs(x = "Task", y = "Success rate proportion") +
                ggtitle(
                  label = "Exact observered proportions",
                  subtitle = paste(
                    "Confidence Intervals at",
                    zp,
                    "and black points indicate statistical best estimates"
                  )
                ) +
                theme(
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15),
                  title = element_text(size = 18)
                )
              
            }#end ifelse of point est
          }) #end table render
          
          
          #plot output -----
          output$plot <- renderPlot({
            print(plotInput())
          })
          
          
          
          #table output ----
          output$table <- renderTable({
            #get data from reactive expression
            df() -> df_t
            
            
            #change table based on CI for bounds of exact
            if (input$point_est == "LaPlace") {
              df_t %>%
                select(
                  "Task" = task,
                  "Exact Proportion" = prop,
                  "LaPlace Proportion" = laplace,
                  "Lower CI" = lowerci,
                  "Upper CI" = upperci
                ) -> df_t
            } else {
              df_t %>%
                select(
                  "Task" = task,
                  "Exact Proportion" = prop,
                  "LaPlace Statitistical Proportion" = laplace,
                  "Lower CI" = lowerci,
                  "Upper CI" = upperci
                ) -> df_t
              
            }
            
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
          
          #saving plot ----
          
          bg_out <- reactive({
            ifelse(input$bg == "Transparent", "transparent","white")
          })
          
        
          
          output$download <- downloadHandler(
            filename = function() {
              paste("my_plot_", nrow(df()), "tasks", ".png", sep = "")
            },
            content = function(file) {
              ggsave(
                file,
                plot = plotInput(),
                device = "png",
                bg =  bg_out(),
                width = 8,
                height = 5
              )
            }
          ) #end plot saver
          
          
          
          
        }
        
        # Run the application
        shinyApp(ui = ui, server = server)
        