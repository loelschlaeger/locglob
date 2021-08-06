### clear environment, load code and libraries
rm(list = ls())
try(setwd("RMACML"),silent=TRUE)
for(file in list.files(path="LO/ll_visual/code",pattern=".R")) 
  source(paste0("LO/ll_visual/code/",file))
library(ggplot2)
library(shiny)
library(xtable)

ui = fluidPage(
  theme = shinythemes::shinytheme("paper"),
  titlePanel("Explorer of probit log-likelihood"),
  fluidRow(
    column(6,
      plotOutput("contour")
    ),
    column(6,
      tabsetPanel(
        tabPanel("theta elements",
           column(4,tableOutput("parameter_b")),
           column(4,tableOutput("parameter_o")),
           column(4,tableOutput("parameter_l"))
        ),
        tabPanel("model parameter",
           uiOutput("model_parameter_b"),
           uiOutput("model_parameter_Omega"),
           uiOutput("model_parameter_Sigma")
        )
      )
    )
  ),
  
  hr(),
  fluidRow(
    column(3,
      sliderInput("N_value", "number of decision makers", min = 1, max = 100, value = 10, ticks = FALSE),
      sliderInput("T_value", "number of choice occasions", min = 1, max = 10, value = 1, ticks = FALSE)
    ),
    column(3,
      sliderInput("J_value", "number of choice alternatives", min = 2, max = 5, value = 3, ticks = FALSE),
      sliderInput("P_value", "number of choice attributes", min = 1, max = 5, value = 2, ticks = FALSE)
    ),
    column(3,
      uiOutput("flex_par_1"),
      sliderInput("flex_par_1_range", label = "range", min = -10, max = 10, value = c(-5, 5))
    ),
    column(3,
      uiOutput("flex_par_2")   ,
      sliderInput("flex_par_2_range", label = "range", min = -10, max = 10, value = c(-5, 5))
    )
  )
)

server = function(input, output, session) {
  
  flex_par_1_range_reactive = reactive({
    input$flex_par_1_range
  })
  
  flex_par_2_range_reactive = reactive({
    input$flex_par_2_range
  })
  
  flex_par_reactive = reactive({
    par  = sim_par_reactive()
    flex_1 = unlist(strsplit(par_table(par)$table_all$parameter[as.numeric(input$flex_par_1)],split=""))
    flex_2 = unlist(strsplit(par_table(par)$table_all$parameter[as.numeric(input$flex_par_2)],split=""))
    list(par = c(flex_1[1],flex_2[1]),index = as.numeric(c(flex_1[2],flex_2[2])))
  })
  
  sim_par_reactive = reactive({
    sim_par(J = input$J_value,
            P = input$P_value)
  })
  
  sim_data_reactive = reactive({
    sim_data(N    = input$N_value,
             T    = input$T_value,
             J    = input$J_value,
             P    = input$P_value,
             par  = sim_par_reactive(),
             seed = 1)
  })
  
  output$flex_par_1 = renderUI({
    name_choices = par_table(sim_par_reactive())$table_all$parameter
    choices = as.list(1:length(name_choices))
    names(choices) = name_choices
    selectInput("flex_par_1", "flexible parameter 1", 
                choices = choices, 
                selected = 1)
  })
  
  output$flex_par_2 = renderUI({
    name_choices = par_table(sim_par_reactive())$table_all$parameter
    choices = as.list(1:length(name_choices))
    names(choices) = name_choices
    selectInput("flex_par_2", "flexible parameter 2", 
                choices = choices, 
                selected = 2)
  })
  
  output$contour = renderPlot({
    
    validate(
      need(input$flex_par_1 != "", ""), 
      need(input$flex_par_2 != "", "") 
    )
    
    par  = sim_par_reactive()
    data = sim_data_reactive()
    x_seq = flex_par_1_range_reactive()
    y_seq = flex_par_2_range_reactive()
    grid = make_grid(x    = seq(x_seq[1],x_seq[2],length.out=20),
                     y    = seq(y_seq[1],y_seq[2],length.out=20),
                     par  = par,
                     flex = flex_par_reactive(),
                     data = data)
    make_contour(grid,par)
  })
  
  output$parameter_b = renderTable({
    par_table(sim_par_reactive())$table_b
  })
  
  output$parameter_o = renderTable({
    par_table(sim_par_reactive())$table_o
  })
  
  output$parameter_l = renderTable({
    par_table(sim_par_reactive())$table_l
  })
  
  output$model_parameter_b = renderUI({
    M <- as.matrix(sim_par_reactive()$b)
    M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    html <- paste0("$$", M, "$$")
    list(
      withMathJax(HTML(html))
    )
  })
  
  output$model_parameter_Omega = renderUI({
    M <- sim_par_reactive()$Omega
    M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    html <- paste0("$$", M, "$$")
    list(
      withMathJax(HTML(html))
    )
  })
  
  output$model_parameter_Sigma = renderUI({
    M <- sim_par_reactive()$Sigma
    M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    html <- paste0("$$", M, "$$")
    list(
      withMathJax(HTML(html))
    )
  })
}

shinyApp(ui = ui, server = server)
