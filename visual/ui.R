# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(
  navbarPage("Simulation",
             tabPanel("Plots",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("model", 
                                      label = "Model",
                                      choices = list("All" = 0,
                                                     "N(0,1)" = 1,
                                                     "0.9N(0,1) + 0.1N(0,4)" = 2,
                                                     "0.8N(0,1) + 0.2N(0,4)" = 3,
                                                     "(n-1)N(0,1) + 1N(0,4)" = 4,
                                                     "(n-2)N(0,1) + 2N(0,4)" = 5,
                                                     "Logistic" = 6,
                                                     "Laplace" = 7),
                                      selected = 0),
                          selectInput("size", 
                                      label = "Sample Size",
                                      choices = list("All" = 0,
                                                     "10" = 10,
                                                     "20" = 20,
                                                     "30" = 30,
                                                     "50" = 50),
                                      selected = 0),
                          selectInput("estimator", 
                                      label = "Estimator",
                                      choices = list("All" = 0,
                                                     "Mean" = 1,
                                                     "T0.1" = 2,
                                                     "T0.2" = 3,
                                                     "W0.1" = 4,
                                                     "W0.2" = 5,
                                                     "L0.1" = 6,
                                                     "L0.2" = 7,
                                                     "Median" = 8),
                                      selected = 0),
                          sliderInput("signif", 
                                      label = "Significant digits",
                                      min = 1,
                                      max = 6,
                                      value = 3),
                          hr(),
                          h5("m1 = N(0,1)"),
                          h5("m2 = 0.9N(0,1) + 0.1N(0,4)"),
                          h5("m3 = 0.8N(0,1) + 0.2N(0,4)"),
                          h5("m4 = (n-1)N(0,1) + 1N(0,4)"),
                          h5("m5 = (n-2)N(0,1) + 2N(0,4)"),
                          h5("m6 = Logistic"),
                          h5("m7 = Laplace"),
                          plotOutput("ms",
                                     height = "400px")
                         ),
                        mainPanel(
                          plotOutput("bias", height = "350px",
                                     dblclick = "bias_dbclick",
                                     brush = brushOpts(
                                       id = "bias_brush",
                                       resetOnNew = TRUE)
                        ),
                          plotOutput("variances",height = "350px",
                                   dblclick = "variances_dbclick",
                                   brush = brushOpts(
                                     id = "variances_brush",
                                     resetOnNew = TRUE)
                        ),
                          plotOutput("mse",height = "350px",
                                     dblclick = "mse_dbclick",
                                     brush = brushOpts(
                                       id = "mse_brush",
                                       resetOnNew = TRUE)
                          ) 
                       )
                     )        
                   
             ),
             tabPanel("Bias",
                      DTOutput('bias_table')),
             tabPanel("Variance",
                      DTOutput("variances_table")),
             tabPanel("MSE",
                      DTOutput("mse_table"))
))