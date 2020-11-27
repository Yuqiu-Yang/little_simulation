# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(DT)
library(tidyverse)
library(rmutil)
shinyServer(function(input, output,session) {
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  # read data
  load("./Data/bias.RData")
  load("./Data/variances.RData")
  mse <- bias^2 + variances
  n_row <- nrow(bias)
  n_col <- ncol(bias)
  r_names <- rownames(bias)
  c_names <- colnames(bias)
  e_names <- c("Mean",
               "T0.1",
               "T0.2",
               "W0.1",
               "W0.2",
               "L0.1",
               "L0.2",
               "Median")
  m_names <- c("N(0,1)",
              "0.9N(0,1) + 0.1N(0,4)",
              "0.8N(0,1) + 0.2N(0,4)",
              "(n-1)N(0,1) + 1N(0,4)",
              "(n-2)N(0,1) + 2N(0,4)",
              "Logistic",
              "Laplace")
  m_names <- paste(rep(m_names, each = 4), 
                  "size",
                  c(10, 20, 30,50), sep="_")
  
  # get indices
  getModelIndex <- reactive({
    model_c <- as.numeric(input$model)
    if(model_c == 0)
    {
      ind <- 1 : n_row
    }else{
      ind <- which(as.numeric(substr(r_names, 7,7)) == 
                     model_c)
    }
    ind
  })
  
  getSizeIndex <- reactive({
    size_c <- as.numeric(input$size)
    if(size_c == 0)
    {
      ind <- 1 : n_row
    }else{
      ind <- which(as.numeric(substr(r_names, 9,10)) == 
                     size_c)
    }
    ind
  })
  
  
  getEstimatorIndex <- reactive({
    estimator_c <- as.numeric(input$estimator)
    if(estimator_c == 0)
    {
      ind <- 1 : n_col
    }else{
      ind <- estimator_c
    }
    ind
  })
  
  
  getRows <- reactive({
    m_ind <- getModelIndex()
    s_ind <- getSizeIndex()
    ind <- intersect(m_ind, s_ind)
    sort(ind)
  })
  
  getCols <- reactive({
    e_ind <- getEstimatorIndex()
    e_ind
  })
  
  # Plots
  # ui plots
  output$ms <- renderPlot({
    par(mar=c(4, 4, 3, 4), xpd=TRUE)
    x <- seq(-4,4,by = 0.05)
    plot(x, dlaplace(x, s = 1/sqrt(2)),
         col = "violet", type = "l", 
         ylab = "Density")
    lines(x, dlogis(x, scale = 1/(pi/sqrt(3))),
          col = "cyan")
    lines(x, dnorm(x))
    legend("top",inset=c(0,-0.11), 
           legend = c("Normal","Logistic", "Laplace"),
           lty = 1, cex = 0.7, col = c(1, "cyan", "violet"),
           horiz = T)
  })
  
  # server plots
  rangesBias <- reactiveValues(x = NULL, y = NULL)
  
  output$bias <- renderPlot({
    r_ind <- getRows()
    c_ind <- getCols()
    temp <- as.data.frame(bias[r_ind, c_ind])
    if(is.null(rangesBias$y))
    {
      xlim <- c(1 , nrow(temp))
      ylim <- range(temp)
    }else{
      ylim <- rangesBias$y
      xlim <- rangesBias$x
    }
    
    x_tick <- r_names[r_ind]
    x_tick <- paste(substr(x_tick, 1,1), 
          substr(x_tick, 7,7),
          rep("n", length(x_tick)),
          substr(x_tick, 9,10),
          sep="")
    par(mar=c(4, 4, 3, 4), xpd=TRUE)
    plot(temp[,1],
         xlim = xlim,
         ylim = ylim,
         pch = c_ind[1],
         ylab = "Bias",
         xaxt = "n",
         xlab = NA)
    axis(side=1, at=1 : length(x_tick), 
         labels = x_tick, las = 2)
    legend("top",inset=c(0,-0.11), legend = e_names[c_ind],
           pch = c_ind, cex = 0.9,
           horiz = T)
    if(length(c_ind) > 1)
    {
      for(i in 2 : length(c_ind))
      {
        points(temp[,i], pch=c_ind[i])
      }
    }
    
  })
  observeEvent(input$bias_dbclick,{
    brush <- input$bias_brush
    if (!is.null(brush)) {
      rangesBias$x <- c(brush$xmin, brush$xmax)
      rangesBias$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangesBias$x <- NULL
      rangesBias$y <- NULL
    }
  })
  
  
  rangesVariances <- reactiveValues(x = NULL, y = NULL)
  output$variances <- renderPlot({
    r_ind <- getRows()
    c_ind <- getCols()
    temp <- as.data.frame(variances[r_ind, c_ind])
    if(is.null(rangesVariances$y))
    {
      xlim <- c(1 , nrow(temp))
      ylim <- range(temp)
    }else{
      ylim <- rangesVariances$y
      xlim <- rangesVariances$x
    }
    
    x_tick <- r_names[r_ind]
    x_tick <- paste(substr(x_tick, 1,1), 
                    substr(x_tick, 7,7),
                    rep("n", length(x_tick)),
                    substr(x_tick, 9,10),
                    sep="")
    par(mar=c(4, 4, 3, 4), xpd=TRUE)
    plot(temp[,1],
         xlim = xlim,
         ylim = ylim,
         pch = c_ind[1],
         ylab = "Variances",
         xaxt = "n",
         xlab = NA)
    axis(side=1, at=1 : length(x_tick), 
         labels = x_tick, las = 2)
    legend("top",inset=c(0,-0.11), legend = e_names[c_ind],
           pch = c_ind, cex = 0.9,
           horiz = T)
    if(length(c_ind) > 1)
    {
      for(i in 2 : length(c_ind))
      {
        points(temp[,i], pch=c_ind[i])
      }
    }
  })
  observeEvent(input$variances_dbclick,{
    brush <- input$variances_brush
    if (!is.null(brush)) {
      rangesVariances$x <- c(brush$xmin, brush$xmax)
      rangesVariances$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangesVariances$x <- NULL
      rangesVariances$y <- NULL
    }
  })
  
  rangesMSE <- reactiveValues(x = NULL, y = NULL)
  output$mse <- renderPlot({
    r_ind <- getRows()
    c_ind <- getCols()
    temp <- as.data.frame(mse[r_ind, c_ind])
    if(is.null(rangesMSE$y))
    {
      xlim <- c(1 , nrow(temp))
      ylim <- range(temp)
    }else{
      ylim <- rangesMSE$y
      xlim <- rangesMSE$x
    }
    
    x_tick <- r_names[r_ind]
    x_tick <- paste(substr(x_tick, 1,1), 
                    substr(x_tick, 7,7),
                    rep("n", length(x_tick)),
                    substr(x_tick, 9,10),
                    sep="")
    par(mar=c(4, 4, 3, 4), xpd=TRUE)
    plot(temp[,1],
         xlim = xlim,
         ylim = ylim,
         pch = c_ind[1],
         ylab = "MSE",
         xaxt = "n",
         xlab = NA)
    axis(side=1, at=1 : length(x_tick), 
         labels = x_tick, las = 2)
    legend("top",inset=c(0,-0.11), legend = e_names[c_ind],
           pch = c_ind, cex = 0.9,
           horiz = T)
    if(length(c_ind) > 1)
    {
      for(i in 2 : length(c_ind))
      {
        points(temp[,i], pch=c_ind[i])
      }
    }
  })
  observeEvent(input$mse_dbclick,{
    brush <- input$mse_brush
    if (!is.null(brush)) {
      rangesMSE$x <- c(brush$xmin, brush$xmax)
      rangesMSE$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangesMSE$x <- NULL
      rangesMSE$y <- NULL
    }
  })
  ### Table
  
  output$bias_table <- renderDT({
    r_ind <- getRows()
    c_ind <- getCols()
    temp <- as.data.frame(bias[r_ind, c_ind])
    colnames(temp) <- e_names[c_ind]
    temp_r <- c("N(0,1)",
                "0.9N(0,1) + 0.1N(0,4)",
                "0.8N(0,1) + 0.2N(0,4)",
                "(n-1)N(0,1) + 1N(0,4)",
                "(n-2)N(0,1) + 2N(0,4)",
                "Logistic",
                "Laplace")
    temp_r <- paste(rep(temp_r, each = 4), 
                    "size",
                    c(10, 20, 30,50), sep="_")
    rownames(temp) <- temp_r[r_ind]
    signif(temp,as.numeric(input$signif))
  },options=list(rowCallback = JS(
    'function(row, data) {
    for(i=1; i < data.length;i++){
      data[i] = Math.abs(data[i])
    }
    var num_data = data.slice(1,data.length);
    var row_max = Math.max.apply(Math,num_data);
    var row_min = Math.min.apply(Math,num_data);
    for(i=1;i < data.length; i++) {
      if(data[i] ==row_max) {
        $("td:eq("+i+")", row).css("background-color", "violet")
      } else if(data[i] ==row_min) {
        $("td:eq("+i+")", row).css("background-color", "cyan")
      }
    }
  }')))
  
  output$variances_table <- renderDT({
    r_ind <- getRows()
    c_ind <- getCols()
    temp <- as.data.frame(variances[r_ind, c_ind])
    colnames(temp) <- e_names[c_ind]
    temp_r <- c("N(0,1)",
                "0.9N(0,1) + 0.1N(0,4)",
                "0.8N(0,1) + 0.2N(0,4)",
                "(n-1)N(0,1) + 1N(0,4)",
                "(n-2)N(0,1) + 2N(0,4)",
                "Logistic",
                "Laplace")
    temp_r <- paste(rep(temp_r, each = 4), 
                    "size",
                    c(10, 20, 30,50), sep="_")
    rownames(temp) <- temp_r[r_ind]
    signif(temp,as.numeric(input$signif))
  },options=list(rowCallback = JS(
    'function(row, data) {
    var num_data = data.slice(1,data.length);
    var row_max = Math.max.apply(Math,num_data);
    var row_min = Math.min.apply(Math,num_data);
    for(i=1;i < data.length; i++) {
      if(data[i]==row_max) {
        $("td:eq("+i+")", row).css("background-color", "violet")
      } else if(data[i]==row_min) {
        $("td:eq("+i+")", row).css("background-color", "cyan")
      }
    }
  }')))
  
  output$mse_table <- renderDT({
    r_ind <- getRows()
    c_ind <- getCols()
    temp1 <- as.data.frame(variances[r_ind, c_ind])
    temp2 <- as.data.frame(bias[r_ind, c_ind])
    temp2 <- temp2^2
    temp <- temp1 + temp2
    colnames(temp) <- e_names[c_ind]
    temp_r <- c("N(0,1)",
                "0.9N(0,1) + 0.1N(0,4)",
                "0.8N(0,1) + 0.2N(0,4)",
                "(n-1)N(0,1) + 1N(0,4)",
                "(n-2)N(0,1) + 2N(0,4)",
                "Logistic",
                "Laplace")
    temp_r <- paste(rep(temp_r, each = 4), 
                    "size",
                    c(10, 20, 30,50), sep="_")
    rownames(temp) <- temp_r[r_ind]
    signif(temp,as.numeric(input$signif))
  },options=list(rowCallback = JS(
    'function(row, data) {
    var num_data = data.slice(1,data.length);
    var row_max = Math.max.apply(Math,num_data);
    var row_min = Math.min.apply(Math,num_data);
    for(i=1;i < data.length; i++) {
      if(data[i]==row_max) {
        $("td:eq("+i+")", row).css("background-color", "violet")
      } else if(data[i]==row_min) {
        $("td:eq("+i+")", row).css("background-color", "cyan")
      }
    }
  }')))
  
  
})
