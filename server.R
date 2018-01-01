
(shiny.trace = TRUE)

library(lubridate)
library(reshape2)
library(ggplot2)
library(corrplot)
library(plotly)
library(scales)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)
library(ggfortify)
library(htmltools)

library(rhandsontable)

library(shiny)
library(shinydashboard)
library(googleVis)
library(DT)
library(readr)
library(corrplot)

library(ggplot2)
library(scales)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)


#data <- read.csv("C:/Users/Karima/Desktop/data_democracy.csv", sep=";")

# Correlation Function 
corTest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      try({
        tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
        uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
      }, silent = TRUE)
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

######Shiny Server
shinyServer(function(input, output) {
  
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    
    selectInput("arg","Argument:",ArgNames())
  })
  
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
    }
  })
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- isolate(as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList))))
   
    return(Dataset)
  })
  
  # Select variables:
  output$varselect <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Variable selection:    
    selectInput("vars", "Variables to use:",
                names(Dataset()), names(Dataset()), multiple =TRUE)            
  })
  output$varselect2 <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Variable selection:    
    selectInput("vars", "Variables to use:",
                names(Dataset()), names(Dataset()), multiple =TRUE)            
  })
  
  output$varselect3 <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    # Variable selection:    
    selectInput("vars", "Variables to use:",
                names(Dataset()), names(Dataset()), multiple =TRUE)            
  })
  
  # Show table:
  output$table <- renderDataTable({
    
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    
    return(#Dataset()[,input$vars,drop=FALSE]
           datatable(cbind(Dataset()[,], Dataset()[,],Dataset()[,],Dataset()[,]), 
                     extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                     options = list(searching = TRUE,autoWidth = TRUE,rownames = FALSE,
                                    scroller = TRUE,scrollX = TRUE,scrollY = "500px",fixedHeader = TRUE,
                                    class = 'cell-border stripe',fixedColumns = list(leftColumns = 3,heightMatch = 'none')
                     )
               )
           
           )
  })
  
  
  ### Download dump:
  
  output$downloadDump <- downloadHandler(
    filename = "Rdata.R",
    content = function(con) {
      
      assign(input$name, Dataset()[,input$vars,drop=FALSE])
      
      dump(input$name, con)
    }
  )
  
  ### Download save:
  
  output$downloadSave <- downloadHandler(
    filename = "Rdata.RData",
    content = function(con) {
      
      assign(input$name, Dataset()[,input$vars,drop=FALSE])
      
      save(list=input$name, file=con)
    }
  )
  
  # Show data:
  output$data <- renderDataTable({
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
     
    # NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    # data <- Dataset()
    # colnames(data)[sapply(data, NA2mean)]
    # 
    
    # datatable(cbind(Dataset()[,], Dataset()[,],Dataset()[,],Dataset()[,]),
    #           extensions = c("FixedColumns", "FixedHeader", "Scroller"),
    #           options = list(searching = TRUE,autoWidth = TRUE,rownames = FALSE,
    #                          scroller = TRUE,scrollX = TRUE,scrollY = "500px",fixedHeader = TRUE,
    #                          class = 'cell-border stripe',fixedColumns = list(leftColumns = 3,heightMatch = 'none')
    #           )
    # )
    # 
    
    datatable(cbind(data,data,data,data),
              extensions = c("FixedColumns", "FixedHeader", "Scroller"),
              options = list(searching = TRUE,autoWidth = TRUE,rownames = FALSE,
                             scroller = TRUE,scrollX = TRUE,scrollY = "500px",fixedHeader = TRUE,
                             class = 'cell-border stripe',fixedColumns = list(leftColumns = 3,heightMatch = 'none')
              )
    )
  })
  
  #####Summary of data 
  output$summary <- renderPrint({
    summary(Dataset()[,input$vars,drop=FALSE])
    
  }) 
  
  #####Structure of data 
  output$structure <- renderPrint({
    str(Dataset()[,input$vars,drop=FALSE])
  })
  
  #####Missing values
  output$valueBox <- renderValueBox({
    a<-table(is.na(Dataset()[,,drop=FALSE]))
    a[2]
    valueBox(value = a[2], subtitle = 'missing values', color ='teal',icon = icon("calculator"))
  })
  
  output$mis_values <- renderDataTable({
    Missing=colSums(is.na(Dataset()[,])|Dataset()[,] == 'NA')
    a=as.data.frame(Missing)
  })
  
  
  ###########Variables to use in graphs
  output$dvX = renderUI({
    selectInput('dvX', h5('choose X'), choices = names(Dataset()))
  })
  
  output$dvY = renderUI({
    selectInput('dvY', h5('Choose Y'), choices = names(Dataset()))
  })
  
  output$bivarX = renderUI({
    selectInput('bivarX', h5('choose X'), choices = names(Dataset()))
  })
  
  output$bivarY = renderUI({
    selectInput('bivarY', h5('Choose Y'), choices = names(Dataset()))
  })
  
  
  ##### title (message) for regression
  output$msg1 <- renderPrint({
    print("Regression Summary")
    
  })
  output$msg2 <- renderPrint({
    print("Regression Summary")
    
  })
  output$msg3 <- renderPrint({
    print("Regression Summary")
    
  })
  
  ####title (message) for CI
  output$msg11 <- renderPrint({
    print("Confidence interval of estimators")
    
  })
  output$msg22 <- renderPrint({
    print("Confidence interval of estimators")
    
  })
  output$msg33 <- renderPrint({
    print("Confidence interval of estimators")
    
  })
  
  
  ##### PLOTS  
  
  output$plot <- renderPlotly({
    # NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    # df <- Dataset()
    # colnames(df)[sapply(df, NA2mean)]
    # 
    # Dataset()<-df
    
    if 
    (input$dataset=="Dot Plot") {
      X=Dataset()[,input$dvX]
      Y=Dataset()[,input$dvY]
      
      
      a <- list(
        title = input$dvX,
        
        showticklabels = TRUE
        
      )
      b <- list(
        title = input$dvY,
        
        showticklabels = TRUE
        
      )
      p <- plot_ly(data = Dataset(), x = ~X, y = ~Y) %>%
        
        layout(xaxis = a, yaxis = b, showlegend = FALSE)
      
      p
      
    }

    else if (input$dataset=="Histogram") {
    
      X=Dataset()[,input$dvX]
      Y=Dataset()[,input$dvY]
      
      
      a <- list(
        title = input$dvX,
        
        showticklabels = TRUE
        
      )
      b <- list(
        title = input$dvY,
        
        showticklabels = TRUE
        
      )
      Y=Dataset()[,input$dvY]
      fit<-density(Dataset()[,input$dvY])
      p<-plot_ly(Dataset(),x=~Dataset()[,input$dvY]
                 ,type="histogram") %>%
        add_trace(x=fit$x,y=fit$y,type="scatter",mode="lines",fill="tozeroy",yaxis="y2",name="Density")%>%
        layout(yaxis2=list(overlaying="y",side="right"))%>%
        layout(xaxis = b, showlegend =T) 
      p
      
    }
    
    
    else if (input$dataset=="Boxplot") {
      
      Y=Dataset()[,input$dvY]
      
      
      my_data <- data.frame(Dataset())
      
      a <- list(
        title = input$dvX,
        
        showticklabels = TRUE
        
      )
      b <- list(
        title = input$dvY,
        
        showticklabels = TRUE
        
      )
      p=plot_ly(y=~Y, color = ~cut, type = "box",boxpoints = "all", jitter = 0.3,
                pointpos = -1.8) %>%
        layout(xaxis = b, showlegend = FALSE)
      p
      
    }
    else if (input$dataset=="Pie Chart") {
      data<- data.frame(Dataset())
      Y=Dataset()[,input$dvY]
      a <- list(
        title = input$dvX,
        
        showticklabels = TRUE
        
      )
      b <- list(
        title = input$dvY,
        
        showticklabels = TRUE
        
      )
      p <- data %>%
        group_by(Dataset()[,input$dvY]) %>%
        dplyr::summarise(count = n()) %>%
        plot_ly(labels = ~ unique(Dataset()[,input$dvY]), values = ~count) %>%
        add_pie(hole = 0.6) %>%
        layout(xaxis = b, showlegend = TRUE)
      p
      
    }
    
  })
  
  ####Correlation output
  numericColumns <- reactive({
    
    df <- Dataset()
    colnames(df)[sapply(df, is.numeric)|sapply(df,is.integer)]
  })
  
   mean <- reactive({
    NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
    df <- Dataset()
    colnames(df)[sapply(df, NA2mean)]
   })
  
  correlation <- reactive({
    data <- Dataset()
    #df <- Dataset()
    #colnames(data)[sapply(data, is.numeric)|sapply(data,is.integer)]
    data <- Dataset()
    variables <- input$vars
    if(is.null(data) || !length(intersect(variables, colnames(data)))) {
      NULL
    } else {
      
      # data<- Dataset()
      # d=colnames(data)[sapply(data, is.numeric)|sapply(data,is.integer)]
      # 
      my_data <- data.frame(Dataset())
      d <- my_data [,sapply(my_data ,is.integer)|sapply(my_data ,is.numeric)] 
      d1 <- as.data.frame(round(cor(d), 2))
      
      #Dataset()<-data 
      #cor(Dataset()[,input$vars], use = input$corUse, method = input$corMethod)
      #cor(d1, use = input$corUse, method =c("pearson", "kendall", "spearman"))#temchi
      cor(d1, use = input$corUse, method = input$corMethod)
    }
  })
  
  sigConfMat <- reactive({
    val <- correlation()
    if(!is.null(val))
      corTest(val, input$confLevel)
  })
  
  output$corr <- renderGvis({
    my_data <- data.frame(Dataset())
    d <- my_data [,sapply(my_data ,is.integer)|sapply(my_data ,is.numeric)] 
    cor <- as.data.frame(round(cor(d), 2))
    cor <- cbind(Variables = rownames(cor), cor)
    gvisTable(cor) 
    
    
  })
  
  output$plot2 <- renderPlotly({
    my_data <- data.frame(Dataset())
    p<-corrplot(cor(Dataset()), type="upper", tl.cex = 0.1)
    p
  })
  
  ##  missing tickmarks at top and right of graph
  output$corrPlot <- renderPlot({
    val <- correlation()
    if(is.null(val)) return(NULL)
    
    val[is.na(val)] <- 0
    args <- list(val,tl.cex = 0.1,
                 order = if(input$plotOrder == "manual") "original" else input$plotOrder, 
                 hclust.method = input$plotHclustMethod, 
                 addrect = input$plotHclustAddrect,
                 
                 p.mat = sigConfMat()[[1]],
                 sig.level = if(input$sigTest) input$sigLevel else NULL,
                 insig = if(input$sigTest) input$sigAction else NULL,
                 
                 lowCI.mat = sigConfMat()[[2]],
                 uppCI.mat = sigConfMat()[[3]],
                 plotCI = if(input$showConf) input$confPlot else "n")
                 
    
    if(input$showConf) {
      do.call(corrplot, c(list(type = input$plotType), args))
    } else if(input$plotMethod == "mixed") {
      do.call(corrplot.mixed, c(list(lower = input$plotLower,
                                     upper = input$plotUpper),
                                args))
    } else {
      do.call(corrplot, c(list(method = input$plotMethod, type = input$plotType), args))
    }
  })  
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Select graph one by one !" else d
  })
  
  ##### regression formula
  
  # bivariate model
  
  output$modelbivar <-  renderPrint({
    
    X=Dataset()[,input$bivarX]
    Y=Dataset()[,input$bivarY]
    
    P<-lm(Y~X,data=Dataset())
    summary(P)
  })
  
  modelbivar <- reactive({
    X=Dataset()[,input$bivarX]
    Y=Dataset()[,input$bivarY]
    lm(Y~X,data=Dataset())
  })
  
  output$conft3 <-  renderPrint({
    s<-confint(modelbivar())
    s
  })
  
  
  #polynomial model
  
  output$model <-  renderPrint({
    
    P<-lm(input$X,data=Dataset())
    summary(P)
    
  })
  
  output$conft <-  renderPrint({
    s<-confint(model())
    s
  })
  
  
  model <- reactive({
    lm(input$X,data=Dataset())
  })
  
  ###### residuals:
  ##bivariate
  output$residuals_hist <- renderPlot({
    if (is.null(modelbivar()$residuals)) {}
    else 
    { hist(modelbivar()$residuals, main = paste("Residuals Distribution"), xlab = 'Residuals') }
  })
  output$error1<-renderPlot({
    layout(matrix(1:4,2,2))
    plot(modelbivar())
  })
  
  
  ####polynomial model 
  output$residuals_hist3 <- renderPlot({
    if (is.null(model()$residuals)) {}
    else 
    { hist(model()$residuals, main = paste("Residuals Distribution"), xlab = 'Residuals') }
  })
  output$error3<-renderPlot({
    layout(matrix(1:4,2,2))
    plot(model())
  })   
  
  
  })
  
  
  
  

  




