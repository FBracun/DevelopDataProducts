library(shiny)
library(ggplot2)
library(plyr)
library(caret)

input <- list(rseed=1)

data.maker <- function(nobs=20, 
                       expr="y=2+3*x+u", 
                       seed=1, 
                       sdx=1, 
                       sdu=3 ) {
  set.seed(seed)
  x <- rnorm(500)*sdx+2
  u <- rnorm(500)*sdu
  
  x <- x[1:nobs]
  u <- u[1:nobs]
  
  x2 <- x^2
  x3 <- x^3
  x4 <- x^4
  x5 <- x^5
  expx <- exp(x)
  y <- eval(parse(text=expr))
  
  data.frame(x=x,x2=x2,x3=x3,x4=x4,x5=x5
             ,expx=expx,u=u,y=y)
}

shinyServer(function(input, output) {

  
  mydata <- reactive({
    data.maker(nobs=input$nobs, 
       expr=input$dgp, 
       seed=input$rseed,
       sdx=input$sdx,
       sdu=input$sdu)
  })

  
  output$datacaption <- renderText(paste0("Data Generating Proccess: ",input$dgp))
  output$datacaptionTest <- renderText(paste0("Data Generating Proccess: ",input$dgp))

 
  trainData <- reactive({
            
            set.seed(input$rseed) ### Dodal
            trainIndex <- createDataPartition(y=mydata()$y   , p=input$train/100   , list = FALSE)
            data.maker(nobs=input$nobs, 
                       expr=input$dgp, 
                       seed=input$rseed,
                       sdx=input$sdx,
                       sdu=input$sdu)[trainIndex,]
          })

  testData <- reactive({
            set.seed(input$rseed) ### Dodal
            trainIndex <- createDataPartition(y=mydata()$y   , p=input$train/100   , list = FALSE)      
            data.maker(nobs=input$nobs, 
                       expr=input$dgp, 
                       seed=input$rseed,
                       sdx=input$sdx,
                       sdu=input$sdu)[-trainIndex,]
          })
  
  
  
  output$train <- renderDataTable(trainData(), options = list(iDisplayLength = 10)) 
  output$test <- renderDataTable(testData(), options = list(iDisplayLength = 10)) 
  
  lmResults <- reactive({
    regress.exp <- input$regression
    if (!input$constant) regress.exp <- paste(input$regression, "- 1")
    
   
    set.seed(input$rseed) 
    trainIndex <- createDataPartition(y=mydata()$y   , p=input$train/100   , list = FALSE)
    trainData <- mydata()[trainIndex,]
    testData <- mydata()[-trainIndex,]
          
    lm(regress.exp, data=trainData)  
  })
  
  output$lmStats <- renderTable({
    results <- summary(lmResults())
    data.frame(R2=results$r.squared,
               adj.R2=results$adj.r.squared,
               DOF.model=results$df[1],
               DOF.available=results$df[2],
               DOF.total=sum(results$df[1:2]),
               f.value=results$fstatistic[1],
               f.denom=results$fstatistic[2],
               f.numer=results$fstatistic[3],
               p=1-pf(results$fstatistic[1],
                         results$fstatistic[2],
                         results$fstatistic[3]))
  })
  
  # Show coefficients
  
  output$lmResults <- renderTable(summary(lmResults()))
  
  # Show plot of points, regression line, residuals
  output$scatter <- renderPlot({
    
    set.seed(input$seed)     
    trainIndex <- createDataPartition(y=mydata()$y   , p=input$train/100   , list = FALSE) 
    data1 <- mydata()[-trainIndex,] 
    x <- data1$x
    y <- data1$y
    
    xcon <- seq(min(x)-.1, max(x)+.1, .025)
    x2 <- xcon^2
    x3 <- xcon^3
    x4 <- xcon^4
    x5 <- xcon^5
    expx <- exp(xcon)
    
    predictor <- data.frame(x=xcon,x2=x2,x3=x3,x4=x4,x5=x5,expx=expx) #Dodaj [-trainIndex,]?????
     
    regression <- paste0("Regression Model: ", input$regression)
    
    if(input$regression == "y~x") { 
      
      regression <- expression(paste("Regression Model: ", y, " ~ ", x))
    
    }
    
    if(input$regression == "y~x2") { 
      
      regression <- expression(paste("Regression Model: ", y, " ~ ", x^{2}))
      
    }
    
    if(input$regression == "y~expx") { 
      
      regression <- expression(paste("Regression Model: ", y, " ~ ", e^{x}))
      
    }
    
    if(input$regression == "y~x+x2") { 
      
      regression <- expression(paste("Regression Model: ", y, " ~ ", x+x^{2}))
      
    }
    
    if(input$regression == "y~x+expx") { 
      
      regression <- expression(paste("Regression Model: ", y, " ~ ", x+e^{x}))
      
    }
  
    if(input$regression == "y~x+x2+expx") { 
      
      regression <- expression(paste("Regression Model: ", y, " ~ ", x+x^{2}+e^{x}))
      
    }
  
  
    if(input$regression == "y~x+x2+x3+x4+x5") { 
      
      regression <- expression(paste("Regression Model: ", y, " ~ ", x+x^{2}+x^{3}+x^{4}+x^{5}))      
    }
        
    yline <- predict(lmResults(), predictor)
  
  
  par(bg="cornsilk2", cex.main=1.5, cex.lab=1.3)
  
  plot(c(min(x),max(x)) 
       ,c(min(y,yline),max(y,yline)), 
       type="n",
       xlab="x",
       ylab="y",
       main=regression)
  
  if (input$resid) for (j in 1:length(x)) {
    
    yhat <- predict(lmResults(), data1[j,]) #Dodal
    lines(rep(x[j],2), c(yhat,y[j]), col="red") #Namesto yhat sem dal yline
    
  } 
 
  if (input$showdata) points(x,y)
  
  if (input$predict) lines(xcon, yline, lwd=2, col="blue") #Namesto lines funkcije sem dal funcijo points
  })
  
    
    
  output$residuals <- renderPlot({

    dTest <- testData()
    dTest$y_pred <- predict(lmResults(), newdata=dTest)
    
    
    ggplot(data=dTest,aes(x=y_pred, y=y_pred-y)) +
      geom_point(alpha=0.2,color="black") +
      geom_smooth(aes(x=y_pred,  y = y_pred - y  ),  color="black")

   
  })
  
  
  rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
  rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }


  
  output$R_sq_test <- renderText(paste0("R-squared on test data: ",
                                        round(rsq(testData()$y,predict(lmResults(),
                                                                 newdata=testData())),2)))
  
  
  output$RMSE_test <- renderText(paste0("RMSE on test data: ",
                                        round(rmse(testData()$y,predict(lmResults(),
                                                                 newdata=testData())),2)))
  
  
  output$R_sq_train <- renderText(paste0("R-squared on train data: ",
                                        round(rsq(trainData()$y,predict(lmResults(),
                                                                 newdata=trainData())),2)))
  
  
  output$RMSE_train <- renderText(paste0("RMSE on train data: ",
                                        round(rmse(trainData()$y,predict(lmResults(),
                                                                  newdata=trainData())),)))
    
  
  ##############
  
  #output$Document <- renderText(paste0("A Documentation for Using Shiny Teaching Tool for Regression Analysis"))
  
  output$dataGenerationexpr <- renderUI({
    
    
    withMathJax(
      
      
      helpText('Data Generation'),
      helpText('-----------------------------------------------------------'),
      sprintf('Expression y=4+2*x+u represents equation \\(y=4+2*x+ u\\)'),
      br(),
      sprintf('Expression y=4+2*x-.5*x2+u represents equation  \\(y=4+2*x-.5*x^2+u\\)'),
      br(),
      sprintf('Expression y=y=-10-1.5*x+x2+u represents equation \\(y=-10-1.5*x+x^2+u\\)'),
      br(),
      sprintf('Expression y=-3*x-.6*x2+u represents equation  \\(y=-3*x-.6*x^2+u\\)'),
      br(),
      sprintf('Expression y=5+2*x-.1*expx+u represents equation \\(y=5+2*x-.1*e^x+u\\)'),
      br(),
      sprintf('Expression y=.5*expx+u*10 represents equation  \\(y=.5*e^x+u*10\\)'),
      br(),
      sprintf('Expression y=.5*expx+u*10 represents equation  \\(y=.5*e^x+u*10\\)'),      
      br(),
      sprintf('Expression y=4-2*x2+.4*expx-x*2+u*5 represents equation \\(y=4-2*x^2+.4*e^x-x*2+u*5\\)'),
      br(),
      br(),
      br(),
      
      helpText('Regression Model'),
      helpText('-----------------------------------------------------------'),
      
      sprintf('Expression y~x represents equation \\(y \\sim x\\)'),
      br(),
      sprintf("Expression y~x2 represents equation \\(y \\sim x^2\\) "),
      br(),
      sprintf("Expression y~expx represents equation \\(y \\sim e^x\\) "),
      br(),
      sprintf("Expression y~x+x2 represents equation \\(y \\sim x+x^2\\) "),
      br(),
      
      
      sprintf("Expression y~x+expx represents equation \\(y \\sim x+e^x\\) "),
      br(),
      sprintf("Expression y~x+x2+expxrepresents equation \\(y \\sim x+x^2+e^x\\) "),
      br(),
      sprintf("Expression y~x+x2+x3+x4+x5 represents equation \\(y \\sim x+x^2+x^3+x^4+x^5\\) ")  
      
      
    )
    
    
  })
  
 
  
  
  
})