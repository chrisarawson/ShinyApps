
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  output$SSRPlot <- renderPlot({
  
    #SetResiduals
    set.seed(1212)
    yRes<-rnorm(input$sampleSize,0,0.5)
    yResDyn<-yRes*input$reScale
    
    #Create a sequence of x values of required length
    xVal<-seq(1,10,length.out=input$sampleSize)
    
    #Generate the list of fitted y values given a known intercept and B1
    yfit<-c()
    for(i in xVal) {
      yi<-100+(input$gradient*i)
      yfit<-append(yfit,yi)
    }
    
    #For each x get the y that is removed by the equivalent residual
    yVal<-c()
    for (i in xVal) {
      yi<-100+(input$gradient*(i))+yResDyn[which(xVal==i)]
      yVal<-append(yVal,yi)
    }
    
    #run lm and anova
    reg.lm<-lm(yVal~xVal)
    reg.anova<-anova(reg.lm)
    
    #Plot SSresidual
    plot(xVal,reg.lm$fitted,type="l",ylim=c(50,300),xlab="X Value",ylab="Y Value",col="red")
    points(xVal,yVal,pch=19,col="orange")
    for (i in xVal) {
      lines(c(xVal[which(xVal==i)],xVal[which(xVal==i)]),c(yVal[which(xVal==i)],reg.lm$fitted[which(xVal==i)]))
    }
    text(3,60,paste("SS Res =",round(reg.anova$Sum[2],0)))
    text(5,60,paste("df Res = ",(reg.anova$Df[2])))
    text(7,60,paste("MS Res = ",round(reg.anova$Mean[2],0)))
    })
  
  output$SSMPlot <- renderPlot({
    
    #SetResiduals
    set.seed(1212)
    yRes<-rnorm(input$sampleSize,0,0.5)
    yResDyn<-yRes*input$reScale
    
    #Create a sequence of x values of required length
    xVal<-seq(1,10,length.out=input$sampleSize)
    
    #Generate the list of fitted y values given a known intercept and B1
    yfit<-c()
    for(i in xVal) {
      yi<-100+(input$gradient*i)
      yfit<-append(yfit,yi)
    }
    
    #For each x get the y that is removed by the equivalent residual
    yVal<-c()
    for (i in xVal) {
      yi<-100+(input$gradient*(i))+yResDyn[which(xVal==i)]
      yVal<-append(yVal,yi)
    }
    
    #run lm and anova
    reg.lm<-lm(yVal~xVal)
    reg.anova<-anova(reg.lm)

    #Plot SSregression
    plot(xVal,reg.lm$fitted,type="l",ylim=c(50,300),xlab="X Value",ylab="Y Value",col="red")
    abline(h=mean(yVal),lty="dotted",col="blue")
    points(xVal,yVal,col="orange",pch=19)
    for (i in xVal) {
      lines(c(xVal[which(xVal==i)],xVal[which(xVal==i)]),c(mean(yVal),reg.lm$fitted[which(xVal==i)]))
    }
    text(3,60,paste("SS Reg =",round(reg.anova$Sum[1],0)))
    text(5,60,paste("df Reg = ",(reg.anova$Df[1])))
    text(7,60,paste("MS Reg = ",round(reg.anova$Mean[1],0)))
  })
  
    output$aovSummary = renderPrint(function() {
      #SetResiduals
      set.seed(1212)
      yRes<-rnorm(input$sampleSize,0,0.5)
      yResDyn<-yRes*input$reScale
      
      #Create a sequence of x values of required length
      xVal<-seq(1,10,length.out=input$sampleSize)
      
      #Generate the list of fitted y values given a known intercept and B1
      yfit<-c()
      for(i in xVal) {
        yi<-100+(input$gradient*i)
        yfit<-append(yfit,yi)
      }
      
      #For each x get the y that is removed by the equivalent residual
      yVal<-c()
      for (i in xVal) {
        yi<-100+(input$gradient*(i))+yResDyn[which(xVal==i)]
        yVal<-append(yVal,yi)
      }
      
      #run lm and anova
      reg.lm<-lm(yVal~xVal)
      reg.anova<-anova(reg.lm)
      reg.anova
    })
  })

