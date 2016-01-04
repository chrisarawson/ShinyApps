

library(shiny)

shinyUI(fluidPage(
  titlePanel("Simple Linear Regression Explorer"),
  fluidRow(
    column(4,
           wellPanel(
    "Changing the sample size changes the total degrees of freedom and therefore the residual degrees of freedom",
      sliderInput("sampleSize",
                  "Number of Samples (Measurements)",
                  min = 4,
                  max = 100,
                  value = 10),
    "Increasing the variability increases the SS residual much faster than it changes the SS regression.", 
    sliderInput("reScale",
                  "Adjust variability",
                  min=1,
                  max=100,
                  value=5),
    "Changing the effect of X on Y changes the SS regression but NOT the SS residual",
      sliderInput("gradient",
                  "Degree of Effect (effect of X on Y)",
                  min=0,
                  max=20,
                  value=1.5),
    h3("ANOVA Table"),
    verbatimTextOutput('aovSummary')
      )),
    column(5,
           plotOutput("SSRPlot"),
           plotOutput("SSMPlot")),
    column(3,
           h3("Notes"),
withMathJax(),
helpText("The sample points (in orange) are fitted to a straight line $$Y = \\beta_{0} + \\beta_{1}X$$"),
helpText("In this case \\(\\beta_{0}\\) has been set at 100. You can change \\(\\beta_{1}\\) by changing the 'Degree of Effect'"),
helpText("Remember the following important components of the ANOVA table"),
helpText("1. $$MS_{i} = \\frac{SS_{i}}{df_{i}}$$ So if the SS is increased the MS is increased. If 
         the df is increased the MS decreases"),
helpText("2. $$F_{df1,df2} = \\frac{MS_{regression}}{MS_{residual}}$$ So if the MS of the regression increases F increases and
         if the MS of the residual increases F decreases"),
helpText("3. The higher the **F-ratio**, the lower the **p** -value"),
helpText("4. The null hypothesis tested is $$\\beta_{1} = 0$$")
  )
)
))
