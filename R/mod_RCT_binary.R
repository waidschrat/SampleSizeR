#' RCT_binary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shinyjs
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets sliderTextInput
mod_RCT_binary_ui <- function(id){
  
  ns <- NS(id)
  inits <- get_golem_options("inits")
  
  tagList(
    fixedRow(useShinyjs(),
             column(width = 4, #sidebar panel
                    wellPanel(
                      em("Design Parameters"),
                      br(),
                      numericInput(inputId = ns("Risk_T"),
                                   label = "Event Risk | Treatment Arm", 
                                   value = inits$Risk_T, min = 0, max = 1, step = 0.05),
                      numericInput(inputId = ns("Risk_C"),
                                   label = "Event Risk | Comparator Arm", 
                                   value = inits$Risk_C, min = 0, max = 1, step = 0.05),
                      sliderTextInput(inputId = ns("Sample"),
                                      label = "Total Sample Size",
                                      choices = seqb(c(16,2048), digits = 0),
                                      selected = inits$Sample),
                      sliderTextInput(inputId = ns("Prop_T"),
                                      label = "Sample Proportion | Treatment Arm",
                                      choices = seq(0.1,0.9,by=0.1),
                                      selected = inits$Prop_T),
                      numericInput(inputId = ns("Alpha"),
                                   label = "Type 1 Error Probability", 
                                   value = inits$Alpha, min = 0, max = 1, step = 0.01),
                      numericInput(inputId = ns("Delta"),
                                   label = "Relevance Margin (Odds Ratio)", 
                                   value = inits$Delta, min = 0, max = 25, step = 0.01),
                      br(),
                      em("Implied Effect Size"),
                      tableOutput(outputId = ns("EffectTab"))
                    )
             ),
             column(width = 8, #main panel
                    fixedRow( #plot output subpanel
                      column(width = 12,
                             
                             tabsetPanel(
                               tabPanel("Sampling Distribution", plotOutput(ns("DistPlot"), click = ns("locator"))),
                               tabPanel("Statistical Power", plotOutput(outputId = ns("PowPlot"), click = ns("locator"))),
                               #tabPanel("Assurance", plotOutput(ns("VaccPlotS1"), click = ns("locator"))),
                             ), 
                             br(),br(),br(),br(),br(),br()
                      )
                    ),
                    fixedRow( #plot control subpanel
                      column(width = 5, align="center", 
                             wellPanel(
                               em("Cursor Position"),
                               tableOutput(outputId = ns("CursorPos"))
                               )),
                      column(width = 7, align ="center",
                             # wellPanel(
                             #   downloadButton('downloadData', 'Download Data'),
                             #   downloadButton('downloadPlot', 'Download Plot')
                             # )
                      )
                    ),
                    
                    fixedRow( #plot inputs subpanel (used for debugging)
                      column(width = 12, align="center", 
                             wellPanel(
                               tableOutput(outputId = ns("InputTab"))
                             ))
                    )
             )
    )
  )
}
    
#' RCT_binary Server Functions
#'
#' @noRd 
#' @import stats
#' @import graphics
#' @importFrom grDevices rgb
mod_RCT_binary_server <- function(id){
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    inits <- get_golem_options("inits")
    #input <- inits
    
    # Reactives
    PowNum <- reactive({
      N1 <- as.numeric(input$Sample)*input$Prop_T
      N1 <- seqb(N1, by = 0.01)
      N2 <- as.numeric(input$Sample)*(1-input$Prop_T)
      N2 <- seqb(N2, by = 0.01)

      lOR <- OddsRatio(R1 = input$Risk_T, R2 = input$Risk_C,
                       N1 = round(N1), N2 = round(N2)
                       ) #calculate log OR
      lOR_se <- OddsRatioSE(R1 = input$Risk_T, R2 = input$Risk_C,
                            N1 = round(N1), N2 = round(N2)
                            ) #calculate standard error of log OR
      (1-pnorm(qnorm(1-input$Alpha,0,lOR_se), lOR, lOR_se))*100
    })
    
    # Plots
    output$DistPlot <- renderPlot({
      lOR <- OddsRatio(R1 = input$Risk_T, R2 = input$Risk_C,
                       N1 = 1, N2 = 1
      ) #calculate log OR
      lOR_se <- OddsRatioSE(R1 = input$Risk_T, R2 = input$Risk_C,
                            N1 = input$Sample[2]*input$Prop_T, N2 = input$Sample[2]*(1-input$Prop_T)
      ) #calculate standard error of log OR
      
      x <- seq( log(input$Delta/3), log(input$Delta*3)+lOR, length=250 )
      plot(x, dnorm(x, log(input$Delta), lOR_se), type = "l", lty = 2, axes = F,
           ylab = "probability density", xlab = "estimated odds ratio (OR)",
           main = paste("total sample size =", input$Sample[2]) )
      lines(x, dnorm(x, lOR, lOR_se))
      x <- seq( log(input$Delta/3), log(input$Delta*3)+lOR, length=11 )
      axis(1, at = x, labels = round(exp(x),1) )
      axis(2)
      
      abline(v=log(input$Delta), lty = 3, col = "red")
      x <- seq( qnorm(1-input$Alpha, log(input$Delta), lOR_se), log(input$Delta*3)+lOR, length=100 )
      polygon(c(x,rev(x)),
              c(dnorm(x, log(input$Delta), lOR_se), rep(0, length(x))),
              col=rgb(1,0,0,0.2), border = NA)
      x <- seq( qnorm(1-input$Alpha, log(input$Delta), lOR_se), log(input$Delta*3)+lOR, length=100 )
      polygon(c(x,rev(x)),
              c(dnorm(x, lOR, lOR_se), rep(0, length(x))),
              col=rgb(0,0,1,0.2), border = NA)
      
      pow <- 1-pnorm(qnorm(1-input$Alpha, log(input$Delta), lOR_se), lOR, lOR_se)
      text(lOR, dnorm(lOR, lOR, lOR_se)*0.5, 
           labels = paste(round(pow*100,1), "%") )

      grid()
      legend("topleft", lty = 1:2, legend = c("OR = True Effect", "OR = Relevance Margin"), bty = "n")
    })
    
    output$PowPlot <- renderPlot({
      pow <- round(PowNum(), 1)
      plot(seqb(input$Sample, by = 0.01), pow, type = "l",
           ylim = c(0, 100),
           ylab = "conditional probability of success (%)", xlab = "total sample size")
      grid()
    })
    
    
    
    # Tables
    output$CursorPos <- renderTable({
      if(is.null(input$locator)){
        data.frame("x value"=0, "y value"=0)
      }else{
        data.frame("x value"=input$locator$x, "y value"=input$locator$y)
      }
    }, digits = 1)
    
    
    output$EffectTab <- renderTable({
      lOR <- OddsRatio(R1 = input$Risk_T, R2 = input$Risk_C, N1 = 1, N2 = 1)
      lRR <- log(input$Risk_T) - log(input$Risk_C)
      
      data.frame("Odds Ratio" = exp(lOR), "Risk Ratio" = exp(lRR))
    }, digits = 2)
    
    
    output$InputTab <- renderTable({
      input_copy <- reactiveValuesToList(input)
      input_copy <- input_copy[c("Risk_T","Risk_C","Sample","Alpha","Prop_T")]
      
      data.frame("InputId"=names(input_copy), 
                 "First Value"=sapply(input_copy, function(x) x[1]),
                 "Length"=sapply(input_copy, length),
                 "Type"=sapply(input_copy, typeof)
      )
    }, digits = 1)
 
  })
}
