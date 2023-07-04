#' VaccCoverage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shinyWidgets shinyjs
#' @importFrom shiny NS tagList 
mod_VaccCoverage_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fixedRow(useShinyjs(),
             column(width = 4, #sidebar panel
                    wellPanel(
                      sliderTextInput("deathPortion_S",
                                      "Crude annual mortality rate",
                                      post = " %",
                                      choices = seqb(c(0.25,32), digits = 2),
                                      selected = 0.25),
                      sliderTextInput("vaccPortion_S",
                                      "Crude annual vaccination rate",
                                      post = " %",
                                      choices = c(0.25, seq(0.5,5, by=0.5), seq(1,96, by=1)),
                                      selected=0.25),
                      sliderTextInput("vaccdeathRR_S",
                                      "Rate ratio of mortality under protection",
                                      post = "",
                                      choices = seq(0.1,1, 0.1),
                                      selected=0.1),
                      sliderTextInput("m_dur_S",
                                      "Mean duration until protection loss",
                                      post = " years",
                                      choices = seqb(unique(c(1,50,Inf)))),
                      sliderTextInput("cv_dur_S",
                                      "CV of duration until protection loss",
                                      post = " %",
                                      choices = seqb(unique(c(.2,.5))*100)),
                      sliderTextInput("boost_S",
                                      "Minimal boosting interval",
                                      post = " years",
                                      choices = seqb(c(1,40,Inf)),
                                      selected=1),
                      sliderTextInput("boostPortion_S",
                                      "Crude annual boosting rate",
                                      post = " %",
                                      choices = c(0.25, seq(0.5,5, by=0.5), seq(1,96, by=1)),
                                      selected=30),
                      br(),
                      h5(em("Cohort state at 0 years")),
                      sliderTextInput("V_initial",
                                      "Portion of protected among total",
                                      post = " %",
                                      choices = seq(0,100,2),
                                      selected = 0),
                      sliderTextInput("SR_initial",
                                      "Portion of resusceptible among unprotected",
                                      post = " %",
                                      choices = seq(0,100,2),
                                      selected = 0)
                    )
             ),
             column(width = 8, #main panel
                    fixedRow( #plot output subpanel
                      column(width = 12, 
                             tabsetPanel(
                               tabPanel("Coverage", plotOutput(ns("VaccPlotS0"), click = ns("locator"))),
                               tabPanel("Waning", plotOutput(ns("VaccPlotS1"), click = ns("locator"))),
                               tabPanel("Boosting", plotOutput(ns("VaccPlotS2"), click = ns("locator")))
                             ), br(),br(),br(),br(),br(),br()
                      )
                    ),
                    fixedRow( #plot control subpanel
                      column(width = 5, wellPanel(tableOutput(ns("CursorPos"))), align="center"),
                      column(width = 7, align ="center",
                             wellPanel(
                               downloadButton('downloadData', 'Download Data'),
                               downloadButton('downloadPlot', 'Download Plot') 
                             )
                      )
                    )
             )
    )
  )
}
    
#' VaccCoverage Server Functions
#'
#' @noRd 
mod_VaccCoverage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$VaccPlotS0 <- renderPlot({hist(rnorm(1000,0,1))})
    output$VaccPlotS1 <- renderPlot({plot(airquality)})
    output$VaccPlotS2 <- renderPlot({plot(beaver1)})
    
    
    #ui input monitoring
    observe({
      toggleState(ns("cv_dur_C"), condition = input$m_dur_C != Inf)
      toggleState(ns("boost_C"), condition = input$m_dur_C != Inf)
      toggleState(ns("boostPortion_C"), condition = input$m_dur_C != Inf & input$boost_C != Inf)
      
      toggleState(ns("cv_dur_S"), condition = input$m_dur_S != Inf)
      toggleState(ns("boost_S"), condition = input$m_dur_S != Inf)
      toggleState(ns("SR_initial"), condition = input$m_dur_S != Inf)
      toggleState(ns("boostPortion_S"), condition = input$m_dur_S != Inf & input$boost_S != Inf)
    })
    
    output$CursorPos <- renderTable({
      if(is.null(input$locator)){
        data.frame("Time"=0, "Percent"=0)
      }else{
        data.frame("Time"=input$locator$x, "Percent"=input$locator$y)
      }
    }, digits = 1)
  })
}
