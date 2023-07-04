#' bayesROE UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom colourpicker colourInput
mod_bayesROE_ui <- function(id){
  # set default options
  ns <- NS(id)
  inits <- get_golem_options("inits")
  ref_cols <- get_golem_options("ref_cols")
  
  # fundamental sidebar elements
  sidebar_args <- list(
    fluidRow(
      column(radioButtons(inputId = "type", label = "Plot Type", 
                          choices = list("Threshold"="thres","Probability"="prob")),
             width = 6),
      column(sliderInput(inputId = "nregion", label = "Regions", 
                         min = 1, max = 5, value = length(inits$delta), step = 1, ticks = FALSE),
             width = 6)
    ),
    numericInput(inputId = "ee", label = "Effect Estimate", value = inits$ee,
                 min = -10, max = 10, step = 0.01),
    numericInput(inputId = "se", label = "Standard Error", value = inits$se,
                 min = 0.1, max = 10, step = 0.01),
    br()
  )
  
  # additional sidebar elements | Plot Type
  sidebar_args[[length(sidebar_args)+1]] <- uiOutput("add_sidebar")
  sidebar_args[["width"]] <- 3
  
  
  tagList(
    sidebarLayout(
      do.call(sidebarPanel, args = sidebar_args),
      mainPanel(width = 9,
                plotOutput(outputId = "ROEplot"),
                fluidRow(
                  column(
                    wellPanel(
                      fluidRow(
                        column(
                          uiOutput("plot_limits"),
                          checkboxInput(inputId = "addRef", label = "Show Sceptical Prior", value = FALSE),
                          checkboxInput(inputId = "addEst", label = "Show Effect Estimate", value = FALSE),
                          width = 6),
                        column(
                          sliderInput(inputId = "col_alpha", label = "Colour Opacity", min = 0, max = 1, value = 1, step = 0.1, ticks = FALSE),
                          colourInput(inputId = "col_lower", label = "Lower Colour Key", value = ref_cols$col_lower),
                          colourInput(inputId = "col_upper", label = "Upper Colour Key", value = ref_cols$col_upper),
                          checkboxInput(inputId = "flip", label = "Flip Axes", value = FALSE),
                          width = 6)
                      ),
                    ), width = 6),
                  column(
                    wellPanel(
                      fluidRow(
                        column(downloadButton(outputId = "fig_download",
                                              label = "Download Figure"),
                               br(),br(),
                               selectInput(inputId = "fig_format", label = "Data Format",
                                           choices = list("pdf", "eps", "svg", "tex", "png", "tiff"),
                                           selected = "pdf"),
                               numericInput(inputId = "fig_width", label = "Figure Width (mm)",
                                            min = 150, max = 400, step = 10, value = 200),
                               width=6),
                        
                        column(radioButtons(inputId = "fig_aspect", label = "Aspect Ratio",
                                            choices = list("4:3", "16:9", "16:10"), selected = "4:3"),
                               width=6)
                      )
                    ), width = 6)
                )
      )
    )
  )
}
    
#' bayesROE Server Functions
#'
#' @noRd
mod_bayesROE_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    # set default options
    ns <- session$ns
    inits <- get_golem_options("inits")
    ref_cols <- get_golem_options("ref_cols")
    
    # module server logic
    output$add_sidebar <- renderUI({
      if(input$type == "thres"){
        sidebar_args <- tagList(
          numericInput(inputId = "alpha", label = "Alpha (%)",
                       value = inits$alpha[1]*100,
                       min = 0.1, max = 99.9, step = 0.1),
          bsTooltip(id = "alpha", trigger = "focus", 
                    title = "Posterior probability that effect size is less extreme than threshold(s)",
                    placement = "right", options = list(container = "body"))
        )
        
        for(i in 1:input$nregion){
          if(is.na(inits$delta[i])) inits$delta[i] <<- inits$delta[length(inits$delta)] + 1
          sidebar_args[[length(sidebar_args)+1]] <- numericInput(inputId = paste0("delta",i),
                                                                 label = HTML(paste0("Delta", tags$sub(i))),
                                                                 value = inits$delta[i],
                                                                 min = -100, max = 100, step = 0.01)
        }
      }
      if(input$type == "prob"){
        sidebar_args <- tagList(
          numericInput(inputId = "delta",
                       label = "Delta",
                       value = inits$delta[1],
                       min = -100, max = 100,
                       step = 0.01),
          bsTooltip(id = "delta", trigger = "focus", 
                    title = "Threshold representing the smallest relevant effect size.",
                    placement = "right", options = list(container = "body"))
        )
        
        for(i in 1:input$nregion){
          if(is.na(inits$alpha[i])) inits$alpha[i] <<- inits$alpha[length(inits$alpha)] / 2
          sidebar_args[[length(sidebar_args)+1]] <- numericInput(inputId = paste0("alpha",i),
                                                                 label = HTML(paste0("Alpha", tags$sub(i) ," (%)")),
                                                                 value = inits$alpha[i]*100,
                                                                 min = 0.1, max = 99.9, step = 0.1)
        }
      }
      
      sidebar_args
    })
    
    output$plot_limits <- renderUI({
      tagList(
        sliderInput(inputId = "meanLim", label = "Limits x-Axis",
                    min = -3*abs(input$ee), max = 3*abs(input$ee), round = -1, ticks = FALSE,
                    value = c(pmin(2*input$ee, -0.5*input$ee), pmax(-0.5*input$ee, 2*input$ee)) ),
        sliderInput(inputId = "sdLim", label = "Limits y-Axis", 
                    min = 0, max = 5*input$se, round = -1, ticks = FALSE,
                    value = c(0, 3*input$se) )
      )
    })
    
    fig_height <- reactive({
      if(input$fig_aspect == "4:3"){
        return(round((input$fig_width/4) * 3))
      }else if(input$fig_aspect == "16:9"){
        return(round((input$fig_width/16) * 9))
      }else if(input$fig_aspect == "16:10"){
        return(round((input$fig_width/16) * 10))
      }
    })
    
    delta <- reactive({
      if(input$type == "thres"){
        expr <- paste0("c(",paste(paste0("input$delta",1:input$nregion), collapse = ", "),")")
      }else if(input$type == "prob"){
        expr <- paste0("input$delta")
      }
      eval(parse(text = expr))
    })
    
    alpha <- reactive({
      if(input$type == "thres"){
        expr <- paste0("input$alpha / 100")
      }else if(input$type == "prob"){
        expr <- paste0("c(",paste(paste0("input$alpha",1:input$nregion), collapse = ", "),") / 100")
      }
      eval(parse(text = expr))
    })
    
    ROEfig <- reactive({
      deltas <- delta()
      alphas <- alpha()
      if(length(alphas) >= 1 & length(deltas) >= 1){
        ROE <- ribbonROE(ee = input$ee, se = input$se, 
                         delta = deltas, alpha = alphas,
                         type = input$type, larger = TRUE,
                         meanLim = input$meanLim, sdLim = input$sdLim,
                         nGrid = 500, relative = TRUE, 
                         addRef = input$addRef, addEst = input$addEst,
                         cols = c(input$col_lower, input$col_upper), cols_alpha = input$col_alpha)
        
        if(!input$flip) ROE$plot <- suppressMessages(ROE$plot + coord_flip(ylim = input$meanLim, xlim = input$sdLim))
        
        return(ROE$plot)
      }else{
        return(NULL)
      }
    })
    
    output$ROEplot <- renderPlot({
      ROEfig()
    }, width = 640)
    
    output$fig_download <- downloadHandler(
      filename = function() {paste0('BayesROE_',input$fig_width,'mm.',input$fig_format)},
      content = function(file) {
        ggsave(file, device = input$fig_format, units = "mm", dpi = 300,
               width=input$fig_width, height=fig_height())
      }
    )
 
  })
}
