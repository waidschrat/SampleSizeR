#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @importFrom golem get_golem_options
#' @noRd
app_ui <- function(request) {

  tagList(
    # adding external resources
    golem_add_external_resources(),
    
    # application UI logic
    
    dashboardPage(
      
      dashboardHeader(
        title = "SampleSizeR"
      ),
      
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem("Interventional Study", icon = icon("flask", lib = "font-awesome"),
                   menuSubItem("Binary Outcome", tabName = "rct_binary", icon = icon("angle-right"), selected = TRUE)
                   #menuSubItem("Metric Outcome", tabName = "rct_metric")
                   #menuSubItem("Time-to-Event Outcome", tabName = "rct_time")
          )
          # menuItem("Observational Study", icon = icon("magnifying-glass, lib = "font-awesome"),
          #          menuSubItem("Binary Outcome", tabName = "nis_binary"),
          #          menuSubItem("Metric Outcome", tabName = "nis_metric"),
          #          menuSubItem("Time-to-Event Outcome", tabName = "nis_time")
          # ),
          # menuItem("Shiny UI Templates", icon = icon("stack-overflow", lib = "font-awesome"),
          #          menuSubItem("BayesROE", tabName = "temp_bayes"),
          #          menuSubItem("VaccCoverage", tabName = "temp_vacc")
          # ),
        ), collapsed = FALSE
      ),
      
      dashboardBody(
        tabItems(
          tabItem(tabName = "rct_binary", mod_RCT_binary_ui("RCT_binary_1"))
          #tabItem(tabName = "rct_metric", mod_RCT_binary_ui("RCT_continuous_1"))
          #tabItem(tabName = "temp_bayes", mod_RCT_binary_ui("bayesROE_1")),
          #tabItem(tabName = "temp_vacc", mod_RCT_binary_ui("VaccCoverage_1"))
        )
      )
      
    )
    
  )
  
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SampleSizeR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
