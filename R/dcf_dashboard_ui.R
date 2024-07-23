#'@name dcf_dashboard_ui
#'@export
dcf_dashboard_ui <- function(css_file = NULL){
  if(is.null(css_file)){
    css_file = system.file("www/dcf_dashboard.css", package = "dcf.dashboard")
    if(!dir.exists("www")) dir.create("www")
    file.copy(from = css_file, to = "www")
    css_file = basename(css_file)
  }
  
  cat("CSS file: ", css_file, "\n")
  
  fluidPage(
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    shinyWidgets::useShinydashboard(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = css_file),
      tags$script('var dimension = [0, 0];
                 $(document).on("shiny:connected", function(e) {
                      dimension[0] = window.innerWidth;
                      dimension[1] = window.innerHeight;
                      Shiny.onInputChange("dimension", dimension);
                  });
                  $(window).resize(function(e) {
                      dimension[0] = window.innerWidth;
                      dimension[1] = window.innerHeight;
                      Shiny.onInputChange("dimension", dimension);
                  });')
    ),
    uiOutput("menu"),
  )
}