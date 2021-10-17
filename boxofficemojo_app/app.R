
library(shiny)
source("../99_initialize_boxoffice_app.R")

directorDt <- getDirectorData(director = "John Carpenter", nameDt = nameDt, keyedBoxOfficeDt = keyedBoxOfficeDt)

whichMovieGotClicked <- function(directorDt, x_coord) {
  getDirectorTotals(directorDt) %>% 
    .[, cum_share := cumsum(domestic_total / sum(domestic_total))] %>%
    .[x_coord <= cum_share] %>% 
    .[1, release]
}

# createActiveMovieList <- function(directorDt, x_coord) {
#   allMovies <- 
# }

toolTipForMovie <- function(directorDt, releaseName) {
  releaseInfo <- getDirectorTotals(directorDt) %>%
    .[release == releaseName]
  paste0(releaseName, "\n",
         "Released ", releaseInfo[, opening_week],"\n",
         "Domestic Box Office Gross: ", 
         paste0("$", round(releaseInfo[, domestic_total] / 1000000, digits = 3), "M"))
}

# TODO:
# 4. Decade bands at bottom of barplot :)
# 5. Add drop-down Option to sort choronologically or in $$$$ order
# - add box office rank to tooltip
# Update the top plot to be prettier - maybe a treeplot?
# - put a disclaimer somewhere about movies before 1982
# 6. pass alpha arguments to the bar plot to grey out unselected movies
# clear and select all buttons on the side panel 


ui <- basicPage(
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("director", "Director Selector:", # need to trim the director selections
                  choices = c("John Carpenter",
                              "Edgar Wright", 
                              "Jon Favreau",
                              "Christopher Nolan",
                              "Bill Condon",
                              "Jennifer Lee",
                              "George Lucas",
                              "Michael Bay",
                              "Mel Gibson",
                              "Steven Spielberg",
                              "Todd Phillips",
                              "George Miller", 
                              "Peter Jackson",
                              "Ryan Coogler",
                              "J.J. Abrams",
                              "Anthony Russo", # interesting 
                              "Joe Russo",
                              "James Wan",
                              "James Gunn",
                              "Guy Ritchie",
                              "Zack Snyder",
                              "Quentin Tarantino", 
                              "Tim Burton", 
                              "Werner Herzog", 
                              "Steve McQueen", "David Cronenberg"),
                  selected = NULL)
    ),
    mainPanel(
      plotOutput("plot1", click = "plot_click", height = 50, hover = hoverOpts(id ="plot_hover")),
      plotOutput("plot2"),
      verbatimTextOutput("hover_info")
    )
  )
)
server <- function(input, output) {
  
  # Reactive table
  values <- reactiveValues()
  values$selected_table <- data.table(x = numeric(), 
                                      release = character())
  
  
  masterData <- reactive({
    getDirectorData(director = input$director,
                    keyedBoxOfficeDt = keyedBoxOfficeDt,
                    nameDt = nameDt)
  })
  
  masterPalette <- reactive({
    names  <- masterData()[order(opening_week), unique(release)]
    values <- scales::hue_pal() (length(names))
    values <- setNames(values, names)
    values
  })

  
  output$plot1 <- renderPlot({
    plotTotalsBar(directorDt = masterData()) + scale_fill_manual(values = masterPalette())
  })
  
  observeEvent(input$plot_click, {
    clickedRelease <- whichMovieGotClicked(directorDt = masterData(), x_coord = input$plot_click$x)
    values$selected_table <- rbind(values$selected_table,
                                   data.table(x = input$plot_click$x,
                                              release = clickedRelease)) %>%
      .[, N := .N, by = release] %>% 
      .[N < 2] %>% .[, N := NULL]
  })
  
  # Active Movie Configuration
  output$plot2 <- renderPlot({
    activeReleases <- values$selected_table$release
    plotDirectorDataWalletShare(directorDt = masterData()[release %in% activeReleases]) + 
      scale_color_manual(values = masterPalette()[which(names(masterPalette()) %in% activeReleases)])
  })
  
  # Tooltip configuration
  output$hover_info <- renderText({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      hoveredRelease <- whichMovieGotClicked(directorDt = masterData(), x_coord = hover$x)
      # print(activeReleases())
      # print(toolTipForMovie(directorDt = masterData(), releaseName = hoveredRelease))
    }
  })
}
shinyApp(ui, server)