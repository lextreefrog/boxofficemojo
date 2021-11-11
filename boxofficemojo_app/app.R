
library(shiny)
source("../99_initialize_boxoffice_app.R")

directorDt <- getDirectorData(director = "John Carpenter", nameDt = nameDt, keyedBoxOfficeDt = keyedBoxOfficeDt)
totalsDt   <- getDirectorTotals(directorDt = directorDt)
whichMovieGotClicked <- function(totalsDt, x_coord) {
  totalsDt %>% 
    .[1 - abs(x_coord) <= cum_share] %>% 
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
# bar plot that matches the labeling 


ui <- basicPage(
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("director", "Director Selector:", # need to trim the director selections
                  choices = c("John Carpenter",
                              "Edgar Wright", 
                              "Joel Zwick",
                              "Robert Zemeckis",
                              "Jim Jarmusch",
                              "Jason Reitman",
                              "Kathryn Bigelow",
                              "Walter Murch",
                              "James Cameron",
                              "John Landis",
                              "Christopher Nolan",
                              "M. Night Shyamalan",
                              "Eli Roth",
                              "Alejandro G. Iñárritu",
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
                              "Rian Johnson",
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
      verbatimTextOutput("hover_info"),
      plotOutput("plot1", click = "plot_click", height = 50, hover = hoverOpts(id ="plot_hover")),
      plotOutput("plot2"),
      plotOutput("plot3")
    )
  )
)
server <- function(input, output) {
  
  # Reactive table
  values <- reactiveValues()
  values$selected_table <- data.table(x = numeric(), 
                                      release = character())
  
  
  masterData <- reactive({
    directorDt <- getDirectorData(director = input$director,
                                  keyedBoxOfficeDt = keyedBoxOfficeDt,
                                  nameDt = nameDt)
    list(
      directorDt = directorDt,
      totalsDt   = getDirectorTotals(directorDt = directorDt)
    )
  })
  
  masterPalette <- reactive({
    names  <- masterData()[["totalsDt"]][, unique(release)]
    values <- scales::hue_pal() (length(names))
    values <- setNames(values, names)
    values
  })

  
  output$plot1 <- renderPlot({
    activeReleases <- values$selected_table$release
    plotTotalsBar(totalsDt = masterData()[["totalsDt"]], activeReleases = activeReleases)
  })
  
  observeEvent(input$plot_click, {
    clickedRelease <- whichMovieGotClicked(totalsDt = masterData()[["totalsDt"]], x_coord = input$plot_click$x)
    values$selected_table <- rbind(values$selected_table,
                                   data.table(x = input$plot_click$x,
                                              release = clickedRelease)) %>%
      .[, N := .N, by = release] %>% 
      .[N < 2] %>% .[, N := NULL]
  })
  
  # Active Movie Configuration
  output$plot2 <- renderPlot({
    activeReleases <- values$selected_table$release
    plotDirectorDataWalletShare(directorDt = masterData()[["directorDt"]][release %in% activeReleases]) + 
      scale_color_manual(values = masterPalette()[which(names(masterPalette()) %in% activeReleases)])
  })
  
  # Box office performance 
  output$plot3 <- renderPlot({
    activeReleases <- values$selected_table$release
    plotTotalBars(totalsDt = copy(masterData()[["totalsDt"]]) %>%
                    .[!(release %in% activeReleases), ':='(domestic_total = 0)],
                  activeRelease = activeReleases) + 
      scale_fill_manual(values = masterPalette()[which(names(masterPalette()) %in% activeReleases)])
  })
  
  # Tooltip configuration
  output$hover_info <- renderText({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      hoveredRelease <- whichMovieGotClicked(totalsDt = masterData()[["totalsDt"]], x_coord = hover$x)
      # print(activeReleases())
      # print(toolTipForMovie(directorDt = masterData(), releaseName = hoveredRelease))
    } else {
      "Use this bar to get more info and populate below graphs."
    }
  })
}
shinyApp(ui, server)