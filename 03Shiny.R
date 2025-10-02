library("shiny")
library("bslib")
library("tidyverse")
library("stringr")

teamcol <- read_csv("Data/colors.csv")

ui <- fluidPage(
	div(align = "center",
		uiOutput("appTitle"),
		selectInput("team", "Select an MLB Team.", teamcol[[1]])),
	layout_columns(
		imageOutput("plotwins"),
		imageOutput("plotperc")
	)
)

server <- function(input, output, session) {
	teamhex <- reactive({
		col <- teamcol %>%
			filter(team == input$team) %>% 
			pull(col1)
	})
	
	output$appTitle <- renderUI({
		HTML(sprintf("<h2 style='color:%s;'>%s — Win Tracker</h2>",
					 teamhex(), input$team))
	})
	
	output$plotwins <- renderImage({
		list(src = file.path("Plots/Wins", paste0(input$team, ".gif")),
			 width = 400,
			 height = 400)
	}, deleteFile = FALSE)
	
	output$plotperc <- renderImage({
		list(src = file.path("Plots/Perc", paste0(input$team, ".gif")),
			 width = 400,
			 height = 400)
	}, deleteFile = FALSE)
}

shinyApp(ui, server)